module Make (X : SliceS.IndexedCollection) = struct
  type t = {
      base : X.t;
      offset : int;
      length : int;
    }

  type item = X.item

  let empty = { base = X.empty; offset = 0; length = 0 }

  let is_empty s =
    s.length = 0

  let make l item =
    if l = 0 then
      empty
    else
      { base = X.make l item; offset = 0; length = l }

  let init l f =
    if l = 0 then
      empty
    else
      { base = X.init l f; offset = 0; length = l }

  let singleton item = make 1 item

  let length slice = slice.length

  let append_not_preserve a b =
    let length = a.length + b.length in
    let base = X.create_mut length in
    X.unsafe_blit a.base a.offset base 0 a.length;
    X.unsafe_blit b.base b.offset base a.length b.length;
    { base = X.unsafe_of_mut base; offset = 0; length }

  let append a b =
    if a.length = 0 then
      b
    else if b.length = 0 then
      a
    else if a.base == b.base && a.offset + a.length = b.offset then
      { a with length = a.length + b.length }
    else
      append_not_preserve a b

  let rec find_first_non_empty_opt list =
    match list with
    | [] -> None
    | { length = 0 } :: tl -> find_first_non_empty_opt tl
    | hd :: tl -> Some (hd, tl)

  let concat_length hd tl =
    List.fold_left (fun accu { length } -> accu + length) hd.length tl

  let concat_aux length hd tl =
    let base_new = X.create_mut length in
    X.unsafe_blit hd.base hd.offset base_new 0 hd.length;
    let rec fill list accu_offset =
      match list with
      | [] -> ()
      | { base; offset; length } :: tl ->
          X.unsafe_blit base offset base_new accu_offset length;
          fill tl (accu_offset + length) in
    fill tl hd.length;
    { base = X.unsafe_of_mut base_new; offset = 0; length }

  let rec merge_successive hd tl =
    match tl with
    | [] -> hd, []
    | { length = 0 } :: tl -> merge_successive hd tl
    | { base; offset; length } :: tl when
        hd.base == base && hd.offset + hd.length = offset ->
          merge_successive { hd with length = hd.length + length } tl
    | _ -> hd, tl

  let concat list =
    match find_first_non_empty_opt list with
    | None -> empty
    | Some (hd, tl) ->
        let hd, tl = merge_successive hd tl in
        let length = concat_length hd tl in
        if length = hd.length then
          hd
        else
          concat_aux length hd tl

  let concat_not_preserve list =
    match find_first_non_empty_opt list with
    | None -> empty
    | Some (hd, tl) -> concat_aux (concat_length hd tl) hd tl

  let unsafe_get { base; offset } index =
    X.unsafe_get base (offset + index)

  let in_bounds { offset; length } index =
    index >= 0 && index < length

  let in_bounds_large { offset; length } index =
    index >= 0 && index <= length

  let raise_index_out_of_bounds () =
    invalid_arg "index out of bounds"

  let get slice index =
    if not (in_bounds slice index) then
      raise_index_out_of_bounds ();
    unsafe_get slice index

  let get_opt slice index =
    if in_bounds slice index then
      Some (unsafe_get slice index)
    else
      None

  let raise_slices_must_have_the_same_length () =
    invalid_arg "Slices must have the same length"

  let in_bounds_range { offset; length } sub_offset sub_length =
    sub_offset >= 0 && sub_length >= 0 && sub_offset + sub_length <= length

  let unsafe_sub { base; offset } new_offset length =
    if length = 0 then
      empty
    else
      { base; offset = offset + new_offset; length }

  let sub slice new_offset new_length =
    if new_offset = 0 && slice.length = new_length then
      slice
    else
      begin
        if not (in_bounds_range slice new_offset new_length) then
          raise_index_out_of_bounds ();
        unsafe_sub slice new_offset new_length
      end

  let sub_opt slice new_offset new_length =
    if new_offset = 0 && slice.length = new_length then
      Some slice
    else if in_bounds_range slice new_offset new_length then
      Some (unsafe_sub slice new_offset new_length)
    else
      None

  let iter f { base; offset; length } =
    for i = offset to offset + length - 1 do
      f (X.unsafe_get base i)
    done

  let iteri f { base; offset; length } =
    for i = 0 to length - 1 do
      f i (X.unsafe_get base (offset + i))
    done

  let unsafe_iter2 f a b =
    let interoffset = b.offset - a.offset in
    for i = a.offset to a.offset + a.length - 1 do
      f (X.unsafe_get a.base i) (X.unsafe_get b.base (i + interoffset))
    done

  let iter2 f a b =
    if a.length <> b.length then
      raise_slices_must_have_the_same_length ();
    unsafe_iter2 f a b

  let try_iter2 f a b =
    if a.length = b.length then
      begin
        unsafe_iter2 f a b;
        true
      end
    else
      false

  let unsafe_iter2i f a b =
    for i = 0 to a.length - 1 do
      f i (X.unsafe_get a.base (a.offset + i))
        (X.unsafe_get b.base (b.offset + i))
    done

  let iter2i f a b =
    if a.length <> b.length then
      raise_slices_must_have_the_same_length ();
    unsafe_iter2i f a b

  let try_iter2i f a b =
    if a.length = b.length then
      begin
        unsafe_iter2i f a b;
        true
      end
    else
      false

  let mapi f slice =
    let length = slice.length in
    let rec check_eq i =
      if i < length then
        begin
          let x = unsafe_get slice i in
          let y = f i x in
          if x == y then
            check_eq (succ i)
          else
            begin
              let result = X.create_mut length in
              X.unsafe_blit slice.base slice.offset result 0 i;
              X.unsafe_set result i y;
              for j = i + 1 to slice.length - 1 do
                X.unsafe_set result j (f j (unsafe_get slice j))
              done;
              { base = X.unsafe_of_mut result; offset = 0; length }
            end
        end
      else
        slice in
    check_eq 0

  let map f slice =
    mapi (fun _ x -> f x) slice

  let map_to_array f slice =
    Array.init slice.length (fun i -> f (unsafe_get slice i))

  let unsafe_map2 f a b =
    init a.length (fun i -> f (unsafe_get a i) (unsafe_get b i))

  let map2 f a b =
    if a.length <> b.length then
      raise_slices_must_have_the_same_length ();
    unsafe_map2 f a b

  let map2_opt f a b =
    if a.length = b.length then
      Some (unsafe_map2 f a b)
    else
      None

  let unsafe_map2i f a b =
    init b.length (fun i -> f i (unsafe_get a i) (unsafe_get b i))

  let map2i f a b =
    if a.length <> b.length then
      raise_slices_must_have_the_same_length ();
    unsafe_map2i f a b

  let map2i_opt f a b =
    if a.length = b.length then
      Some (unsafe_map2i f a b)
    else
      None

  let filteri p s =
    let { base; offset; length } = s in
    let last = offset + length in
    let rec find_segment_from accu len i =
      if i < last then
        if p (i - offset) (X.unsafe_get base i) then
          find_segment_to accu len i (succ i)
        else
          find_segment_from accu len (succ i)
      else
        accu, len
    and find_segment_to accu len from i =
      if i < last then
        if p (i - offset) (X.unsafe_get base i) then
          find_segment_to accu len from (succ i)
        else
          find_segment_from ({ base; offset = from; length = i - from } :: accu)
            (len + i - from) (succ i)
      else
        { base; offset = from; length = i - from } :: accu, len + i - from in
    let segments, len = find_segment_from [] 0 offset in
    match List.rev segments with
    | [] -> empty
    | [segment] ->
        if segment.offset = offset && segment.length = length then
          s
        else
          segment
    | hd :: tl -> concat_aux len hd tl

  let filter p s =
    filteri (fun _ x -> p x) s

  let filter_lefti p s =
    let { base; offset; length } = s in
    let last = offset + length in
    let rec aux i =
      if i < last then
        if p (i - offset) (X.unsafe_get base i) then
          if i = offset then
            s
          else
            { base; offset = i; length = last - i }
        else
          aux (succ i)
      else
        empty in
    aux offset

  let filter_left p s =
    filter_lefti (fun _ x -> p x) s

  let filter_righti p s =
    let { base; offset; length } = s in
    let last = offset + length in
    let rec aux i =
      if i > offset then
        let j = pred i in
        if p (j - offset) (X.unsafe_get base j) then
          if i = last then
            s
          else
            { base; offset; length = i - offset }
        else
          aux j
      else
        empty in
    aux last

  let filter_right p s =
    filter_righti (fun _ x -> p x) s

  let split_lefti_opt_off off p { base; offset; length } =
    let last = offset + length in
    let rec aux i =
      if i < last then
        if p (i - off) (X.unsafe_get base i) then
          let left_length = i - offset in
          let left =
            if left_length = 0 then
              empty
            else
              { base; offset; length = left_length } in
          let right_length = last - i - 1 in
          let right =
            if right_length = 0 then
              empty
            else
              { base; offset = i + 1; length = right_length } in
          Some (left, right)
        else
          aux (succ i)
      else
        None in
    aux offset

  let split_lefti_opt p s =
    split_lefti_opt_off s.offset p s

  let split_left_opt p s =
    split_lefti_opt (fun _ x -> p x) s

  let split_lefti p s =
    match split_lefti_opt p s with
    | None -> raise Not_found
    | Some result -> result

  let split_left p s =
    match split_left_opt p s with
    | None -> raise Not_found
    | Some result -> result

  let split_righti_opt p { base; offset; length } =
    let last = offset + length in
    let rec aux i =
      if i > offset then
        let j = pred i in
        let left_length = j - offset in
        if p left_length (X.unsafe_get base j) then
          let left =
            if left_length = 0 then
              empty
            else
              { base; offset; length = left_length } in
          let right_length = last - i in
          let right =
            if right_length = 0 then
              empty
            else
              { base; offset = i; length = right_length } in
          Some (left, right)
        else
          aux j
      else
        None in
    aux last

  let split_right_opt p s =
    split_righti_opt (fun _ x -> p x) s

  let split_righti p s =
    match split_righti_opt p s with
    | None -> raise Not_found
    | Some result -> result

  let split_right p s =
    match split_right_opt p s with
    | None -> raise Not_found
    | Some result -> result

  let spliti p s =
    let offset = s.offset in
    let rec aux accu s =
      match split_lefti_opt_off offset p s with
      | None -> List.rev (s :: accu)
      | Some (left, right) -> aux (left :: accu) right in
    aux [] s

  let split p s =
    spliti (fun _ x -> p x) s

  let unsafe_split_at index slice =
    let { base; offset; length } = slice in
    if index = 0 then
      (empty, slice)
    else if index = length then
      (slice, empty)
    else
      let right_offset = offset + index in
      ({ base; offset; length = index },
        { base; offset = right_offset; length = length - index })

  let split_at_opt index slice =
    if in_bounds_large slice index then
      Some (unsafe_split_at index slice)
    else
      None

  let split_at index slice =
    if not (in_bounds_large slice index) then
      raise_index_out_of_bounds ();
    unsafe_split_at index slice

  let for_all f { base; offset; length } =
    let offset_end = offset + length in
    let rec loop i =
       i = offset_end || f (X.unsafe_get base i) && loop (succ i) in
    loop offset

  let for_alli f slice =
    let { length } = slice in
    let rec loop i =
       i = length || f i (unsafe_get slice i) && loop (succ i) in
    loop 0

  let unsafe_for_all2 f a b =
    let offset_end = a.offset + a.length in
    let interoffset = b.offset - a.offset in
    let rec loop i =
       i = offset_end ||
       f (X.unsafe_get a.base i)
         (X.unsafe_get b.base (i + interoffset)) &&
       loop (succ i) in
    loop a.offset

  let for_all2 f a b =
    if a.length <> b.length then
      raise_slices_must_have_the_same_length ();
    unsafe_for_all2 f a b

  let for_all2_opt f a b =
    if a.length = b.length then
      Some (unsafe_for_all2 f a b)
    else
      None

  let unsafe_for_all2i f a b =
    let { length } = a in
    let rec loop i =
       i = length ||
       f i (unsafe_get a i) (unsafe_get b i) &&
       loop (succ i) in
    loop 0

  let for_all2i f a b =
    if a.length <> b.length then
      raise_slices_must_have_the_same_length ();
    unsafe_for_all2i f a b

  let for_all2i_opt f a b =
    if a.length = b.length then
      Some (unsafe_for_all2i f a b)
    else
      None

  let exists f { base; offset; length } =
    let offset_end = offset + length in
    let rec loop i =
       i < offset_end && (f (X.unsafe_get base i) || loop (succ i)) in
    loop offset

  let existsi f slice =
    let { length } = slice in
    let rec loop i =
       i < length && (f i (unsafe_get slice i) || loop (succ i)) in
    loop 0

  let mem x slice = exists (( = ) x) slice

  let unsafe_exists2 f a b =
    let offset_end = a.offset + a.length in
    let interoffset = b.offset - a.offset in
    let rec loop i =
       i < offset_end && (
         f (X.unsafe_get a.base i)
           (X.unsafe_get b.base (i + interoffset)) ||
         loop (succ i)) in
    loop a.offset

  let exists2 f a b =
    if a.length <> b.length then
      raise_slices_must_have_the_same_length ();
    unsafe_exists2 f a b

  let exists2_opt f a b =
    if a.length = b.length then
      Some (unsafe_exists2 f a b)
    else
      None

  let rec unsafe_exists2i f a b =
    let { length } = a in
    let rec loop i =
       i < length && (
         f i (unsafe_get a i) (unsafe_get b i) ||
         loop (succ i)) in
    loop 0

  let exists2i f a b =
    if a.length <> b.length then
      raise_slices_must_have_the_same_length ();
    unsafe_exists2i f a b

  let exists2i_opt f a b =
    if a.length = b.length then
      Some (unsafe_exists2i f a b)
    else
      None

  let fold_left f init { base; offset; length } =
    let offset_end = offset + length in
    let rec loop i accu =
      if i = offset_end then
        accu
      else
        loop (succ i) (f accu (X.unsafe_get base i)) in
    loop offset init

  let fold_lefti f init { base; offset; length } =
    let rec loop i accu =
      if i = length then
        accu
      else
        loop (succ i) (f i accu (X.unsafe_get base (offset + i))) in
    loop 0 init

  let fold_right f { base; offset; length } init =
    let rec loop i accu =
      if i = offset then
        accu
      else
        let j = pred i in
        loop j (f (X.unsafe_get base j) accu) in
    loop (offset + length) init

  let fold_righti f { base; offset; length } init  =
    let rec loop i accu =
      if i = 0 then
        accu
      else
        let j = pred i in
        loop j (f j (X.unsafe_get base (offset + j)) accu) in
    loop length init

  let compare cmp a b =
    match a.length - b.length with
    | 0 ->
        let offset_end = a.offset + a.length in
        let interoffset = b.offset - a.offset in
        let rec loop i =
          if i = offset_end then
            0
          else
            match
              cmp (X.unsafe_get a.base i)
                (X.unsafe_get b.base (i + interoffset))
            with
            | 0 -> loop (succ i)
            | c -> c in
        loop a.offset
    | c -> c

  let equal eq a b =
    if a.length = b.length then
      for_all2 eq a b
    else
      false

  let hash h a =
    Hashtbl.hash (map_to_array h a)

  let to_seq { base; offset; length } =
    let offset_end = offset + length in
    let rec aux i () =
      if i < offset_end then
        let x = X.unsafe_get base i in
        Stdcompat.Seq.Cons (x, aux (succ i))
      else
        Stdcompat.Seq.Nil in
    aux offset

  let to_seqi { base; offset; length } =
    let rec aux i () =
      if i < length then
        let x = X.unsafe_get base (i + offset) in
        Stdcompat.Seq.Cons ((i, x), aux (succ i))
      else
        Stdcompat.Seq.Nil in
    aux 0

  let of_rev_list rev_list length =
    match rev_list with
    | [] -> empty
    | _ ->
        let base = X.create_mut length in
        let rec fill i rev_list =
          match rev_list with
          | [] -> ()
          | hd :: tl ->
              X.unsafe_set base i hd;
              fill (pred i) tl in
        fill (length - 1) rev_list;
        { base = X.unsafe_of_mut base; offset = 0; length }

  let of_seq s =
    let rev_list, length =
      Stdcompat.Seq.fold_left (fun (accu, len) x -> (x :: accu, succ len))
        ([], 0) s in
    of_rev_list rev_list length

  let of_list list =
    match list with
    | [] -> empty
    | _ ->
        let length = List.length list in
        let base = X.create_mut length in
        let rec fill i list =
          match list with
          | [] -> ()
          | hd :: tl ->
              X.unsafe_set base i hd;
              fill (succ i) tl in
        fill 0 list;
        { base = X.unsafe_of_mut base; offset = 0; length }

  let to_list slice =
    fold_right (fun item accu -> item :: accu) slice []

  let of_array a =
    init (Array.length a) (Array.unsafe_get a)

  let to_array s =
    Array.init (length s) (unsafe_get s)

  let of_slice slice =
    init (Slice.length slice) (Slice.unsafe_get slice)

  let to_slice slice =
    Slice.init slice.length (unsafe_get slice)
end

module MakeMut (X : SliceS.MutIndexedCollection) = struct
  include Make (struct
    include X

    type mut = t
  end)

  let create length =
    { base = X.create_mut length; offset = 0; length }

  let unsafe_set { base; offset } index value =
    X.unsafe_set base (offset + index) value

  let set slice index value =
    if not (in_bounds slice index) then
      raise_index_out_of_bounds ();
    unsafe_set slice index value

  let try_set slice index value =
    if in_bounds slice index then
      begin
        unsafe_set slice index value;
        true
      end
    else
      false

  let copy { base; offset; length } =
    let base_mut = X.create_mut length in
    X.unsafe_blit base offset base_mut 0 length;
    { base = X.unsafe_of_mut base_mut; offset = 0; length }

  let unsafe_recopy ~src ~tgt =
    X.unsafe_blit src.base src.offset tgt.base tgt.offset src.length

  let recopy ~src ~tgt =
    if src.length <> tgt.length then
      raise_slices_must_have_the_same_length ();
    unsafe_recopy ~src ~tgt

  let try_recopy ~src ~tgt =
    if src.length = tgt.length then
      begin
        unsafe_recopy ~src ~tgt;
        true
      end
    else
      false

  let append_preserve = append

  let append = append_not_preserve

  let concat_preserve = concat

  let concat = concat_not_preserve

  let map_preserve = map

  let mapi_preserve = mapi

  let filter_preserve = filter

  let filteri_preserve = filteri

  let map f slice =
    init slice.length (fun i -> f (unsafe_get slice i))

  let mapi f slice =
    init slice.length (fun i -> f i (unsafe_get slice i))
end

module CharSliceExt (S : SliceS.S with type item := char) = struct
  let split_on_char c s =
    S.split (( = ) c) s

  let is_not_space = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> false
    | _ -> true

  let trim s =
    S.filter_right is_not_space (S.filter_left is_not_space s)
end
