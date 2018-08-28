let floatarray_blit src off_src tgt off_tgt len =
  let off = off_tgt - off_src in
  if src == tgt && off > 0 then
    for i = off_src + len - 1 downto off_src do
      Stdcompat.Float.Array.unsafe_set tgt (i + off)
        (Stdcompat.Float.Array.unsafe_get src i)
    done
  else
    for i = off_src to off_src + len - 1 do
      Stdcompat.Float.Array.unsafe_set tgt (i + off)
        (Stdcompat.Float.Array.unsafe_get src i)
    done

include MonoSlice.MakeMut (struct
  type t = Stdcompat.floatarray

  type item = float

  let empty = Stdcompat.Float.Array.create 0

  let init len f =
    let result = Stdcompat.Float.Array.create len in
    for i = 0 to len - 1 do
      Stdcompat.Float.Array.set result i (f i);
    done;
    result

  let make len value =
    init len (fun _ -> value)

  let unsafe_get = Stdcompat.Float.Array.unsafe_get

  type mut = Stdcompat.floatarray

  let create_mut = Stdcompat.Float.Array.create

  let unsafe_set = Stdcompat.Float.Array.unsafe_set

  let unsafe_blit = floatarray_blit

  let unsafe_of_mut x = x
end)

let of_floatarray base =
  let length = Stdcompat.Float.Array.length base in
  if length = 0 then
    empty
  else
    { base; offset = 0; length }

let copy_to_floatarray { base; offset; length } =
  let copy = Stdcompat.Float.Array.create length in
  floatarray_blit base offset copy 0 length;
  copy

let to_floatarray slice =
  let { base; offset; length } = slice in
  if offset = 0 && Stdcompat.Float.Array.length base = length then
    base
  else
    copy_to_floatarray slice

let compare = compare Pervasives.compare

let equal = equal ( = )

let hash = hash Hashtbl.hash
