module type S = sig
  type t
  (** The type of a monomorphic slice.
      {!module:CharSlice}[.t] is the type of immutable subranges of [string],
      and
      {!module:MutCharSlice}[.t] is the type of mutable subranges of [bytes].
      {!module:FloatSlice}[.t] is the type of immutable subranges of
      [floatarray],
      and {!module:MutFloatSlice}[.t] is the type of mutable subranges of
      [floatarray]. *)

  type item
  (** The type of elements of a slice. Elements of {!module:CharSlice}[.t] and
      {!module:MutCharSlice}[.t] are type [char].
      Elements of {!module:FloatSlice}[.t] and
      {!module:MutFloatSlice}[.t] are type [float].*)

  val empty : t
  (** The empty slice: this is the unique 0-length slice of this type, i.e.
a slice [s] is 0-length if and only if [s] is physically equal to [empty].
    {[
assert (CharSlice.length CharSlice.empty = 0);
assert (FloatSlice.length FloatSlice.empty = 0);
assert (CharSlice.of_string "" == CharSlice.empty);
assert (FloatSlice.of_list [] == FloatSlice.empty);
    ]} *)

  val is_empty : t -> bool
  (** [is_empty s] returns [true] if [s] is the empty slice, i.e. [s] is
0-length or, equivalently, [s] is physically equal to [empty].
    {[
assert (CharSlice.is_empty CharSlice.empty);
assert (FloatSlice.is_empty FloatSlice.empty);
assert (CharSlice.is_empty (CharSlice.of_string ""));
assert (not (CharSlice.is_empty (CharSlice.of_string "A")));
assert (FloatSlice.is_empty (FloatSlice.of_list []));
assert (not (FloatSlice.is_empty (FloatSlice.of_list [0.0])));
    ]} *)

  val make : int -> item -> t
  (** [make n x] returns a fresh slice of length [n], initialized with [x].
    {[
assert (CharSlice.make 0 'a' == CharSlice.empty);
assert (CharSlice.length (CharSlice.make 3 'a') = 3);
assert (CharSlice.to_string (CharSlice.make 4 'b') = "bbbb");
assert (FloatSlice.make 0 0. == FloatSlice.empty);
assert (FloatSlice.length (FloatSlice.make 3 0.) = 3);
assert (FloatSlice.to_array (FloatSlice.make 4 1.) = [| 1.; 1.; 1.; 1. |]);
    ]} *)

  val init : int -> (int -> item) -> t
  (** [init n f] returns a fresh slice of length [n], with element index [i]
      initialized to the result of [f i].
    {[
assert (CharSlice.init 0 char_of_int == CharSlice.empty);
assert (CharSlice.length (CharSlice.init 3 char_of_int) = 3);
let ascii = CharSlice.init 256 char_of_int in
assert (
  CharSlice.to_string (CharSlice.sub ascii (int_of_char 'a') 3) = "abc");
assert (FloatSlice.init 0 float_of_int == FloatSlice.empty);
assert (FloatSlice.length (FloatSlice.init 3 float_of_int) = 3);
assert (
  FloatSlice.to_array
    (FloatSlice.sub (FloatSlice.init 10 float_of_int) 5 3)
      = [| 5.; 6.; 7. |]);
    ]} *)

  val length : t -> int
  (** [length s] returns the length (number of elements) of slice [s].
    {[
assert (
  CharSlice.length (CharSlice.sub (CharSlice.of_string "abcdef") 2 3) = 3);
assert (
  let slice6 = FloatSlice.init 6 float_of_int in
  FloatSlice.length (FloatSlice.sub slice6 2 3) = 3);
    ]} *)

  val get : t -> int -> item
  (** [get s i] returns the element index [i] of slice [s].
      The first element has index 0.
      Raise [Invalid_argument "index out of bounds"]
      if [i] is outside the range [0] to [Slice.length s - 1].
    {[
assert (CharSlice.get (CharSlice.of_string "abcd") 2 = 'c');
assert (
  try
    ignore (CharSlice.get (CharSlice.of_string "abcd") 4);
    false
  with Invalid_argument _ ->
    true
);
assert (
  try
    ignore (CharSlice.get (CharSlice.of_string "abcd") (-1));
    false
  with Invalid_argument _ ->
    true
);
assert (FloatSlice.get (FloatSlice.of_list [0.0; 1.0; 2.0]) 2 = 2.0);
assert (
  try
    ignore (FloatSlice.get (FloatSlice.init 3 float_of_int) 3);
    false
  with Invalid_argument _ ->
    true
);
assert (
  try
    ignore (FloatSlice.get (FloatSlice.init 3 float_of_int) (-1));
    false
  with Invalid_argument _ ->
    true
);
    ]} *)

  val get_opt : t -> int -> item option
  (** [get_opt s i] returns the element index [i] of slice [s].
      The first element has index 0.
      Return [None] if [i] is outside the range [0] to
      [Slice.length s - 1].
    {[
assert (CharSlice.get_opt (CharSlice.of_string "abcd") 2 = Some 'c');
assert (CharSlice.get_opt ascii 256 = None);
assert (CharSlice.get_opt ascii (-1) = None);
assert (FloatSlice.get_opt (FloatSlice.of_list [0.0; 1.0; 2.0]) 2 = Some 2.0);
assert (FloatSlice.get_opt (FloatSlice.init 3 float_of_int) 3 = None);
assert (FloatSlice.get_opt (FloatSlice.init 3 float_of_int) (-1) = None);
    ]} *)

  val unsafe_get : t -> int -> item
  (** [unsafe_get s i] returns the element index [i] of slice [s].
      The first element has index 0.
      Indices are unchecked and the behavior is unspecified if
      [i] is outside the range [0] to [Slice.length s - 1]. *)

  val sub : t -> int -> int -> t
  (** [sub s off len] returns a subslice of length [len], containing
      the elements index [off] to [off + len - 1] of slice [s].
      The elements are not copied. In particular, if the slice is
      mutable, then modifying the subslice modify
      the original slice too.
      Raise [Invalid_argument "index out of bounds"]
      if [off] and [len] do not designate a valid subslice of [s];
      that is, if [off < 0], or [len < 0], or [off + len > Slice.length s].
    {[
assert (CharSlice.sub ascii 0 256 == ascii);
assert (CharSlice.sub ascii 0 0 == CharSlice.empty);
assert (
  CharSlice.to_string
    (CharSlice.sub (CharSlice.sub ascii (int_of_char 'a') 26) 4 4) =
  "efgh");
assert (
  try
    ignore (CharSlice.sub ascii 1 256);
    false
  with Invalid_argument _ ->
    true
);
assert (
  try
    ignore (CharSlice.sub ascii (-1) 255);
    false
  with Invalid_argument _ ->
    true
);
assert (
  try
    ignore (CharSlice.sub ascii 3 (-2));
    false
  with Invalid_argument _ ->
    true
);
assert (
  try
    ignore (CharSlice.sub (CharSlice.sub ascii 1 5) 3 3);
    false
  with Invalid_argument _ ->
    true
);
    ]} *)

  val sub_opt : t -> int -> int -> t option
  (** [sub_opt s off len] returns a subslice of length [len], containing
      the elements index [off] to [off + len - 1] of slice [s].
      The elements are not copied. In particular, if the slice is
      mutable, then modifying the subslice modify
      the original slice too.
      Return [None]
      if [off] and [len] do not designate a valid subslice of [s];
      that is, if [off < 0], or [len < 0], or [off + len > Slice.length s].
    {[
let option_exists p o =
  match o with
  | None -> false
  | Some x -> p x in
assert (option_exists (( == ) ascii) (CharSlice.sub_opt ascii 0 256));
assert (option_exists (( == ) CharSlice.empty) (CharSlice.sub_opt ascii 0 0));
assert (CharSlice.sub_opt ascii 1 256 = None);
assert (CharSlice.sub_opt ascii (-1) 255 = None);
    ]} *)

  val unsafe_sub : t -> int -> int -> t
  (** [unsafe_sub s off len] returns a subslice of length [len], containing
      the elements index [off] to [off + len - 1] of slice [s].
      The elements are not copied. In particular, if the slice is
      mutable, then modifying the subslice modify
      the original slice too.
      Indices are unchecked and the behavior is unspecified if
      [off] and [len] do not designate a valid subslice of [s];
      that is, if [off < 0], or [len < 0], or [off + len > Slice.length s]. *)

  val append : t -> t -> t
  (** [append a b] returns a slice containing the concatenation of the slices
      [a] and [b].
      If slices are immutable, the elements are copied if and only if the
      slices [a] and [b] are not contiguous and neither [a] nor [b] are empty.
      In particular, if [a] or [b] is empty, then the other slice is returned
      unchanged.
      If slices are mutable, the returned slice is fresh; that is, the elements
      are always copied: use {!val:MutS.append_preserve} to get the same
      behaviour than in the immutable case.
    {[
assert (CharSlice.append (CharSlice.sub ascii 3 0) ascii == ascii);
assert (CharSlice.append ascii (CharSlice.sub ascii 2 0) == ascii);
assert (
  CharSlice.length (CharSlice.append (CharSlice.sub ascii 1 5)
    (CharSlice.sub ascii 0 3)) = 8);
assert (
  CharSlice.to_string (CharSlice.append (CharSlice.sub ascii 0 100)
    (CharSlice.sub ascii 100 156)) == CharSlice.to_string ascii);
assert (
  let mut_ascii = MutCharSlice.init 256 char_of_int in
  let mut_ascii' = MutCharSlice.append (MutCharSlice.sub mut_ascii 0 4)
    (MutCharSlice.sub mut_ascii 4 2) in
  MutCharSlice.set mut_ascii' 2 'x';
  MutCharSlice.get mut_ascii 2 = '\002');
    ]} *)

  val concat : t list -> t
  (** [concat l] returns a slice containing the concatenation of the slices
      listed in [l].
      If slices are immutable, the elements are copied if and only if the
      non-empty slices in [l] are not contiguous. In particular, if there is
      only one non-empty slice in [l], then this slice is returned unchanged.
      If slices are mutable, the returned slice is fresh; that is, the elements
      are always copied: use {!val:MutGenS.concat_preserve} to get the same
      behaviour than in the immutable case.
    {[
assert (
  CharSlice.to_string (CharSlice.concat
    ([CharSlice.sub ascii (int_of_char 'u') 3;
      CharSlice.sub ascii (int_of_char 'L') 2;
      CharSlice.sub ascii (int_of_char '3') 4]))
  = "uvwLM3456");
assert (
  CharSlice.concat ([CharSlice.sub ascii 2 0; ascii; CharSlice.sub ascii 4 0])
  == ascii);
assert (
  CharSlice.to_string (CharSlice.concat [CharSlice.sub ascii 0 32;
    CharSlice.sub ascii 32 224]) == CharSlice.to_string ascii);
assert (
  let mut_ascii = MutCharSlice.init 256 char_of_int in
  let mut_ascii' = MutCharSlice.concat [MutCharSlice.sub mut_ascii 0 4;
    MutCharSlice.sub mut_ascii 4 2] in
  MutCharSlice.set mut_ascii' 2 'x';
  MutCharSlice.get mut_ascii 2 = '\002');
    ]} *)

  val singleton : item -> t
  (** [singleton x] returns a slice containing exactly one element, [x].
    {[
assert (CharSlice.length (CharSlice.singleton 'x') = 1);
assert (CharSlice.to_string (CharSlice.singleton 'x') = "x");
    ]} *)

  val iter : (item -> unit) -> t -> unit
  (** [iter f s] applies function [f] in turn to all elements of [s].
    {[
assert (
  let q = Queue.create () in
  CharSlice.iter (fun i -> Queue.add i q)
    (CharSlice.sub ascii (int_of_char 'a') 4);
  Stdcompat.String.of_seq (Stdcompat.Queue.to_seq q) = "abcd");
    ]} *)

  val iteri : (int -> item -> unit) -> t -> unit
  (** [iteri f s] applies function [f] in turn to all elements of [s],
with the index of the element as first argument, and the element itself
as second argument.
    {[
assert (
  let q = Queue.create () in
  CharSlice.iteri (fun i j -> Queue.add (i, j) q)
    (CharSlice.sub ascii (int_of_char 'a') 4);
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 'a'; 1, 'b'; 2, 'c'; 3, 'd']);
    ]} *)

  val iter2 : (item -> item -> unit) -> t -> t -> unit
  (** [iter2 f a b] checks that [a] and [b] are the same length, and
applies function [f] to all elements of [a] and [b].
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
Raise [Invalid_argument] if the slices are not the same length: in this
case, the function [f] is never called.
    {[
assert (
  let q = Queue.create () in
  CharSlice.iter2 (fun i j -> Queue.add (i, j) q)
    (CharSlice.sub ascii (int_of_char 'a') 4)
    (CharSlice.sub ascii (int_of_char 'A') 4);
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    ['a', 'A'; 'b', 'B'; 'c', 'C'; 'd', 'D']);
assert (
  try
    CharSlice.iter2 (fun i j -> ()) (CharSlice.sub ascii 1 4)
      (CharSlice.sub ascii 1 3);
    false
  with Invalid_argument _ ->
    true);
    ]} *)

  val try_iter2 : (item -> item -> unit) -> t -> t -> bool
  (** [try_iter2 f a b] checks that [a] and [b] are the same length, and
      applies function [f] to all elements of [a] and [b], and returns [true].
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      Return [false] if the slices are not the same length: in this
      case, the function [f] is never called.
    {[
assert (
  let q = Queue.create () in
  CharSlice.try_iter2 (fun i j -> Queue.add (i, j) q)
    (CharSlice.sub ascii (int_of_char 'a') 4)
    (CharSlice.sub ascii (int_of_char 'A') 4) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    ['a', 'A'; 'b', 'B'; 'c', 'C'; 'd', 'D']);
assert (
  not (CharSlice.try_iter2 (fun i j -> ())
    (CharSlice.sub ascii 1 2) (CharSlice.sub ascii 2 1)));
    ]} *)

  val unsafe_iter2 : (item -> item -> unit) -> t -> t -> unit
  (** [unsafe_iter2 f a b] applies function [f] to all elements of [a] and [b].
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      The behaviour is unspecified if the slices are not the same length. *)

  val iter2i : (int -> item -> item -> unit) -> t -> t -> unit
  (** [iter2i f a b] checks that [a] and [b] are the same length, and
      applies function [f] to all elements of [a] and [b].
      The first argument passed to [f] is the index, the second argument
      is the element of [a] and the third argument is the element of [b].
      Raise [Invalid_argument] if the slices are not the same length: in this
      case, the function [f] is never called.
    {[
assert (
  let q = Queue.create () in
  CharSlice.iter2i (fun i j k -> Queue.add (i, j, k) q)
    (CharSlice.sub ascii (int_of_char 'a') 3)
    (CharSlice.sub ascii (int_of_char 'A') 3);
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 'a', 'A'; 1, 'b', 'B'; 2, 'c', 'C']);
assert (
  try
    CharSlice.iter2i (fun i j k -> ())
      (CharSlice.sub ascii 0 3) (CharSlice.sub ascii 1 2);
    false
  with Invalid_argument _ ->
    true);
    ]} *)

  val try_iter2i : (int -> item -> item -> unit) -> t -> t -> bool
  (** [try_iter2i f a b] checks that [a] and [b] are the same length, and
      applies function [f] to all elements of [a] and [b], and returns [true].
      The first argument passed to [f] is the index, the second argument
      is the element of [a] and the third argument is the element of [b].
      Return [false] if the slices are not the same length: in this
      case, the function [f] is never called.
    {[
assert (
  let q = Queue.create () in
  CharSlice.try_iter2i (fun i j k -> Queue.add (i, j, k) q)
    (CharSlice.sub ascii (int_of_char 'a') 3)
    (CharSlice.sub ascii (int_of_char 'A') 3) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 'a', 'A'; 1, 'b', 'B'; 2, 'c', 'C']);
assert (
  not (
    CharSlice.try_iter2i (fun i j k -> ())
      (CharSlice.sub ascii 0 3) (CharSlice.sub ascii 1 2)));
    ]} *)

  val unsafe_iter2i : (int -> item -> item -> unit) -> t -> t -> unit
  (** [unsafe_iter2i f a b] applies function [f] to all elements of [a] and
      [b], assuming that [a] and [b] are the same length.
      The first argument passed to [f] is the index, the second argument
      is the element of [a] and the third argument is the element of [b].
      The behaviour is unspecified if the slices are not the same length. *)

  val map : (item -> item) -> t -> t
  (** [map f s] applies function [f] to all the elements of [s], and
      buids a slice with the results returned by [f].
      If slices are immutable, the elements are copied if and only if there
      exists an element [x] for which [f x] differs from [x]: if [f] preserves
      all the elements of [s], then [map f s == s].
      If slices are mutable, the elements are always copied, and [map f s == s]
      if and only if [s] is empty.
      Use {!val:MutS.map_preserve} to get the same
      behaviour than in the immutable case.
    {[
assert (
  FloatSlice.to_list (FloatSlice.map (fun x -> x *. 2.)
      (FloatSlice.sub (FloatSlice.init 6 float_of_int) 2 4))
    = [4.; 6.; 8.; 10.]);
assert (CharSlice.is_empty (CharSlice.map (fun x -> '0') CharSlice.empty));
assert (CharSlice.map (fun x -> x) ascii == ascii);
    ]} *)

  val mapi : (int -> item -> item) -> t -> t
  (** [mapi f s] applies function [f] to all the elements of [s], and
      buids a slice with the results returned by [f].
      The first argument passed to [f] is the index, and the second
      argument is the element itself.
      If slices are immutable, the elements are copied if and only if there
      exists an element [x] at index i for which [f i x] differs from [x]:
      if [f] preserves all the elements of [s], then [mapi f s == s].
      If slices are mutable, the elements are always copied, and [mapi f s == s]
      if and only if [s] is empty.
      Use {!val:MutS.mapi_preserve} to get the same
      behaviour than in the immutable case.
    {[
assert (
  FloatSlice.to_list (FloatSlice.mapi (fun i j -> float_of_int i +. j)
      (FloatSlice.sub (FloatSlice.init 6 float_of_int) 1 5))
    = [1.; 3.; 5.; 7.; 9.]);
assert (CharSlice.mapi (fun _ x -> x) ascii == ascii);
  ]} *)

  val map2 : (item -> item -> item) -> t -> t -> t
  (** [map2 f a b] checks that [a] and [b] are the same length,
      and applies function [f] to all the elements of [a] and [b],
      and buids a slice with the results returned by [f].
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      Raise [Invalid_argument] if the slices are not the same length: in this
      case, the function [f] is never called.
    {[
assert (
  FloatSlice.to_list (FloatSlice.map2 (fun i j -> i +. j)
    (FloatSlice.sub (FloatSlice.init 6 float_of_int) 2 4)
    (FloatSlice.sub (FloatSlice.init 6 float_of_int) 1 4))
    = [3.; 5.; 7.; 9.]);
assert (
  try
    ignore (CharSlice.map2 (fun i j -> 'x')
      (CharSlice.sub ascii 1 4) (CharSlice.sub ascii 1 3));
    false
  with Invalid_argument _ ->
    true);
  ]} *)

  val map2_opt : (item -> item -> item) -> t -> t -> t option
  (** [map2_opt f a b] checks that [a] and [b] are the same length,
      and applies function [f] to all the elements of [a] and [b],
      and buids a slice with the results returned by [f].
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      Return [None] if the slices are not the same length: in this
      case, the function [f] is never called.
    {[
assert (
  match FloatSlice.map2_opt (fun i j -> i +. j)
    (FloatSlice.sub (FloatSlice.init 6 float_of_int) 2 4)
    (FloatSlice.sub (FloatSlice.init 6 float_of_int) 1 4) with
  | None -> false
  | Some slice -> FloatSlice.to_list slice = [3.; 5.; 7.; 9.]);
assert (
  CharSlice.map2_opt (fun i j -> i)
    (CharSlice.sub ascii 1 4) (CharSlice.sub ascii 1 3) = None);
    ]} *)

  val unsafe_map2 : (item -> item -> item) -> t -> t -> t
  (** [unsafe_map2 f a b] applies function [f] to all the elements of [a] and
      [b], assuming that [a] and [b] are the same length,
      and buids a slice with the results returned by [f].
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      The behaviour is unspecified if the slices are not the same length. *)

  val map2i : (int -> item -> item -> item) -> t -> t -> t
  (** [map2i f a b] checks that [a] and [b] are the same length,
      and applies function [f] to all the elements of [a] and [b],
      and buids a slice with the results returned by [f].
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      Raise [Invalid_argument] if the slices are not the same length: in this
      case, the function [f] is never called.
    {[
assert (
  FloatSlice.to_list (
    FloatSlice.map2i (fun i j k -> float_of_int i +. j +. k)
      (FloatSlice.sub (FloatSlice.init 6 float_of_int) 2 4)
      (FloatSlice.sub (FloatSlice.init 6 float_of_int) 1 4))
    = [3.; 6.; 9.; 12.]);
assert (
  try
    ignore (CharSlice.map2i (fun i j k -> j)
      (CharSlice.sub ascii 0 3) (CharSlice.sub ascii 1 2));
    false
  with Invalid_argument _ ->
    true);
    ]} *)

  val map2i_opt : (int -> item -> item -> item) -> t -> t -> t option
  (** [map2i_opt f a b] checks that [a] and [b] are the same length,
      and applies function [f] to all the elements of [a] and [b],
      and buids a slice with the results returned by [f].
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      Return [None] if the slices are not the same length: in this
      case, the function [f] is never called.
    {[
assert (
  match FloatSlice.map2i_opt (fun i j k -> float_of_int i +. j +. k)
    (FloatSlice.sub (FloatSlice.init 6 float_of_int) 2 4)
    (FloatSlice.sub (FloatSlice.init 6 float_of_int) 1 4) with
  | None -> false
  | Some slice -> FloatSlice.to_list slice = [3.; 6.; 9.; 12.]);
assert (
  CharSlice.map2i_opt (fun i j k -> j)
    (CharSlice.sub ascii 0 3) (CharSlice.sub ascii 1 2) = None);
    ]} *)

  val unsafe_map2i : (int -> item -> item -> item) -> t -> t -> t
  (** [unsafe_map2i f a b] applies function [f] to all the elements of [a] and
      [b], assuming that [a] and [b] are the same length,
      and buids a slice with the results returned by [f].
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      The behaviour is unspecified if the slices are not the same length. *)

  val filter : (item -> bool) -> t -> t
  (** [filter p s] applies predicate [p] to all the elements of [a] and
      returns a slice that contains all the elements that satisfies [p].
      If [s] is immutable and
      if all the elements of [s] satisfies [p], then [s] is returned itself,
      that is [filter p s == s].
      If [s] is immutable and
      if all the elements of [s] that satisfies [p] are contiguous, then
      [filter p s] is a subslice of [s] and the elements of [s] are not
      copied.
      Otherwise, the elements of [s] are copied and a fresh slice is
      returned.
      In particular, if [s] is mutable, the elements are always copied, and
      [filter s == s] if and only if [s] is empty.
      Use {!val:MutS.filter_preserve} to get the same
      behaviour than in the immutable case.
    {[
let alphabet = CharSlice.filter (fun c -> c >= 'a' && c <= 'z') ascii in
assert (CharSlice.filter (fun _ -> true) alphabet == alphabet);
assert (CharSlice.to_string (CharSlice.concat [
  CharSlice.filter (fun c -> c < 'a') ascii;
  CharSlice.filter (fun c -> c < 'm') alphabet;
  CharSlice.filter (fun c -> c >= 'm') alphabet;
  CharSlice.filter (fun c -> c > 'z') ascii; ])
  == CharSlice.to_string ascii);
let tones_a2_to_a6 =
  FloatSlice.init 48 (fun i -> 440. *. 2. ** (float_of_int (i - 24) /. 12.)) in
let tones_a3_to_a5 = FloatSlice.sub tones_a2_to_a6 12 24 in
assert (FloatSlice.to_list
  (FloatSlice.filter (fun f -> fst (modf f) < epsilon_float)
    tones_a3_to_a5) = [220.; 440.]);
    ]} *)

  val filteri : (int -> item -> bool) -> t -> t
  (** [filteri p s] applies predicate [p] to all the elements of [a] and
      returns a slice that contains all the elements that satisfies [p].
      The first argument passed to [p] is the index, the second argument
      is the element of [s] itself.
      If [s] is immutable and
      if all the elements of [s] satisfies [p], then [s] is returned itself,
      that is [filter p s == s].
      If [s] is immutable and
      if all the elements of [s] that satisfies [p] are contiguous, then
      [filter p s] is a subslice of [s] and the elements of [s] are not
      copied.
      Otherwise, the elements of [s] are copied and a fresh slice is
      returned.
      In particular, if [s] is mutable, the elements are always copied, and
      [filter s == s] if and only if [s] is empty.
      Use {!val:MutS.filteri_preserve} to get the same
      behaviour than in the immutable case.
    {[
assert (
  let q = Queue.create () in
  CharSlice.length
    (CharSlice.filteri (fun i c -> Queue.add i q; c <= 'd') alphabet) = 4
  && List.of_seq (Queue.to_seq q) = List.init 26 (fun i -> i));
    ]} *)

  val filter_left : (item -> bool) -> t -> t
  (** [filter_left p s] returns the longest suffix of [s] beginning with
      an element that satisfies [p].
      If the first element of [s] satisfies [p], then [s] is returned itself,
      that is [filter_left p s == s].
      [filter_left p s] is always a subslice of [s] and the elements of [s] are
      never copied.
    {[
assert (CharSlice.filter_left (fun c -> c = 'a') alphabet == alphabet);
assert (FloatSlice.unsafe_to_floatarray (FloatSlice.concat [
  FloatSlice.sub tones_a2_to_a6 0 24;
  FloatSlice.filter_left (fun f -> f >= 440.) tones_a3_to_a5;
  FloatSlice.sub tones_a2_to_a6 36 12; ])
  == FloatSlice.unsafe_to_floatarray tones_a2_to_a6);
    ]} *)

  val filter_lefti : (int -> item -> bool) -> t -> t
  (** [filter_lefti p s] returns the longest suffix of [s] beginning with
      an element that satisfies [p].
      The first argument passed to [p] is the index, the second argument
      is the element of [s] itself.
      If the first element of [s] satisfies [p], then [s] is returned itself,
      that is [filter_lefti p s == s].
      [filter_lefti p s] is always a subslice of [s] and the elements of [s] are
      never copied.
    {[
assert (
  let q = Queue.create () in
  CharSlice.length
    (CharSlice.filter_lefti (fun i c -> Queue.add i q; c = 'd') alphabet)
      = 23
  && List.of_seq (Queue.to_seq q) = [0; 1; 2; 3]);
    ]} *)

  val filter_right : (item -> bool) -> t -> t
  (** [filter_right p s] returns the longest prefix of [s] ending with
      an element that satisfies [p].
      If the last element of [s] satisfies [p], then [s] is returned itself,
      that is [filter_right p s == s].
      [filter_right p s] is always a subslice of [s] and the elements of [s] are
      never copied.
    {[
assert (CharSlice.filter_right (fun c -> c = 'z') alphabet == alphabet);
assert (FloatSlice.unsafe_to_floatarray (FloatSlice.concat [
  FloatSlice.sub tones_a2_to_a6 0 12;
  FloatSlice.filter_right (fun f -> f <= 440.) tones_a3_to_a5;
  FloatSlice.sub tones_a2_to_a6 25 23; ])
  == FloatSlice.unsafe_to_floatarray tones_a2_to_a6);
    ]} *)

  val filter_righti : (int -> item -> bool) -> t -> t
  (** [filter_righti p s] returns the longest prefix of [s] ending with
      an element that satisfies [p].
      The first argument passed to [p] is the index, the second argument
      is the element of [s] itself.
      If the last element of [s] satisfies [p], then [s] is returned itself,
      that is [filter_righti p s == s].
      [filter_righti p s] is always a subslice of [s] and the elements of [s]
      are never copied.
    {[
assert (
  let q = Queue.create () in
  CharSlice.length
    (CharSlice.filter_righti (fun i c -> Queue.add i q; c = 'm') alphabet)
      = 13
  && List.of_seq (Queue.to_seq q) =
      [25; 24; 23; 22; 21; 20; 19; 18; 17; 16; 15; 14; 13; 12]);
    ]} *)

  val split : (item -> bool) -> t -> t list
  (** [split sep s] returns the list of all (possibly empty) subslices of [s]
      that are delimited by elements satisfying the predicate [sep].
      The function returns as many subslices as there are elements satisfying
      [sep] in [s] plus one: in particular, the list is not empty.
      No subslice in the result contains an element satisfying the predicate
      [sep].
    {[
assert (
  match CharSlice.split (fun _ -> false) alphabet with
  | [slice] -> slice == alphabet
  | _ -> false);
assert (
  CharSlice.split (fun _ -> true) alphabet =
    Array.to_list (Array.make 27 CharSlice.empty));
assert (
  match
    FloatSlice.split (fun f -> fst (modf f) < epsilon_float) tones_a3_to_a5 with
  | [empty_slice; tones_a3_to_a4; tones_b4_to_a5] ->
    empty_slice == FloatSlice.empty &&
    FloatSlice.unsafe_to_floatarray (
      FloatSlice.concat [FloatSlice.sub tones_a2_to_a6 0 13; tones_a3_to_a4;
        FloatSlice.sub tones_a2_to_a6 24 1; tones_b4_to_a5;
        FloatSlice.sub tones_a2_to_a6 36 12])
      == FloatSlice.unsafe_to_floatarray tones_a2_to_a6
  | _ -> false);
    ]} *)

  val spliti : (int -> item -> bool) -> t -> t list
  (** [spliti sep s] returns the list of all (possibly empty) subslices of [s]
      that are delimited by elements satisfying the predicate [sep].
      The first argument passed to [p] is the index, the second argument
      is the element of [s] itself.
      The function returns as many subslices as there are elements satisfying
      [sep] in [s] plus one: in particular, the list is not empty.
      No subslice in the result contains an element satisfying the predicate
      [sep].
    {[
assert (
  let q = Queue.create () in
  List.length
    (FloatSlice.spliti (fun i f -> Queue.add i q; fst (modf f) < epsilon_float)
      tones_a3_to_a5) = 3
  && List.of_seq (Queue.to_seq q) = List.init 24 (fun i -> i));
    ]} *)

  val split_left : (item -> bool) -> t -> t * t
  (** [split_left sep s] finds the first element [e] of [s] satisfying [sep] and
      returns the pair [(l, r)] where [l] is the prefix of [s] preceding [e] and
      [r] is the suffix of [s] succeeding [e]. [e] is not included, neither in
      [l] nor in [r], and there is no element satisfying [sep] in [l].
      The result [(l, r)] satisfies the following invariant:
      [s = concat [l; sub s (length l) 1; r]].
      Raise [Not_found] if there is no element in [s] satisfying [sep].
    {[
assert (
  let l, r = FloatSlice.split_left (fun f -> f >= 440.)
    tones_a3_to_a5 in
  FloatSlice.length l = 12 &&
  FloatSlice.unsafe_to_floatarray (
    FloatSlice.concat [FloatSlice.sub tones_a2_to_a6 0 12; l;
      FloatSlice.sub tones_a2_to_a6 24 1; r;
      FloatSlice.sub tones_a2_to_a6 36 12])
    == FloatSlice.unsafe_to_floatarray tones_a2_to_a6);
assert (
  try
    ignore (CharSlice.split_left (fun c -> c = '!') alphabet);
    false
  with Not_found ->
    true);
    ]} *)

  val split_lefti : (int -> item -> bool) -> t -> t * t
  (** [split_lefti sep s] finds the first element [e] of [s] satisfying [sep]
      and returns the pair [(l, r)] where [l] is the prefix of [s] preceding [e]
      and [r] is the suffix of [s] succeeding [e].
      The first argument passed to [p] is the index, the second argument
      is the element of [s] itself.
      [e] is not included, neither in [l] nor in [r], and there is no element
      satisfying [sep] in [l].
      The result [(l, r)] satisfies the following invariant:
      [s = concat [l; sub s (length l) 1; r]].
      Raise [Not_found] if there is no element in [s] satisfying [sep].
    {[
assert (
  let q = Queue.create () in
  let l, r =
    CharSlice.split_lefti (fun i c -> Queue.add i q; c = 'f') alphabet in
  CharSlice.length l = 5 &&
  CharSlice.to_string (
    CharSlice.concat [CharSlice.sub ascii 0 (int_of_char 'a'); l;
      CharSlice.sub ascii (int_of_char 'f') 1; r;
      snd (CharSlice.split_at (int_of_char 'z' + 1) ascii)])
    == CharSlice.to_string ascii &&
  List.of_seq (Queue.to_seq q) = [0; 1; 2; 3; 4; 5]);
assert (
  let q = Queue.create () in
  (try
    ignore (FloatSlice.split_lefti (fun i f -> Queue.add i q; f < 220.)
      tones_a3_to_a5);
    false
  with Not_found ->
    true) &&
  List.of_seq (Queue.to_seq q) = List.init 24 (fun i -> i));
    ]} *)

  val split_left_opt : (item -> bool) -> t -> (t * t) option
  (** [split_left_opt sep s] finds the first element [e] of [s] satisfying [sep]
      and returns [Some (l, r)] where [l] is the prefix of [s] preceding [e]
      and [r] is the suffix of [s] succeeding [e]. [e] is not included, neither
      in [l] nor in [r], and there is no element satisfying [sep] in [l].
      The result [Some (l, r)] satisfies the following invariant:
      [s = concat [l; sub s (length l) 1; r]].
      Return [None] if there is no element in [s] satisfying [sep].
    {[
assert (
  match CharSlice.split_left_opt (fun c -> c = 'n') alphabet with
  | None -> false
  | Some (l, r) ->
      CharSlice.length l = 13 &&
      CharSlice.to_string (
        CharSlice.concat [CharSlice.sub ascii 0 (int_of_char 'a'); l;
          CharSlice.sub ascii (int_of_char 'n') 1; r;
          snd (CharSlice.split_at (int_of_char 'z' + 1) ascii)])
        == CharSlice.to_string ascii);
assert (FloatSlice.split_left_opt (fun f -> f >= 880.) tones_a3_to_a5 == None);
    ]} *)

  val split_lefti_opt : (int -> item -> bool) -> t -> (t * t) option
  (** [split_lefti_opt sep s] finds the first element [e] of [s] satisfying
      [sep] and returns [Some (l, r)] where [l] is the prefix of [s]
      preceding [e] and [r] is the suffix of [s] succeeding [e].
      The first argument passed to [p] is the index, the second argument
      is the element of [s] itself.
      [e] is not included, neither in [l] nor in [r], and there is no element
      satisfying [sep] in [l].
      The result [Some (l, r)] satisfies the following invariant:
      [s = concat [l; sub s (length l) 1; r]].
      Return [None] if there is no element in [s] satisfying [sep].
    {[
assert (
  let q = Queue.create () in
  match FloatSlice.split_lefti_opt (fun i x -> Queue.add i q; x > 440.)
    tones_a3_to_a5 with
  | None -> false
  | Some (l, r) ->
      FloatSlice.length l = 13 &&
      FloatSlice.unsafe_to_floatarray (
        FloatSlice.concat [FloatSlice.sub tones_a2_to_a6 0 12; l;
          FloatSlice.sub tones_a2_to_a6 25 1; r;
          FloatSlice.sub tones_a2_to_a6 36 12])
        == FloatSlice.unsafe_to_floatarray tones_a2_to_a6 &&
      List.of_seq (Queue.to_seq q) = List.init 14 (fun i -> i));
assert (
  let q = Queue.create () in
  CharSlice.split_lefti_opt (fun i c -> Queue.add i q; c = '!') alphabet = None
  && List.of_seq (Queue.to_seq q) = List.init 26 (fun i -> i));
    ]} *)

  val split_right : (item -> bool) -> t -> t * t
  (** [split_right sep s] finds the last element [e] of [s] satisfying [sep] and
      returns the pair [(l, r)] where [l] is the prefix of [s] preceding [e] and
      [r] is the suffix of [s] succeeding [e]. [e] is not included, neither in
      [l] nor in [r], and there is no element satisfying [sep] in [r].
      The result [(l, r)] satisfies the following invariant:
      [s = concat [l; sub s (length l) 1; r]].
      Raise [Not_found] if there is no element in [s] satisfying [sep].
    {[
assert (
  let l, r =
    CharSlice.split_right (fun c -> (int_of_char c - int_of_char 'a') mod 7 = 0)
      alphabet in
  CharSlice.length l = 21 &&
  CharSlice.to_string (
    CharSlice.concat [CharSlice.sub ascii 0 (int_of_char 'a'); l;
      CharSlice.sub alphabet 21 1; r;
      snd (CharSlice.split_at (int_of_char 'z' + 1) ascii)])
    == CharSlice.to_string ascii);
assert (
  try
    ignore (FloatSlice.split_right (fun f -> f < 220.) tones_a3_to_a5);
    false
  with Not_found ->
    true);
    ]} *)

  val split_righti : (int -> item -> bool) -> t -> t * t
  (** [split_righti sep s] finds the last element [e] of [s] satisfying [sep]
      and returns the pair [(l, r)] where [l] is the prefix of [s] preceding [e]
      and [r] is the suffix of [s] succeeding [e].
      The first argument passed to [p] is the index, the second argument
      is the element of [s] itself.
      [e] is not included, neither in [l] nor in [r], and there is no element
      satisfying [sep] in [r].
      The result [(l, r)] satisfies the following invariant:
      [s = concat [l; sub s (length l) 1; r]].
      Raise [Not_found] if there is no element in [s] satisfying [sep].
    {[
assert (
  let q = Queue.create () in
  let l, r =
    FloatSlice.split_righti (fun i f -> Queue.add i q; f <= 440.)
      tones_a3_to_a5 in
  FloatSlice.length l = 12 &&
  FloatSlice.unsafe_to_floatarray (
    FloatSlice.concat [FloatSlice.sub tones_a2_to_a6 0 12; l;
      FloatSlice.sub tones_a2_to_a6 24 1; r;
      FloatSlice.sub tones_a2_to_a6 36 12])
    == FloatSlice.unsafe_to_floatarray tones_a2_to_a6 &&
  List.of_seq (Queue.to_seq q) = List.init 12 (fun i -> 23 - i));
assert (
  let q = Queue.create () in
  (try
    ignore (CharSlice.split_righti (fun i c -> Queue.add i q; c = '!')
      alphabet);
    false
  with Not_found ->
    true) &&
  List.of_seq (Queue.to_seq q) = List.init 26 (fun i -> 25 - i));
    ]} *)

  val split_right_opt : (item -> bool) -> t -> (t * t) option
  (** [split_right_opt sep s] finds the last element [e] of [s] satisfying
      [sep] and returns [Some (l, r)] where [l] is the prefix of [s] preceding
      [e] and [r] is the suffix of [s] succeeding [e]. [e] is not included,
      neither in [l] nor in [r], and there is no element satisfying [sep] in
      [r].
      The result [Some (l, r)] satisfies the following invariant:
      [s = concat [l; sub s (length l) 1; r]].
      Return [None] if there is no element in [s] satisfying [sep].
    {[
let vowels =
  CharSlice.filter
    (fun c -> c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u' || c = 'y')
      alphabet in
assert (
  match CharSlice.split_right_opt (fun c -> CharSlice.mem c vowels) alphabet
     with
  | None -> false
  | Some (l, r) ->
      CharSlice.length l = 24 &&
      CharSlice.to_string (
        CharSlice.concat [CharSlice.sub ascii 0 (int_of_char 'a'); l;
          CharSlice.sub ascii (int_of_char 'y') 1; r;
          snd (CharSlice.split_at (int_of_char 'z' + 1) ascii)])
        == CharSlice.to_string ascii);
assert (FloatSlice.split_right_opt (fun x -> x < 220.) tones_a3_to_a5 == None);
    ]} *)

  val split_righti_opt : (int -> item -> bool) -> t -> (t * t) option
  (** [split_righti_opt sep s] finds the last element [e] of [s] satisfying
      [sep] and returns [Some (l, r)] where [l] is the prefix of [s]
      preceding [e] and [r] is the suffix of [s] succeeding [e].
      The first argument passed to [p] is the index, the second argument
      is the element of [s] itself.
      [e] is not included, neither in [l] nor in [r], and there is no element
      satisfying [sep] in [r].
      The result [Some (l, r)] satisfies the following invariant:
      [s = concat [l; sub s (length l) 1; r]].
      Return [None] if there is no element in [s] satisfying [sep].
    {[
assert (
  let q = Queue.create () in
  match FloatSlice.split_righti_opt
      (fun i f -> Queue.add i q; fst (modf f) < epsilon_float)
      tones_a3_to_a5 with
  | None -> false
  | Some (l, r) ->
      FloatSlice.length l = 12 &&
      FloatSlice.unsafe_to_floatarray (
        FloatSlice.concat [FloatSlice.sub tones_a2_to_a6 0 12; l;
          FloatSlice.sub tones_a2_to_a6 24 1; r;
          FloatSlice.sub tones_a2_to_a6 36 12])
      == FloatSlice.unsafe_to_floatarray tones_a2_to_a6 &&
      List.of_seq (Queue.to_seq q) = List.init 12 (fun i -> 23 - i));
assert (
  let q = Queue.create () in
  CharSlice.split_righti_opt (fun i c -> Queue.add i q; c = '!') alphabet = None
  && List.of_seq (Queue.to_seq q) = List.init 26 (fun i -> 25 - i));
    ]} *)

  val split_at : int -> t -> t * t
  (** [split_at i s] returns the pair [(l, r)] where [l] is the prefix of
      [s] of length [i], and [r] is the suffix such that [append l r = s].
      Raise [Invalid_argument "index out of bounds"]
      if [i] is outside the range [0] to [Slice.length s].
    {[
assert (
  let l, r = CharSlice.split_at 0 alphabet in
  l == CharSlice.empty && r == alphabet);
assert (
  let l, r = FloatSlice.split_at 24 tones_a3_to_a5 in
  l == tones_a3_to_a5 && r == FloatSlice.empty);
assert (
  let l, r = CharSlice.split_at 5 alphabet in
  CharSlice.length l = 5 &&
  CharSlice.to_string (
    CharSlice.concat [CharSlice.sub ascii 0 (int_of_char 'a'); l; r;
      snd (CharSlice.split_at (int_of_char 'z' + 1) ascii)])
    == CharSlice.to_string ascii);
assert (
  try
    ignore (FloatSlice.split_at (-1) tones_a3_to_a5);
    false
  with Invalid_argument _ ->
    true);
    ]}
 *)

  val split_at_opt : int -> t -> (t * t) option
  (** [split_at_opt i s] returns [Some (l, r)] where [l] is the prefix of
      [s] of length [i], and [r] is the suffix such that [append l r = s].
      Return [None]
      if [i] is outside the range [0] to [Slice.length s].
    {[
assert (
  match FloatSlice.split_at_opt 0 tones_a3_to_a5 with
  | None -> false
  | Some (l, r) -> l == FloatSlice.empty && r == tones_a3_to_a5);
assert (
  match CharSlice.split_at_opt 26 alphabet with
  | None -> false
  | Some (l, r) -> l == alphabet && r == CharSlice.empty);
assert (
  match FloatSlice.split_at_opt 5 tones_a3_to_a5 with
  | None -> false
  | Some (l, r) ->
      FloatSlice.length l = 5 &&
      FloatSlice.unsafe_to_floatarray (
        FloatSlice.concat [FloatSlice.sub tones_a2_to_a6 0 12; l; r;
          FloatSlice.sub tones_a2_to_a6 36 12])
        == FloatSlice.unsafe_to_floatarray tones_a2_to_a6);
assert (CharSlice.split_at_opt 27 alphabet = None);
    ]} *)

  val unsafe_split_at : int -> t -> t * t
  (** [unsafe_split_at i s] returns the pair [(l, r)] where [l] is the prefix of
      [s] of length [i], and [r] is the suffix such that [append l r = s].
      The behavior is not specified
      if [i] is outside the range [0] to [Slice.length s]. *)

  val mem : item -> t -> bool
  (** [mem x s] returns [true] if there exists an element of [s] which is
      equal to [x].
    {[
assert (CharSlice.mem 'b' alphabet);
assert (not (CharSlice.mem 'B' alphabet));
    ]} *)

  val for_all : (item -> bool) -> t -> bool
  (** [for_all p s] checks if all elements of the slice satisfy the predicate
      [p]. That is, the function returns [true] if [p] returns [true] for all
      the elements of [s]; otherwise, [false] is returned as soon as [p]
      returns [false], and [p] is not called with the remaining elements.
    {[
assert (
  let q = Queue.create () in
  CharSlice.for_all (fun c -> Queue.add c q; c >= 'a' && c <= 'd')
    (CharSlice.sub ascii (int_of_char 'a') 4) &&
  Stdcompat.String.of_seq (Stdcompat.Queue.to_seq q) = "abcd");
assert (
  let q = Queue.create () in
  not (CharSlice.for_all (fun c -> Queue.add c q; c >= 'a' && c < 'c')
    (CharSlice.sub ascii (int_of_char 'a') 4)) &&
  Stdcompat.String.of_seq (Stdcompat.Queue.to_seq q) = "abc");
    ]} *)

  val for_alli : (int -> item -> bool) -> t -> bool
  (** [for_alli p s] checks if all elements of the slice satisfy the predicate
      [p]. That is, the function returns [true] if [p] returns [true] for all
      the elements of [s]; otherwise, [false] is returned as soon as [p]
      returns [false], and [p] is not called with the remaining elements.
      The first argument passed to [f] is the index, the second argument
      is the element itself.
    {[
assert (
  let q = Queue.create () in
  CharSlice.for_alli (fun i c -> Queue.add (i, c) q; c >= 'a' && c <= 'd')
    (CharSlice.sub ascii (int_of_char 'a') 4) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 'a'; 1, 'b'; 2, 'c'; 3, 'd']);
assert (
  let q = Queue.create () in
  not (CharSlice.for_alli (fun i c -> Queue.add (i, c) q; c >= 'a' && c < 'c')
    (CharSlice.sub ascii (int_of_char 'a') 4)) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 'a'; 1, 'b'; 2, 'c']);
    ]} *)

  val for_all2 : (item -> item -> bool) -> t -> t -> bool
  (** [for_all2 p a b] checks that [a] and [b] are the same length,
      and checks if all elements of the slices satisfy the predicate
      [p]. That is, the function returns [true] if [p] returns [true] for all
      the elements of [a] and [b]; otherwise, [false] is returned as soon as [p]
      returns [false], and [p] is not called with the remaining elements.
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      Raise [Invalid_argument] if the slices are not the same length: in this
      case, the function [p] is never called.
    {[
assert (
  let slice6 = FloatSlice.init 6 float_of_int in
  let q = Queue.create () in
  FloatSlice.for_all2 (fun i j -> Queue.add (i, j) q; i < j)
    (FloatSlice.sub slice6 1 4)
    (FloatSlice.sub slice6 2 4) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1., 2.; 2., 3.; 3., 4.; 4., 5.]);
assert (
  let slice6 = FloatSlice.init 6 float_of_int in
  let q = Queue.create () in
  not (FloatSlice.for_all2 (fun i j -> Queue.add (i, j) q; i < 3.)
    (FloatSlice.sub slice6 1 4)
    (FloatSlice.sub slice6 2 4)) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1., 2.; 2., 3.; 3., 4.]);
assert (
  try
    ignore (CharSlice.for_all2 (fun i j -> false)
      (CharSlice.sub ascii 0 3) (CharSlice.sub ascii 1 2));
    false
  with Invalid_argument _ ->
    true);
    ]} *)

  val for_all2_opt : (item -> item -> bool) -> t -> t -> bool option
  (** [for_all2_opt p a b] checks that [a] and [b] are the same length,
      and checks if all elements of the slice satisfy the predicate
      [p]. That is, the function returns [Some true] if [p] returns [true] for
      all the elements of [a] and [b];
      otherwise, [Some false] is returned as soon as
      [p] returns [false], and [p] is not called with the remaining elements.
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      Return [None] if the slices are not the same length: in this
      case, the function [p] is never called.
    {[
assert (
  let slice6 = FloatSlice.init 6 float_of_int in
  let q = Queue.create () in
  FloatSlice.for_all2_opt (fun i j -> Queue.add (i, j) q; i < j)
    (FloatSlice.sub slice6 1 4)
    (FloatSlice.sub slice6 2 4) = Some true &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1., 2.; 2., 3.; 3., 4.; 4., 5.]);
assert (
  let slice6 = FloatSlice.init 6 float_of_int in
  let q = Queue.create () in
  FloatSlice.for_all2_opt (fun i j -> Queue.add (i, j) q; i < 3.)
    (FloatSlice.sub slice6 1 4)
    (FloatSlice.sub slice6 2 4) = Some false &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1., 2.; 2., 3.; 3., 4.]);
assert (
  CharSlice.for_all2_opt (fun i j -> false)
    (CharSlice.sub ascii 0 3) (CharSlice.sub ascii 1 2) = None);
    ]} *)

  val unsafe_for_all2 : (item -> item -> bool) -> t -> t -> bool
  (** [unsafe_for_all2 p a b] checks that [a] and [b] are the same length,
      and checks if all elements of the slices satisfy the predicate
      [p]. That is, the function returns [true] if [p] returns [true] for all
      the elements of [a] and [b]; otherwise, [false] is returned as soon as [p]
      returns [false], and [p] is not called with the remaining elements.
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      The behaviour is unspecified if the slices are not the same length. *)

  val for_all2i : (int -> item -> item -> bool) -> t -> t -> bool
  (** [for_all2i p a b] checks that [a] and [b] are the same length,
      and checks if all elements of the slice satisfy the predicate
      [p]. That is, the function returns [true] if [p] returns [true] for all
      the elements of [a] and [b]; otherwise, [false] is returned as soon as [p]
      returns [false], and [p] is not called with the remaining elements.
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      Raise [Invalid_argument] if the slices are not the same length: in this
      case, the function [p] is never called.
    {[
assert (
  let slice6 = FloatSlice.init 6 float_of_int in
  let q = Queue.create () in
  FloatSlice.for_all2i (fun i j k -> Queue.add (i, j, k) q; j < k)
    (FloatSlice.sub slice6 1 4)
    (FloatSlice.sub slice6 2 4) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1., 2.; 1, 2., 3.; 2, 3., 4.; 3, 4., 5.]);
assert (
  let slice6 = FloatSlice.init 6 float_of_int in
  let q = Queue.create () in
  not (FloatSlice.for_all2i (fun i j k -> Queue.add (i, j, k) q; j < 3.)
    (FloatSlice.sub slice6 1 4)
    (FloatSlice.sub slice6 2 4)) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1., 2.; 1, 2., 3.; 2, 3., 4.]);
assert (
  try
    ignore (CharSlice.for_all2i (fun i j k -> false)
      (CharSlice.sub ascii 0 3) (CharSlice.sub ascii 1 2));
    false
  with Invalid_argument _ ->
    true);
    ]} *)

  val for_all2i_opt : (int -> item -> item -> bool) -> t -> t -> bool option
  (** [for_all2i_opt p a b] checks that [a] and [b] are the same length,
      and checks if all elements of the slice satisfy the predicate
      [p]. That is, the function returns [Some true] if [p] returns [true] for
      all the elements of [a] and [b]; otherwise, [Some false] is returned as
      soon as [p] returns [false], and [p] is not called with the remaining
      elements.
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      Return [None] if the slices are not the same length: in this
      case, the function [p] is never called.
    {[
assert (
  let slice6 = FloatSlice.init 6 float_of_int in
  let q = Queue.create () in
  FloatSlice.for_all2i_opt (fun i j k -> Queue.add (i, j, k) q; j < k)
    (FloatSlice.sub slice6 1 4)
    (FloatSlice.sub slice6 2 4) = Some true &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1., 2.; 1, 2., 3.; 2, 3., 4.; 3, 4., 5.]);
assert (
  let slice6 = FloatSlice.init 6 float_of_int in
  let q = Queue.create () in
  FloatSlice.for_all2i_opt (fun i j k -> Queue.add (i, j, k) q; j < 3.)
    (FloatSlice.sub slice6 1 4)
    (FloatSlice.sub slice6 2 4) = Some false &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1., 2.; 1, 2., 3.; 2, 3., 4.]);
assert (
  CharSlice.for_all2i_opt (fun i j k -> false)
    (CharSlice.sub ascii 0 3) (CharSlice.sub ascii 1 2) = None);
    ]} *)

  val unsafe_for_all2i : (int -> item -> item -> bool) -> t -> t -> bool
  (** [unsafe_for_all2i p a b] checks that [a] and [b] are the same length,
      and checks if all elements of the slice satisfy the predicate
      [p]. That is, the function returns [true] if [p] returns [true] for all
      the elements of [a] and [b]; otherwise, [false] is returned as soon as [p]
      returns [false], and [p] is not called with the remaining elements.
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      The behaviour is unspecified if the slices are not the same length. *)

  val exists : (item -> bool) -> t -> bool
  (** [exists p s] checks if at least one element of the slice satisfies the
      predicate [p].
      That is, the function returns [true] as soon as [p] returns [true] for one
      element of [s], and [p] is not called with the remaining elements;
      otherwise, [false] is returned since [p] returns [false] for all elements
      of [s].
    {[
assert (
  let q = Queue.create () in
  CharSlice.exists (fun c -> Queue.add c q; c = 'c')
    (CharSlice.sub ascii (int_of_char 'a') 4) &&
  Stdcompat.String.of_seq (Stdcompat.Queue.to_seq q) = "abc");
assert (
  let q = Queue.create () in
  not (CharSlice.exists (fun c -> Queue.add c q; c = 'e')
    (CharSlice.sub ascii (int_of_char 'a') 4)) &&
  Stdcompat.String.of_seq (Stdcompat.Queue.to_seq q) = "abcd");
    ]} *)

  val existsi : (int -> item -> bool) -> t -> bool
  (** [existsi p s] checks if at least one element of the slice satisfies the
      predicate [p].
      That is, the function returns [true] as soon as [p] returns [true] for one
      element of [s], and [p] is not called with the remaining elements;
      otherwise, [false] is returned since [p] returns [false] for all elements
      of [s].
      The first argument passed to [f] is the index, the second argument
      is the element itself.
    {[
assert (
  let q = Queue.create () in
  CharSlice.existsi (fun i c -> Queue.add (i, c) q; c = 'c')
    (CharSlice.sub ascii (int_of_char 'a') 4) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) = [0, 'a'; 1, 'b'; 2, 'c']);
assert (
  let q = Queue.create () in
  not (CharSlice.existsi (fun i c -> Queue.add (i, c) q; c = 'e')
    (CharSlice.sub ascii (int_of_char 'a') 4)) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q)
    = [0, 'a'; 1, 'b'; 2, 'c'; 3, 'd']);
    ]} *)

  val exists2 : (item -> item -> bool) -> t -> t -> bool
  (** [exists2 p a b] checks if at least one index for which the element of [a]
      and the element of [b] at this same index satisfies the
      predicate [p].
      That is, the function returns [true] as soon as [p] returns [true] for one
      such pair of elements, and [p] is not called with the remaining elements;
      otherwise, [false] is returned since [p] returns [false] for all pairs of
      elements.
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      Raise [Invalid_argument] if the slices are not the same length: in this
      case, the function [p] is never called.
    {[
assert (
  let q = Queue.create () in
  Slice.exists2 (fun i j -> Queue.add (i, j) q; i = 3)
    (Slice.sub slice6 1 4) (Slice.sub slice6 2 4) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1, 2; 2, 3; 3, 4]);
assert (
  let q = Queue.create () in
  not (Slice.exists2 (fun i j -> Queue.add (i, j) q; i < j)
    (Slice.sub slice6 1 5) (Slice.sub slice6 0 5)) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1, 0; 2, 1; 3, 2; 4, 3; 5, 4]);
assert (
  try
    ignore (Slice.exists2 (fun i j -> false)
      (Slice.sub slice6 0 3) (Slice.sub slice6 1 2));
    false
  with Invalid_argument _ ->
    true);
    ]} *)

  val exists2_opt : (item -> item -> bool) -> t -> t -> bool option
  (** [exists2 p a b] checks if at least one index for which the element of [a]
      and the element of [b] at this same index satisfies the
      predicate [p].
      That is, the function returns [true] as soon as [p] returns [true] for one
      such pair of elements, and [p] is not called with the remaining elements;
      otherwise, [false] is returned since [p] returns [false] for all pairs of
      elements.
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      Raise [Invalid_argument] if the slices are not the same length: in this
      case, the function [p] is never called.
    {[
assert (
  let q = Queue.create () in
  Slice.exists2 (fun i j -> Queue.add (i, j) q; i = 3)
    (Slice.sub slice6 1 4) (Slice.sub slice6 2 4) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1, 2; 2, 3; 3, 4]);
assert (
  let q = Queue.create () in
  not (Slice.exists2 (fun i j -> Queue.add (i, j) q; i < j)
    (Slice.sub slice6 1 5) (Slice.sub slice6 0 5)) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1, 0; 2, 1; 3, 2; 4, 3; 5, 4]);
assert (
  try
    ignore (Slice.exists2 (fun i j -> false)
      (Slice.sub slice6 0 3) (Slice.sub slice6 1 2));
    false
  with Invalid_argument _ ->
    true);
    ]} *)

  val unsafe_exists2 : (item -> item -> bool) -> t -> t -> bool
  (** [unsafe_exists2 p a b] checks if at least one index for which the element
      of [a] and the element of [b] at this same index satisfies the
      predicate [p].
      That is, the function returns [true] as soon as [p] returns [true] for one
      such pair of elements, and [p] is not called with the remaining elements;
      otherwise, [false] is returned since [p] returns [false] for all pairs of
      elements.
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      The behaviour is unspecified if the slices are not the same length. *)

  val exists2i : (int -> item -> item -> bool) -> t -> t -> bool
  (** [exists2i p a b] checks if at least one index for which the element of [a]
      and the element of [b] at this same index satisfies the
      predicate [p].
      That is, the function returns [true] as soon as [p] returns [true] for one
      such pair of elements, and [p] is not called with the remaining elements;
      otherwise, [false] is returned since [p] returns [false] for all pairs of
      elements.
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      Raise [Invalid_argument] if the slices are not the same length: in this
      case, the function [p] is never called.
    {[
assert (
  let q = Queue.create () in
  Slice.exists2i (fun i j k -> Queue.add (i, j, k) q; i = 2)
    (Slice.sub slice6 1 4) (Slice.sub slice6 2 4) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1, 2; 1, 2, 3; 2, 3, 4]);
assert (
  let q = Queue.create () in
  not (Slice.exists2i (fun i j k -> Queue.add (i, j, k) q; i > j)
    (Slice.sub slice6 1 5) (Slice.sub slice6 0 5)) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1, 0; 1, 2, 1; 2, 3, 2; 3, 4, 3; 4, 5, 4]);
assert (
  try
    ignore (Slice.exists2i (fun i j k -> false)
      (Slice.sub slice6 0 3) (Slice.sub slice6 1 2));
    false
  with Invalid_argument _ ->
    true);
    ]} *)

  val exists2i_opt : (int -> item -> item -> bool) -> t -> t -> bool option
  (** [exists2i_opt p a b] checks that [a] and [b] are the same length,
      and checks if at least one index for which the element of [a]
      and the element of [b] at this same index satisfies the
      predicate [p].
      That is, the function returns [Some true] as soon as [p] returns [true]
      for one such pair of elements, and [p] is not called with the remaining
      elements;
      otherwise, [Some false] is returned since [p] returns [false] for all
      pairs of elements.
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      Return [None] if the slices are not the same length: in this
      case, the function [p] is never called.
    {[
assert (
  let q = Queue.create () in
  Slice.exists2i_opt (fun i j k -> Queue.add (i, j, k) q; i = 2)
    (Slice.sub slice6 1 4) (Slice.sub slice6 2 4) = Some true &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1, 2; 1, 2, 3; 2, 3, 4]);
assert (
  let q = Queue.create () in
  Slice.exists2i_opt (fun i j k -> Queue.add (i, j, k) q; i > j)
    (Slice.sub slice6 1 5) (Slice.sub slice6 0 5) = Some false &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1, 0; 1, 2, 1; 2, 3, 2; 3, 4, 3; 4, 5, 4]);
assert (
  Slice.exists2i_opt (fun i j k -> false)
    (Slice.sub slice6 0 3) (Slice.sub slice6 1 2) = None);
    ]} *)

  val unsafe_exists2i : (int -> item -> item -> bool) -> t -> t -> bool
  (** [unsafe_exists2i p a b] checks if at least one index for which the element
      of [a] and the element of [b] at this same index satisfies the
      predicate [p].
      That is, the function returns [true] as soon as [p] returns [true] for one
      such pair of elements, and [p] is not called with the remaining elements;
      otherwise, [false] is returned since [p] returns [false] for all pairs of
      elements.
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      The behaviour is unspecified if the slices are not the same length. *)

  val fold_left : ('a -> item -> 'a) -> 'a -> t -> 'a
  (** [fold_left f init s] computes [f (...(f (f init e0) e1)...) en], where
      [e0], ..., [en] are the elements of [s], and [n] is [length s - 1].
    {[
assert (
  Slice.fold_left (fun accu i -> i :: accu) []
    (Slice.sub slice6 1 4) = [4; 3; 2; 1]);
    ]} *)

  val fold_lefti : (int -> 'a -> item -> 'a) -> 'a -> t -> 'a
  (** [fold_lefti f init s] computes [f n (...(f 1 (f 0 init e0) e1)...) en],
      where [e0], ..., [en] are the elements of [s],
      and [n] is [length s - 1].
    {[
assert (
  Slice.fold_lefti (fun i accu j -> (i, j) :: accu) []
    (Slice.sub slice6 1 4) = [3, 4; 2, 3; 1, 2; 0, 1]);
    ]} *)

  val fold_right : (item -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold_right f s init] computes [f e0 (f e1 (...(f en init)))], where
      [e0], ..., [en] are the elements of [s], and [n] is [length s - 1].
    {[
assert (
  Slice.fold_right (fun i accu -> i :: accu)
    (Slice.sub slice6 1 4) [] = [1; 2; 3; 4]);
    ]} *)

  val fold_righti : (int -> item -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold_righti f s init] computes [f 0 e0 (f 1 e1 (...(f n en init)))],
      where [e0], ..., [en] are the elements of [s],
      and [n] is [length s - 1].
    {[
assert (
  Slice.fold_righti (fun i j accu -> (i, j) :: accu)
    (Slice.sub slice6 1 4) [] = [0, 1; 1, 2; 2, 3; 3, 4]);
    ]} *)

  val compare : (item -> item -> int) -> t -> t -> int
  (** [compare cmp a b], given an order [cmp] over elements, compares
      the two slices [a] and [b] and provides an order over slices:
      that is to say, the function returns [0] is [a] equals [b],
      a strictly negative value if [a] is less than [b] and
      a strictly positive value if [a] is greater than [b].
      Slices are first ordered by increasing length, and [cmp] is called
      only if [a] and [b] are the same length. Moreover, [cmp] is called until
      finding a
      pair of elements for which the result is not null, and in this
      case the result of [cmp] is returned. [0] is returned if and only
      if the two slices are the same length and [cmp] returns [0] for
      every pair of elements. The order is therefore total if and only if
      [cmp] is a total order.
    {[
assert(
  Slice.compare compare (Slice.sub slice6 1 3) (Slice.sub slice6 2 2) > 0);
assert(
  Slice.compare compare (Slice.sub slice6 2 3) (Slice.sub slice6 1 4) < 0);
assert(
  Slice.compare compare (Slice.sub slice6 2 3) (Slice.sub slice6 3 3) < 0);
assert(
  Slice.compare compare (Slice.sub slice6 2 3) (Slice.sub slice6 0 3) > 0);
assert(
  Slice.compare compare (Slice.sub slice6 2 3) (Slice.sub slice6 2 3) = 0);
assert(
  Slice.compare (fun i j -> compare j i)
    (Slice.sub slice6 2 3) (Slice.sub slice6 0 3) < 0);
    ]} *)

  val equal : (item -> item -> bool) -> t -> t -> bool
  (** [equal eq a b], given an equality predicate [eq] over elements,
      checks whether [a] and [b] are equal. Lengths are checked first, and
      [eq] is called only if [a] and [b] are the same length. Moreover, [eq] is
      called until finding a pair of elements which are not equal for [eq].
      [true] is returned if and only if the two slices are the same length
      and [eq] returns [true] for every pair of elements.
    {[
assert(
  not (Slice.equal ( = ) (Slice.sub slice6 1 3) (Slice.sub slice6 2 2)));
assert(
  not (Slice.equal ( = ) (Slice.sub slice6 1 3) (Slice.sub slice6 2 3)));
assert(
  Slice.equal ( = ) (Slice.sub slice6 1 3) (Slice.sub slice6 1 3));
assert(
  Slice.equal (fun i j -> i mod 2 = j mod 2)
    (Slice.sub slice6 1 3) (Slice.sub slice6 3 3));
    ]} *)

  val hash : (item -> int) -> t -> int
  (** [hash h s] returns a hash for [s], given an hash function [h] over
      elements.
    {[
assert (
  Slice.hash Hashtbl.hash (Slice.sub slice6 1 3) =
  Slice.hash Hashtbl.hash (Slice.init 3 succ));
assert (
  Slice.hash Hashtbl.hash (Slice.sub slice6 1 4) <>
  Slice.hash Hashtbl.hash (Slice.init 3 succ));
assert (
  Slice.hash Hashtbl.hash (Slice.sub slice6 1 5) <>
  Slice.hash Hashtbl.hash (Slice.init 5 pred));
    ]} *)

  val to_seq : t -> item Stdcompat.Seq.t
  (** [to_seq s] iterates on the elements of [s].
      If the slice is mutable, modifications of the slice during iteration
      will be reflected in the iterator.
    {[
assert (
  List.of_seq (Slice.to_seq (Slice.sub slice6 1 4)) =
    [1; 2; 3; 4]);
    ]} *)

  val to_seqi : t -> (int * item) Stdcompat.Seq.t
  (** [to_seqi s] iterates on the elements of [s], yielding indices as
      first components and the elements themselves as second components.
      If the slice is mutable, modifications of the slice during iteration
      will be reflected in the iterator. *)

  val of_seq : item Stdcompat.Seq.t -> t
  (** [of_seq s] creates a slice from the sequence [s].
    {[
assert (
  Slice.equal ( = ) (Slice.of_seq (List.to_seq [1; 2; 3]))
    (Slice.sub slice6 1 3));
    ]} *)

  val of_list : item list -> t
  (** [of_list l] creates a slice from the list [l].
    {[
assert (
  Slice.equal ( = ) (Slice.of_list [2; 3; 4; 5])
    (Slice.sub slice6 2 4));
    ]} *)

  val to_list : t -> item list
  (** [to_list s] returns the list of the elements of [s].
    {[
assert (Slice.to_list (Slice.sub slice6 1 5) = [1; 2; 3; 4; 5]);
    ]} *)

  val of_array : item array -> t
  (** [of_array a] creates a fresh slice covering all the elements of [a].
      Elements are always copied.
    ]} *)

  val to_array : t -> item array
  (** [to_array s] creates a fresh array with the elements of [s]. *)

  val map_to_array : (item -> item) -> t -> item array
  (** [map_to_array f s] applies function [f] to all the elements of [s], and
      buids an array with the results returned by [f].
    {[
assert (
  FloatSlice.map_to_array (fun i -> i *. 2.)
      (FloatSlice.sub (FloatSlice.init 6 float_of_int) 1 5)
    = [| 2.; 4.; 6.; 8.; 10. |]);
    ]} *)
end

module type GenS = sig
  type 'a t
  (** The type of a generic slice. *)

  val empty : 'a t
  (** The empty slice: this is the unique 0-length slice of this type, i.e.
a slice [s] is 0-length if and only if [s] is physically equal to [empty].
    {[
assert (Slice.length Slice.empty = 0);
assert (Slice.of_list [] == Slice.empty);
assert (Slice.of_list [()] != Slice.empty);
    ]} *)

  val is_empty : 'a t -> bool
  (** [is_empty s] returns [true] if [s] is the empty slice, i.e. [s] is
0-length or, equivalently, [s] is physically equal to [empty].
    {[
assert (Slice.is_empty Slice.empty);
assert (Slice.is_empty (Slice.make 0 ()));
assert (not (Slice.is_empty (Slice.make 1 ())));
assert (Slice.is_empty (Slice.of_list []));
assert (not (Slice.is_empty (Slice.of_list [()])));
    ]} *)


  val make : int -> 'a -> 'a t
  (** [make n x] returns a fresh slice of length [n], initialized with [x].
    {[
assert (Slice.make 0 () == Slice.empty);
assert (Slice.length (Slice.make 3 ()) = 3);
assert (Slice.to_list (Slice.make 4 "a") = ["a"; "a"; "a"; "a"]);
    ]} *)

  val init : int -> (int -> 'a) -> 'a t
  (** [init n f] returns a fresh slice of length [n], with element index [i]
      initialized to the result of [f i].
    {[
assert (Slice.init 0 (fun i -> i) == Slice.empty);
assert (Slice.length (Slice.init 3 (fun i -> i)) = 3);
assert (Slice.to_list (Slice.init 3 (fun i -> i)) = [0; 1; 2]);
    ]} *)

  val length : 'a t -> int
  (** [length s] returns the length (number of elements) of slice [s].
    {[
assert (
  let slice6 = Slice.init 6 (fun i -> i) in
  Slice.length (Slice.sub slice6 2 3) = 3);
    ]} *)

  val get : 'a t -> int -> 'a
  (** [get s i] returns the element index [i] of slice [s].
      The first element has index 0.
      Raise [Invalid_argument "index out of bounds"]
      if [i] is outside the range [0] to [Slice.length s - 1].
    {[
assert (Slice.get (Slice.init 3 (fun i -> i)) 2 = 2);
assert (
  try
    ignore (Slice.get (Slice.init 3 (fun i -> i)) 3);
    false
  with Invalid_argument _ ->
    true
);
assert (
  try
    ignore (Slice.get (Slice.init 3 (fun i -> i)) (-1));
    false
  with Invalid_argument _ ->
    true
);
    ]} *)

  val get_opt : 'a t -> int -> 'a option
  (** [get_opt s i] returns the element index [i] of slice [s].
      The first element has index 0.
      Return [None] if [i] is outside the range [0] to
      [Slice.length s - 1].
    {[
assert (Slice.get_opt (Slice.init 3 (fun i -> i)) 1 = (Some 1));
assert (Slice.get_opt (Slice.init 3 (fun i -> i)) 4 = None);
assert (Slice.get_opt (Slice.init 3 (fun i -> i)) (-2) = None);
    ]} *)

  val unsafe_get : 'a t -> int -> 'a
  (** [unsafe_get s i] returns the element index [i] of slice [s].
      The first element has index 0.
      Indices are unchecked and the behavior is unspecified if
      [i] is outside the range [0] to [Slice.length s - 1]. *)

  val sub : 'a t -> int -> int -> 'a t
  (** [sub s off len] returns a subslice of length [len], containing
      the elements index [off] to [off + len - 1] of slice [s].
      The elements are not copied. In particular, if the slice is
      mutable, then modifying the subslice modify
      the original slice too.
      Raise [Invalid_argument "index out of bounds"]
      if [off] and [len] do not designate a valid subslice of [s];
      that is, if [off < 0], or [len < 0], or [off + len > Slice.length s].
    {[
let slice6 = Slice.init 6 (fun i -> i) in
assert (Slice.sub slice6 0 6 == slice6);
assert (Slice.sub slice6 0 0 == Slice.empty);
assert (
  Slice.to_list (Slice.sub slice6 2 3) = [2; 3; 4]);
assert (
  Slice.to_list (Slice.sub (Slice.sub slice6 2 4) 1 3)
  = [3; 4; 5]);
assert (
  try
    ignore (Slice.sub slice6 2 5);
    false
  with Invalid_argument _ ->
    true
);
assert (
  try
    ignore (Slice.sub slice6 (-1) 5);
    false
  with Invalid_argument _ ->
    true
);
assert (
  try
    ignore (Slice.sub slice6 4 (-2));
    false
  with Invalid_argument _ ->
    true
);
assert (
  try
    ignore (Slice.sub (Slice.sub slice6 1 3) 2 3);
    false
  with Invalid_argument _ ->
    true
);
    ]} *)

  val sub_opt : 'a t -> int -> int -> 'a t option
  (** [sub_opt s off len] returns a subslice of length [len], containing
      the elements index [off] to [off + len - 1] of slice [s].
      The elements are not copied. In particular, if the slice is
      mutable, then modifying the subslice modify
      the original slice too.
      Return [None]
      if [off] and [len] do not designate a valid subslice of [s];
      that is, if [off < 0], or [len < 0], or [off + len > Slice.length s].
    {[
assert (
  option_exists (fun slice -> Slice.to_list slice = [1; 2; 3; 4])
    (Slice.sub_opt slice6 1 4));
assert (Slice.sub_opt slice6 6 1 = None);
assert (Slice.sub_opt slice6 (-1) 2 = None);
    ]} *)

  val unsafe_sub : 'a t -> int -> int -> 'a t
  (** [unsafe_sub s off len] returns a subslice of length [len], containing
      the elements index [off] to [off + len - 1] of slice [s].
      The elements are not copied. In particular, if the slice is
      mutable, then modifying the subslice modify
      the original slice too.
      Indices are unchecked and the behavior is unspecified if
      [off] and [len] do not designate a valid subslice of [s];
      that is, if [off < 0], or [len < 0], or [off + len > Slice.length s]. *)

  val append : 'a t -> 'a t -> 'a t
  (** [append a b] returns a slice containing the concatenation of the slices
      [a] and [b].
      If slices are immutable, the elements are copied if and only if the
      slices [a] and [b] are not contiguous and neither [a] nor [b] are empty.
      In particular, if [a] or [b] is empty, then the other slice is returned
      unchanged.
      If slices are mutable, the returned slice is fresh; that is, the elements
      are always copied: use {!val:MutGenS.append_preserve} to get the same
      behaviour than in the immutable case.
    {[
assert (Slice.append (Slice.sub slice6 3 0) slice6 == slice6);
assert (Slice.append slice6 (Slice.sub slice6 2 0) == slice6);
assert (
  Slice.length (Slice.append (Slice.sub slice6 1 5)
    (Slice.sub slice6 0 3)) = 8);
assert (
  Slice.to_list (Slice.append (Slice.sub slice6 3 2)
    (Slice.sub slice6 2 4)) = [3; 4; 2; 3; 4; 5]);
assert (
  Slice.unsafe_to_array (Slice.append (Slice.sub slice6 0 4)
    (Slice.sub slice6 4 2)) == Slice.unsafe_to_array slice6);
assert (
  let mut_slice6 = MutSlice.init 6 (fun i -> i) in
  let mut_slice6' = MutSlice.append (MutSlice.sub mut_slice6 0 4)
    (MutSlice.sub mut_slice6 4 2) in
  MutSlice.set mut_slice6' 2 3;
  MutSlice.get mut_slice6 2 = 2);
    ]} *)

  val concat : 'a t list -> 'a t
  (** [concat l] returns a slice containing the concatenation of the slices
      listed in [l].
      If slices are immutable, the elements are copied if and only if the
      non-empty slices in [l] are not contiguous. In particular, if there is
      only one non-empty slice in [l], then this slice is returned unchanged.
      If slices are mutable, the returned slice is fresh; that is, the elements
      are always copied: use {!val:MutGenS.concat_preserve} to get the same
      behaviour than in the immutable case.
    {[
assert (
  Slice.to_list (Slice.concat
    ([Slice.sub slice6 2 2; Slice.sub slice6 3 3; Slice.sub slice6 4 2]))
  = [2; 3; 3; 4; 5; 4; 5]);
assert (
  Slice.concat ([Slice.sub slice6 2 0; slice6; Slice.sub slice6 4 0])
  == slice6);
assert (
  Slice.unsafe_to_array (Slice.concat [Slice.sub slice6 0 4;
    Slice.sub slice6 4 2]) == Slice.unsafe_to_array slice6);
assert (
  let mut_slice6 = MutSlice.init 6 (fun i -> i) in
  let mut_slice6' = MutSlice.concat [MutSlice.sub mut_slice6 0 4;
    MutSlice.sub mut_slice6 4 2] in
  MutSlice.set mut_slice6' 2 3;
  MutSlice.get mut_slice6 2 = 2);
    ]} *)

  val singleton : 'a -> 'a t
  (** [singleton x] returns a slice containing exactly one element, [x].
    {[
assert (Slice.length (Slice.singleton 1) = 1);
assert (Slice.to_list (Slice.singleton 2) = [2]);
    ]} *)

  val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f s] applies function [f] in turn to all elements of [s].
    {[
assert (
  let q = Queue.create () in
  Slice.iter (fun i -> Queue.add i q) (Slice.sub slice6 2 4);
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) = [2; 3; 4; 5]);
    ]} *)

  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  (** [iteri f s] applies function [f] in turn to all elements of [s],
with the index of the element as first argument, and the element itself
as second argument.
    {[
assert (
  let q = Queue.create () in
  Slice.iteri (fun i j -> Queue.add (i, j) q) (Slice.sub slice6 1 5);
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1; 1, 2; 2, 3; 3, 4; 4, 5]);
    ]} *)

  val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
  (** [iter2 f a b] checks that [a] and [b] are the same length, and
applies function [f] to all elements of [a] and [b].
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
Raise [Invalid_argument] if the slices are not the same length: in this
case, the function [f] is never called.
    {[
assert (
  let q = Queue.create () in
  Slice.iter2 (fun i j -> Queue.add (i, j) q)
    (Slice.sub slice6 2 4)
    (Slice.sub (Slice.init 6 (fun i -> i mod 3 = 0)) 1 4);
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [2, false; 3, false; 4, true; 5, false]);
assert (
  try
    Slice.iter2 (fun i j -> ()) (Slice.sub slice6 1 4) (Slice.sub slice6 1 3);
    false
  with Invalid_argument _ ->
    true);
    ]} *)

  val try_iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> bool
  (** [try_iter2 f a b] checks that [a] and [b] are the same length, and
      applies function [f] to all elements of [a] and [b], and returns [true].
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      Return [false] if the slices are not the same length: in this
      case, the function [f] is never called.
    {[
assert (
  let q = Queue.create () in
  Slice.try_iter2 (fun i j -> Queue.add (i, j) q)
    (Slice.sub slice6 1 5)
    (Slice.sub (Slice.init 6 (fun i -> i mod 3 = 0)) 0 5) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1, true; 2, false; 3, false; 4, true; 5, false]);
assert (
  not (Slice.try_iter2 (fun i j -> ())
    (Slice.sub slice6 1 2) (Slice.sub slice6 2 1)));
    ]} *)

  val unsafe_iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
  (** [unsafe_iter2 f a b] applies function [f] to all elements of [a] and [b].
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      The behaviour is unspecified if the slices are not the same length. *)

  val iter2i : (int -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
  (** [iter2i f a b] checks that [a] and [b] are the same length, and
      applies function [f] to all elements of [a] and [b].
      The first argument passed to [f] is the index, the second argument
      is the element of [a] and the third argument is the element of [b].
      Raise [Invalid_argument] if the slices are not the same length: in this
      case, the function [f] is never called.
    {[
assert (
  let q = Queue.create () in
  Slice.iter2i (fun i j k -> Queue.add (i, j, k) q)
    (Slice.sub slice6 3 3) (Slice.sub slice6 2 3);
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 3, 2; 1, 4, 3; 2, 5, 4]);
assert (
  try
    Slice.iter2i (fun i j k -> ())
      (Slice.sub slice6 0 3) (Slice.sub slice6 1 2);
    false
  with Invalid_argument _ ->
    true);
    ]} *)


  val try_iter2i : (int -> 'a -> 'b -> unit) -> 'a t -> 'b t -> bool
  (** [try_iter2i f a b] checks that [a] and [b] are the same length, and
      applies function [f] to all elements of [a] and [b], and returns [true].
      The first argument passed to [f] is the index, the second argument
      is the element of [a] and the third argument is the element of [b].
      Return [false] if the slices are not the same length: in this
      case, the function [f] is never called.
    {[
assert (
  let q = Queue.create () in
  Slice.try_iter2i (fun i j k -> Queue.add (i, j, k) q)
    (Slice.sub slice6 1 5)
    (Slice.sub (Slice.init 6 (fun i -> i mod 3 = 0)) 0 5) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1, true; 1, 2, false; 2, 3, false; 3, 4, true; 4, 5, false]);
assert (
  not (Slice.try_iter2i (fun i j k -> ())
    (Slice.sub slice6 1 2) (Slice.sub slice6 2 1)));
    ]} *)

  val unsafe_iter2i : (int -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
  (** [unsafe_iter2i f a b] applies function [f] to all elements of [a] and
      [b], assuming that [a] and [b] are the same length.
      The first argument passed to [f] is the index, the second argument
      is the element of [a] and the third argument is the element of [b].
      The behaviour is unspecified if the slices are not the same length. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f s] applies function [f] to all the elements of [s], and
      buids a slice with the results returned by [f].
    {[
assert (
  Slice.to_list (Slice.map (fun i -> i * 2) (Slice.sub slice6 2 4))
    = [4; 6; 8; 10]);
    ]} *)

  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  (** [mapi f s] applies function [f] to all the elements of [s], and
      buids a slice with the results returned by [f].
      The first argument passed to [f] is the index, and the second
      argument is the element itself.
    {[
assert (
  Slice.to_list (Slice.mapi (fun i j -> (i, j)) (Slice.sub slice6 1 5))
    = [0, 1; 1, 2; 2, 3; 3, 4; 4, 5]);
  ]} *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [map2 f a b] checks that [a] and [b] are the same length,
      and applies function [f] to all the elements of [a] and [b],
      and buids a slice with the results returned by [f].
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      Raise [Invalid_argument] if the slices are not the same length: in this
      case, the function [f] is never called.
    {[
assert (
  Slice.to_list (Slice.map2 (fun i j -> (i, j))
    (Slice.sub slice6 2 4)
    (Slice.sub (Slice.init 6 (fun i -> i mod 3 = 0)) 1 4))
    = [2, false; 3, false; 4, true; 5, false]);
assert (
  try
    ignore (Slice.map2 (fun i j -> ())
      (Slice.sub slice6 1 4) (Slice.sub slice6 1 3));
    false
  with Invalid_argument _ ->
    true);
  ]} *)

  val map2_opt : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t option
  (** [map2_opt f a b] checks that [a] and [b] are the same length,
      and applies function [f] to all the elements of [a] and [b],
      and buids a slice with the results returned by [f].
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      Return [None] if the slices are not the same length: in this
      case, the function [f] is never called.
    {[
assert (
  match Slice.map2_opt (fun i j -> (i, j))
    (Slice.sub slice6 2 4)
    (Slice.sub (Slice.init 6 (fun i -> i mod 3 = 0)) 1 4) with
  | None -> false
  | Some slice ->
      Slice.to_list slice = [2, false; 3, false; 4, true; 5, false]);
assert (
  Slice.map2_opt (fun i j -> ())
    (Slice.sub slice6 1 4) (Slice.sub slice6 1 3) = None);
    ]} *)

  val unsafe_map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [unsafe_map2 f a b] applies function [f] to all the elements of [a] and
      [b], assuming that [a] and [b] are the same length,
      and buids a slice with the results returned by [f].
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      The behaviour is unspecified if the slices are not the same length. *)

  val map2i : (int -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [map2i f a b] checks that [a] and [b] are the same length,
      and applies function [f] to all the elements of [a] and [b],
      and buids a slice with the results returned by [f].
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      Raise [Invalid_argument] if the slices are not the same length: in this
      case, the function [f] is never called.
    {[
assert (
  Slice.to_list (Slice.map2i (fun i j k -> (i, j, k))
    (Slice.sub slice6 3 3) (Slice.sub slice6 2 3)) =
    [0, 3, 2; 1, 4, 3; 2, 5, 4]);
assert (
  try
    ignore (Slice.map2i (fun i j k -> ())
      (Slice.sub slice6 0 3) (Slice.sub slice6 1 2));
    false
  with Invalid_argument _ ->
    true);
    ]} *)

  val map2i_opt : (int -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t option
  (** [map2i_opt f a b] checks that [a] and [b] are the same length,
      and applies function [f] to all the elements of [a] and [b],
      and buids a slice with the results returned by [f].
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      Return [None] if the slices are not the same length: in this
      case, the function [f] is never called.
    {[
assert (
  match Slice.map2i_opt (fun i j k -> (i, j, k))
    (Slice.sub slice6 3 3) (Slice.sub slice6 2 3) with
  | None -> false
  | Some slice -> Slice.to_list slice = [0, 3, 2; 1, 4, 3; 2, 5, 4]);
assert (
  Slice.map2i_opt (fun i j k -> ())
    (Slice.sub slice6 0 3) (Slice.sub slice6 1 2) = None);
    ]} *)

  val unsafe_map2i : (int -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [unsafe_map2i f a b] applies function [f] to all the elements of [a] and
      [b], assuming that [a] and [b] are the same length,
      and buids a slice with the results returned by [f].
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      The behaviour is unspecified if the slices are not the same length. *)

  val filter : ('a -> bool) -> 'a t -> 'a t
  (** [filter p s] applies predicate [p] to all the elements of [a] and
      returns a slice that contains all the elements that satisfies [p].
      If [s] is immutable and
      if all the elements of [s] satisfies [p], then [s] is returned itself,
      that is [filter p s == s].
      If [s] is immutable and
      if all the elements of [s] that satisfies [p] are contiguous, then
      [filter p s] is a subslice of [s] and the elements of [s] are not
      copied.
      Otherwise, the elements of [s] are copied and a fresh slice is
      returned.
      In particular, if [s] is mutable, the elements are always copied, and
      [filter s == s] if and only if [s] is empty.
      Use {!val:MutGenS.filter_preserve} to get the same
      behaviour than in the immutable case.
    {[
let slice12 = Slice.init 12 (fun i -> i) in
let slice_1_to_10 = Slice.sub slice12 1 10 in
assert (Slice.filter (fun _ -> true) slice_1_to_10 == slice_1_to_10);
assert (Slice.unsafe_to_array (Slice.concat [
  Slice.filter (fun x -> x < 1) slice12;
  Slice.filter (fun x -> x < 5) slice_1_to_10;
  Slice.filter (fun x -> x >= 5) slice_1_to_10;
  Slice.filter (fun x -> x > 10) slice12; ])
  == Slice.unsafe_to_array slice12);
assert (Slice.to_list (Slice.filter (fun x -> x mod 3 >= 1) slice_1_to_10)
  = [1; 2; 4; 5; 7; 8; 10]);
    ]} *)

  val filteri : (int -> 'a -> bool) -> 'a t -> 'a t
  (** [filteri p s] applies predicate [p] to all the elements of [a] and
      returns a slice that contains all the elements that satisfies [p].
      The first argument passed to [p] is the index, the second argument
      is the element of [s] itself.
      If [s] is immutable and
      if all the elements of [s] satisfies [p], then [s] is returned itself,
      that is [filter p s == s].
      If [s] is immutable and
      if all the elements of [s] that satisfies [p] are contiguous, then
      [filter p s] is a subslice of [s] and the elements of [s] are not
      copied.
      Otherwise, the elements of [s] are copied and a fresh slice is
      returned.
      In particular, if [s] is mutable, the elements are always copied, and
      [filter s == s] if and only if [s] is empty.
      Use {!val:MutGenS.filteri_preserve} to get the same
      behaviour than in the immutable case.
    {[
assert (
  let q = Queue.create () in
  Slice.length
    (Slice.filteri (fun i x -> Queue.add i q; x <= 6) slice_1_to_10) = 6
  && List.of_seq (Queue.to_seq q) = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]);
    ]} *)

  val filter_left : ('a -> bool) -> 'a t -> 'a t
  (** [filter_left p s] returns the longest suffix of [s] beginning with
      an element that satisfies [p].
      If the first element of [s] satisfies [p], then [s] is returned itself,
      that is [filter_left p s == s].
      [filter_left p s] is always a subslice of [s] and the elements of [s] are
      never copied.
    {[
assert (Slice.filter_left (fun x -> x = 1) slice_1_to_10 == slice_1_to_10);
assert (Slice.unsafe_to_array (Slice.concat [
  Slice.sub slice12 0 5;
  Slice.filter_left (fun x -> x mod 5 = 0) slice_1_to_10;
  Slice.sub slice12 11 1; ])
  == Slice.unsafe_to_array slice12);
    ]} *)

  val filter_lefti : (int -> 'a -> bool) -> 'a t -> 'a t
  (** [filter_lefti p s] returns the longest suffix of [s] beginning with
      an element that satisfies [p].
      The first argument passed to [p] is the index, the second argument
      is the element of [s] itself.
      If the first element of [s] satisfies [p], then [s] is returned itself,
      that is [filter_lefti p s == s].
      [filter_lefti p s] is always a subslice of [s] and the elements of [s] are
      never copied.
    {[
assert (
  let q = Queue.create () in
  Slice.length
    (Slice.filter_lefti (fun i x -> Queue.add i q; x mod 5 = 0) slice_1_to_10)
      = 6
  && List.of_seq (Queue.to_seq q) = [0; 1; 2; 3; 4]);
    ]} *)

  val filter_right : ('a -> bool) -> 'a t -> 'a t
  (** [filter_right p s] returns the longest prefix of [s] ending with
      an element that satisfies [p].
      If the last element of [s] satisfies [p], then [s] is returned itself,
      that is [filter_right p s == s].
      [filter_right p s] is always a subslice of [s] and the elements of [s] are
      never copied.
    {[
assert (Slice.filter_right (fun x -> x = 10) slice_1_to_10 == slice_1_to_10);
assert (Slice.unsafe_to_array (Slice.concat [
  Slice.sub slice12 0 1;
  Slice.filter_right (fun x -> x mod 4 = 0) slice_1_to_10;
  Slice.sub slice12 9 3; ])
  == Slice.unsafe_to_array slice12);
    ]} *)

  val filter_righti : (int -> 'a -> bool) -> 'a t -> 'a t
  (** [filter_righti p s] returns the longest prefix of [s] ending with
      an element that satisfies [p].
      The first argument passed to [p] is the index, the second argument
      is the element of [s] itself.
      If the last element of [s] satisfies [p], then [s] is returned itself,
      that is [filter_righti p s == s].
      [filter_righti p s] is always a subslice of [s] and the elements of [s]
      are never copied.
    {[
assert (
  let q = Queue.create () in
  Slice.length
    (Slice.filter_righti (fun i x -> Queue.add i q; x mod 4 = 0) slice_1_to_10)
      = 8
  && List.of_seq (Queue.to_seq q) = [9; 8; 7]);
    ]} *)

  val split : ('a -> bool) -> 'a t -> 'a t list
  (** [split sep s] returns the list of all (possibly empty) subslices of [s]
      that are delimited by elements satisfying the predicate [sep].
      The function returns as many subslices as there are elements satisfying
      [sep] in [s] plus one: in particular, the list is not empty.
      No subslice in the result contains an element satisfying the predicate
      [sep].
    {[
assert (
  match Slice.split (fun _ -> false) slice_1_to_10 with
  | [slice] -> slice == slice_1_to_10
  | _ -> false);
assert (
  Slice.split (fun _ -> true) slice_1_to_10 =
    Array.to_list (Array.make 11 Slice.empty));
assert (
  match Slice.split (fun x -> x mod 4 = 0) slice_1_to_10 with
  | [s_1_to_3; s_5_to_7; s_9_to_10] ->
    Slice.unsafe_to_array (
      Slice.concat [Slice.sub slice12 0 1; s_1_to_3; Slice.sub slice12 4 1;
        s_5_to_7; Slice.sub slice12 8 1; s_9_to_10; Slice.sub slice12 11 1])
      == Slice.unsafe_to_array slice12
  | _ -> false);
    ]} *)

  val spliti : (int -> 'a -> bool) -> 'a t -> 'a t list
  (** [spliti sep s] returns the list of all (possibly empty) subslices of [s]
      that are delimited by elements satisfying the predicate [sep].
      The first argument passed to [p] is the index, the second argument
      is the element of [s] itself.
      The function returns as many subslices as there are elements satisfying
      [sep] in [s] plus one: in particular, the list is not empty.
      No subslice in the result contains an element satisfying the predicate
      [sep].
    {[
assert (
  let q = Queue.create () in
  List.length
    (Slice.spliti (fun i x -> Queue.add i q; x mod 3 = 0) slice_1_to_10) = 4
  && List.of_seq (Queue.to_seq q) = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]);
    ]} *)

  val split_left : ('a -> bool) -> 'a t -> 'a t * 'a t
  (** [split_left sep s] finds the first element [e] of [s] satisfying [sep] and
      returns the pair [(l, r)] where [l] is the prefix of [s] preceding [e] and
      [r] is the suffix of [s] succeeding [e]. [e] is not included, neither in
      [l] nor in [r], and there is no element satisfying [sep] in [l].
      The result [(l, r)] satisfies the following invariant:
      [s = concat [l; sub s (length l) 1; r]].
      Raise [Not_found] if there is no element in [s] satisfying [sep].
    {[
assert (
  let l, r = Slice.split_left (fun x -> x mod 4 = 0) slice_1_to_10 in
  Slice.length l = 3 &&
  Slice.unsafe_to_array (
    Slice.concat [Slice.sub slice12 0 1; l; Slice.sub slice12 4 1; r;
      Slice.sub slice12 11 1])
    == Slice.unsafe_to_array slice12);
assert (
  try
    ignore (Slice.split_left (fun x -> x = 0) slice_1_to_10);
    false
  with Not_found ->
    true);
    ]} *)

  val split_lefti : (int -> 'a -> bool) -> 'a t -> 'a t * 'a t
  (** [split_lefti sep s] finds the first element [e] of [s] satisfying [sep]
      and returns the pair [(l, r)] where [l] is the prefix of [s] preceding [e]
      and [r] is the suffix of [s] succeeding [e].
      The first argument passed to [p] is the index, the second argument
      is the element of [s] itself.
      [e] is not included, neither in [l] nor in [r], and there is no element
      satisfying [sep] in [l].
      The result [(l, r)] satisfies the following invariant:
      [s = concat [l; sub s (length l) 1; r]].
      Raise [Not_found] if there is no element in [s] satisfying [sep].
    {[
assert (
  let q = Queue.create () in
  let l, r =
    Slice.split_lefti (fun i x -> Queue.add i q; x mod 3 = 0) slice_1_to_10 in
  Slice.length l = 2 &&
  Slice.unsafe_to_array (
    Slice.concat [Slice.sub slice12 0 1; l; Slice.sub slice12 3 1; r;
      Slice.sub slice12 11 1])
    == Slice.unsafe_to_array slice12 &&
  List.of_seq (Queue.to_seq q) = [0; 1; 2]);
assert (
  let q = Queue.create () in
  (try
    ignore (Slice.split_lefti (fun i x -> Queue.add i q; x = 0) slice_1_to_10);
    false
  with Not_found ->
    true) &&
  List.of_seq (Queue.to_seq q) = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]);
    ]} *)

  val split_left_opt : ('a -> bool) -> 'a t -> ('a t * 'a t) option
  (** [split_left_opt sep s] finds the first element [e] of [s] satisfying [sep]
      and returns [Some (l, r)] where [l] is the prefix of [s] preceding [e]
      and [r] is the suffix of [s] succeeding [e]. [e] is not included, neither
      in [l] nor in [r], and there is no element satisfying [sep] in [l].
      The result [Some (l, r)] satisfies the following invariant:
      [s = concat [l; sub s (length l) 1; r]].
      Return [None] if there is no element in [s] satisfying [sep].
    {[
assert (
  match Slice.split_left_opt (fun x -> x mod 4 = 0) slice_1_to_10 with
  | None -> false
  | Some (l, r) ->
      Slice.length l = 3 &&
      Slice.unsafe_to_array (
        Slice.concat [Slice.sub slice12 0 1; l; Slice.sub slice12 4 1; r;
          Slice.sub slice12 11 1])
        == Slice.unsafe_to_array slice12);
assert (Slice.split_left_opt (fun x -> x = 0) slice_1_to_10 == None);
    ]} *)

  val split_lefti_opt : (int -> 'a -> bool) -> 'a t -> ('a t * 'a t) option
  (** [split_lefti_opt sep s] finds the first element [e] of [s] satisfying
      [sep] and returns [Some (l, r)] where [l] is the prefix of [s]
      preceding [e] and [r] is the suffix of [s] succeeding [e].
      The first argument passed to [p] is the index, the second argument
      is the element of [s] itself.
      [e] is not included, neither in [l] nor in [r], and there is no element
      satisfying [sep] in [l].
      The result [Some (l, r)] satisfies the following invariant:
      [s = concat [l; sub s (length l) 1; r]].
      Return [None] if there is no element in [s] satisfying [sep].
    {[
assert (
  let q = Queue.create () in
  match Slice.split_lefti_opt (fun i x -> Queue.add i q; x mod 3 = 0)
    slice_1_to_10 with
  | None -> false
  | Some (l, r) ->
      Slice.length l = 2 &&
      Slice.unsafe_to_array (
        Slice.concat [Slice.sub slice12 0 1; l; Slice.sub slice12 3 1; r;
          Slice.sub slice12 11 1])
        == Slice.unsafe_to_array slice12 &&
      List.of_seq (Queue.to_seq q) = [0; 1; 2]);
assert (
  let q = Queue.create () in
  Slice.split_lefti_opt (fun i x -> Queue.add i q; x = 0) slice_1_to_10 = None
  && List.of_seq (Queue.to_seq q) = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]);
    ]} *)

  val split_right : ('a -> bool) -> 'a t -> 'a t * 'a t
  (** [split_right sep s] finds the last element [e] of [s] satisfying [sep] and
      returns the pair [(l, r)] where [l] is the prefix of [s] preceding [e] and
      [r] is the suffix of [s] succeeding [e]. [e] is not included, neither in
      [l] nor in [r], and there is no element satisfying [sep] in [r].
      The result [(l, r)] satisfies the following invariant:
      [s = concat [l; sub s (length l) 1; r]].
      Raise [Not_found] if there is no element in [s] satisfying [sep].
    {[
assert (
  let l, r = Slice.split_right (fun x -> x mod 4 = 0) slice_1_to_10 in
  Slice.length l = 7 &&
  Slice.unsafe_to_array (
    Slice.concat [Slice.sub slice12 0 1; l; Slice.sub slice12 8 1; r;
      Slice.sub slice12 11 1])
    == Slice.unsafe_to_array slice12);
assert (
  try
    ignore (Slice.split_right (fun x -> x = 0) slice_1_to_10);
    false
  with Not_found ->
    true);
    ]} *)

  val split_righti : (int -> 'a -> bool) -> 'a t -> 'a t * 'a t
  (** [split_righti sep s] finds the last element [e] of [s] satisfying [sep]
      and returns the pair [(l, r)] where [l] is the prefix of [s] preceding [e]
      and [r] is the suffix of [s] succeeding [e].
      The first argument passed to [p] is the index, the second argument
      is the element of [s] itself.
      [e] is not included, neither in [l] nor in [r], and there is no element
      satisfying [sep] in [r].
      The result [(l, r)] satisfies the following invariant:
      [s = concat [l; sub s (length l) 1; r]].
      Raise [Not_found] if there is no element in [s] satisfying [sep].
    {[
assert (
  let q = Queue.create () in
  let l, r =
    Slice.split_righti (fun i x -> Queue.add i q; x mod 3 = 0) slice_1_to_10 in
  Slice.length l = 8 &&
  Slice.unsafe_to_array (
    Slice.concat [Slice.sub slice12 0 1; l; Slice.sub slice12 9 1; r;
      Slice.sub slice12 11 1])
    == Slice.unsafe_to_array slice12 &&
  List.of_seq (Queue.to_seq q) = [9; 8]);
assert (
  let q = Queue.create () in
  (try
    ignore (Slice.split_righti (fun i x -> Queue.add i q; x = 0) slice_1_to_10);
    false
  with Not_found ->
    true) &&
  List.of_seq (Queue.to_seq q) = [9; 8; 7; 6; 5; 4; 3; 2; 1; 0]);
    ]} *)

  val split_right_opt : ('a -> bool) -> 'a t -> ('a t * 'a t) option
  (** [split_right_opt sep s] finds the last element [e] of [s] satisfying
      [sep] and returns [Some (l, r)] where [l] is the prefix of [s] preceding
      [e] and [r] is the suffix of [s] succeeding [e]. [e] is not included,
      neither in [l] nor in [r], and there is no element satisfying [sep] in
      [r].
      The result [Some (l, r)] satisfies the following invariant:
      [s = concat [l; sub s (length l) 1; r]].
      Return [None] if there is no element in [s] satisfying [sep].
    {[
assert (
  match Slice.split_right_opt (fun x -> x mod 4 = 0) slice_1_to_10 with
  | None -> false
  | Some (l, r) ->
      Slice.length l = 7 &&
      Slice.unsafe_to_array (
        Slice.concat [Slice.sub slice12 0 1; l; Slice.sub slice12 8 1; r;
          Slice.sub slice12 11 1])
        == Slice.unsafe_to_array slice12);
assert (Slice.split_right_opt (fun x -> x = 0) slice_1_to_10 == None);
    ]} *)

  val split_righti_opt : (int -> 'a -> bool) -> 'a t -> ('a t * 'a t) option
  (** [split_righti_opt sep s] finds the last element [e] of [s] satisfying
      [sep] and returns [Some (l, r)] where [l] is the prefix of [s]
      preceding [e] and [r] is the suffix of [s] succeeding [e].
      The first argument passed to [p] is the index, the second argument
      is the element of [s] itself.
      [e] is not included, neither in [l] nor in [r], and there is no element
      satisfying [sep] in [r].
      The result [Some (l, r)] satisfies the following invariant:
      [s = concat [l; sub s (length l) 1; r]].
      Return [None] if there is no element in [s] satisfying [sep].
    {[
assert (
  let q = Queue.create () in
  match Slice.split_righti_opt (fun i x -> Queue.add i q; x mod 3 = 0)
    slice_1_to_10 with
  | None -> false
  | Some (l, r) ->
      Slice.length l = 8 &&
      Slice.unsafe_to_array (
        Slice.concat [Slice.sub slice12 0 1; l; Slice.sub slice12 9 1; r;
          Slice.sub slice12 11 1])
        == Slice.unsafe_to_array slice12 &&
      List.of_seq (Queue.to_seq q) = [9; 8]);
assert (
  let q = Queue.create () in
  Slice.split_righti_opt (fun i x -> Queue.add i q; x = 0) slice_1_to_10 = None
  && List.of_seq (Queue.to_seq q) = [9; 8; 7; 6; 5; 4; 3; 2; 1; 0]);
    ]} *)

  val split_at : int -> 'a t -> 'a t * 'a t
  (** [split_at i s] returns the pair [(l, r)] where [l] is the prefix of
      [s] of length [i], and [r] is the suffix such that [append l r = s].
      Raise [Invalid_argument "index out of bounds"]
      if [i] is outside the range [0] to [Slice.length s].
    {[
assert (
  let l, r = Slice.split_at 0 slice_1_to_10 in
  l == Slice.empty && r == slice_1_to_10);
assert (
  let l, r = Slice.split_at 10 slice_1_to_10 in
  l == slice_1_to_10 && r == Slice.empty);
assert (
  let l, r = Slice.split_at 5 slice_1_to_10 in
  Slice.length l = 5 &&
  Slice.unsafe_to_array (
    Slice.concat [Slice.sub slice12 0 1; l; r; Slice.sub slice12 11 1])
    == Slice.unsafe_to_array slice12);
assert (
  try
    ignore (Slice.split_at 11 slice_1_to_10);
    false
  with Invalid_argument _ ->
    true);
    ]}
 *)

  val split_at_opt : int -> 'a t -> ('a t * 'a t) option
  (** [split_at_opt i s] returns [Some (l, r)] where [l] is the prefix of
      [s] of length [i], and [r] is the suffix such that [append l r = s].
      Return [None]
      if [i] is outside the range [0] to [Slice.length s].
    {[
assert (
  match Slice.split_at_opt 0 slice_1_to_10 with
  | None -> false
  | Some (l, r) -> l == Slice.empty && r == slice_1_to_10);
assert (
  match Slice.split_at_opt 10 slice_1_to_10 with
  | None -> false
  | Some (l, r) -> l == slice_1_to_10 && r == Slice.empty);
assert (
  match Slice.split_at_opt 5 slice_1_to_10 with
  | None -> false
  | Some (l, r) ->
      Slice.length l = 5 &&
      Slice.unsafe_to_array (
        Slice.concat [Slice.sub slice12 0 1; l; r; Slice.sub slice12 11 1])
        == Slice.unsafe_to_array slice12);
assert (Slice.split_at_opt 11 slice_1_to_10 = None);
    ]} *)

  val unsafe_split_at : int -> 'a t -> 'a t * 'a t
  (** [unsafe_split_at i s] returns the pair [(l, r)] where [l] is the prefix of
      [s] of length [i], and [r] is the suffix such that [append l r = s].
      The behavior is not specified
      if [i] is outside the range [0] to [Slice.length s]. *)

  val mem : 'a -> 'a t -> bool
  (** [mem x s] returns [true] if there exists an element of [s] which is
      structurally equal to [x].
    {[
assert (Slice.mem 2 (Slice.sub slice6 1 3));
assert (Slice.mem (2, 2)
   (Slice.sub (Slice.mapi (fun i j -> (i, j)) slice6) 1 3));
assert (not (Slice.mem 5 (Slice.sub slice6 1 3)));
    ]} *)

  val memq : 'a -> 'a t -> bool
  (** [mem x s] returns [true] if there exists an element of [s] which is
      physically equal to [x].
    {[
assert (Slice.memq 2 (Slice.sub slice6 1 3));
assert (not (Slice.memq (2, 2)
   (Slice.sub (Slice.mapi (fun i j -> (i, j)) slice6) 1 3)));
assert (not (Slice.memq 5 (Slice.sub slice6 1 3)));
    ]} *)

  val for_all : ('a -> bool) -> 'a t -> bool
  (** [for_all p s] checks if all elements of the slice satisfy the predicate
      [p]. That is, the function returns [true] if [p] returns [true] for all
      the elements of [s]; otherwise, [false] is returned as soon as [p]
      returns [false], and [p] is not called with the remaining elements.
    {[
assert (
  let q = Queue.create () in
  Slice.for_all (fun i -> Queue.add i q; i >= 2 && i <= 5)
    (Slice.sub slice6 2 4) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) = [2; 3; 4; 5]);
assert (
  let q = Queue.create () in
  not (Slice.for_all (fun i -> Queue.add i q; i >= 2 && i < 4)
    (Slice.sub slice6 2 4)) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) = [2; 3; 4]);
    ]} *)

  val for_alli : (int -> 'a -> bool) -> 'a t -> bool
  (** [for_alli p s] checks if all elements of the slice satisfy the predicate
      [p]. That is, the function returns [true] if [p] returns [true] for all
      the elements of [s]; otherwise, [false] is returned as soon as [p]
      returns [false], and [p] is not called with the remaining elements.
      The first argument passed to [f] is the index, the second argument
      is the element itself.
    {[
assert (
  let q = Queue.create () in
  Slice.for_alli (fun i j -> Queue.add (i, j) q; i < j)
    (Slice.sub slice6 1 5) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1; 1, 2; 2, 3; 3, 4; 4, 5]);
assert (
  let q = Queue.create () in
  not (Slice.for_alli (fun i j -> Queue.add (i, j) q; i < 3)
    (Slice.sub slice6 1 5)) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1; 1, 2; 2, 3; 3, 4]);
    ]} *)

  val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** [for_all2 p a b] checks that [a] and [b] are the same length,
      and checks if all elements of the slices satisfy the predicate
      [p]. That is, the function returns [true] if [p] returns [true] for all
      the elements of [a] and [b]; otherwise, [false] is returned as soon as [p]
      returns [false], and [p] is not called with the remaining elements.
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      Raise [Invalid_argument] if the slices are not the same length: in this
      case, the function [p] is never called.
    {[
assert (
  let q = Queue.create () in
  Slice.for_all2 (fun i j -> Queue.add (i, j) q; i < j)
    (Slice.sub slice6 1 4) (Slice.sub slice6 2 4) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1, 2; 2, 3; 3, 4; 4, 5]);
assert (
  let q = Queue.create () in
  not (Slice.for_all2 (fun i j -> Queue.add (i, j) q; i < 3)
    (Slice.sub slice6 1 5) (Slice.sub slice6 0 5)) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1, 0; 2, 1; 3, 2]);
assert (
  try
    ignore (Slice.for_all2 (fun i j -> false)
      (Slice.sub slice6 0 3) (Slice.sub slice6 1 2));
    false
  with Invalid_argument _ ->
    true);
    ]} *)

  val for_all2_opt : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool option
  (** [for_all2_opt p a b] checks that [a] and [b] are the same length,
      and checks if all elements of the slice satisfy the predicate
      [p]. That is, the function returns [Some true] if [p] returns [true] for
      all the elements of [a] and [b];
      otherwise, [Some false] is returned as soon as
      [p] returns [false], and [p] is not called with the remaining elements.
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      Return [None] if the slices are not the same length: in this
      case, the function [p] is never called.
    {[
assert (
  let q = Queue.create () in
  Slice.for_all2_opt (fun i j -> Queue.add (i, j) q; i < j)
    (Slice.sub slice6 1 4) (Slice.sub slice6 2 4) = Some true &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1, 2; 2, 3; 3, 4; 4, 5]);
assert (
  let q = Queue.create () in
  Slice.for_all2_opt (fun i j -> Queue.add (i, j) q; i < 3)
    (Slice.sub slice6 1 5) (Slice.sub slice6 0 5) = Some false &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1, 0; 2, 1; 3, 2]);
assert (
  Slice.for_all2_opt (fun i j -> false)
    (Slice.sub slice6 0 3) (Slice.sub slice6 1 2) = None);
    ]} *)

  val unsafe_for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** [unsafe_for_all2 p a b] checks that [a] and [b] are the same length,
      and checks if all elements of the slices satisfy the predicate
      [p]. That is, the function returns [true] if [p] returns [true] for all
      the elements of [a] and [b]; otherwise, [false] is returned as soon as [p]
      returns [false], and [p] is not called with the remaining elements.
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      The behaviour is unspecified if the slices are not the same length. *)

  val for_all2i : (int -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** [for_all2i p a b] checks that [a] and [b] are the same length,
      and checks if all elements of the slice satisfy the predicate
      [p]. That is, the function returns [true] if [p] returns [true] for all
      the elements of [a] and [b]; otherwise, [false] is returned as soon as [p]
      returns [false], and [p] is not called with the remaining elements.
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      Raise [Invalid_argument] if the slices are not the same length: in this
      case, the function [p] is never called.
    {[
assert (
  let q = Queue.create () in
  Slice.for_all2i (fun i j k -> Queue.add (i, j, k) q; i < j)
    (Slice.sub slice6 1 4) (Slice.sub slice6 2 4) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1, 2; 1, 2, 3; 2, 3, 4; 3, 4, 5]);
assert (
  let q = Queue.create () in
  not (Slice.for_all2i (fun i j k -> Queue.add (i, j, k) q; i < 3)
    (Slice.sub slice6 1 5) (Slice.sub slice6 0 5)) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1, 0; 1, 2, 1; 2, 3, 2; 3, 4, 3]);
assert (
  try
    ignore (Slice.for_all2i (fun i j k -> false)
      (Slice.sub slice6 0 3) (Slice.sub slice6 1 2));
    false
  with Invalid_argument _ ->
    true);
    ]} *)

  val for_all2i_opt : (int -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool option
  (** [for_all2i_opt p a b] checks that [a] and [b] are the same length,
      and checks if all elements of the slice satisfy the predicate
      [p]. That is, the function returns [Some true] if [p] returns [true] for
      all the elements of [a] and [b]; otherwise, [Some false] is returned as
      soon as [p] returns [false], and [p] is not called with the remaining
      elements.
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      Return [None] if the slices are not the same length: in this
      case, the function [p] is never called.
    {[
assert (
  let q = Queue.create () in
  Slice.for_all2i_opt (fun i j k -> Queue.add (i, j, k) q; i < j)
    (Slice.sub slice6 1 4) (Slice.sub slice6 2 4) = Some true &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1, 2; 1, 2, 3; 2, 3, 4; 3, 4, 5]);
assert (
  let q = Queue.create () in
  Slice.for_all2i_opt (fun i j k -> Queue.add (i, j, k) q; i < 3)
    (Slice.sub slice6 1 5) (Slice.sub slice6 0 5) = Some false &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1, 0; 1, 2, 1; 2, 3, 2; 3, 4, 3]);
assert (
  Slice.for_all2i_opt (fun i j k -> false)
    (Slice.sub slice6 0 3) (Slice.sub slice6 1 2) = None);
    ]} *)

  val unsafe_for_all2i : (int -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** [unsafe_for_all2i p a b] checks that [a] and [b] are the same length,
      and checks if all elements of the slice satisfy the predicate
      [p]. That is, the function returns [true] if [p] returns [true] for all
      the elements of [a] and [b]; otherwise, [false] is returned as soon as [p]
      returns [false], and [p] is not called with the remaining elements.
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      The behaviour is unspecified if the slices are not the same length. *)

  val exists : ('a -> bool) -> 'a t -> bool
  (** [exists p s] checks if at least one element of the slice satisfies the
      predicate [p].
      That is, the function returns [true] as soon as [p] returns [true] for one
      element of [s], and [p] is not called with the remaining elements;
      otherwise, [false] is returned since [p] returns [false] for all elements
      of [s].
    {[
assert (
  let q = Queue.create () in
  Slice.exists (fun i -> Queue.add i q; i = 4)
    (Slice.sub slice6 2 4) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) = [2; 3; 4]);
assert (
  let q = Queue.create () in
  not (Slice.exists (fun i -> Queue.add i q; i = 6)
    (Slice.sub slice6 2 4)) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) = [2; 3; 4; 5]);
    ]} *)

  val existsi : (int -> 'a -> bool) -> 'a t -> bool
  (** [existsi p s] checks if at least one element of the slice satisfies the
      predicate [p].
      That is, the function returns [true] as soon as [p] returns [true] for one
      element of [s], and [p] is not called with the remaining elements;
      otherwise, [false] is returned since [p] returns [false] for all elements
      of [s].
      The first argument passed to [f] is the index, the second argument
      is the element itself.
    {[
assert (
  let q = Queue.create () in
  Slice.existsi (fun i j -> Queue.add (i, j) q; i = 3)
    (Slice.sub slice6 1 5) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1; 1, 2; 2, 3; 3, 4]);
assert (
  let q = Queue.create () in
  not (Slice.existsi (fun i j -> Queue.add (i, j) q; i = -1)
    (Slice.sub slice6 1 5)) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1; 1, 2; 2, 3; 3, 4; 4, 5]);
    ]} *)

  val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** [exists2 p a b] checks if at least one index for which the element of [a]
      and the element of [b] at this same index satisfies the
      predicate [p].
      That is, the function returns [true] as soon as [p] returns [true] for one
      such pair of elements, and [p] is not called with the remaining elements;
      otherwise, [false] is returned since [p] returns [false] for all pairs of
      elements.
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      Raise [Invalid_argument] if the slices are not the same length: in this
      case, the function [p] is never called.
    {[
assert (
  let q = Queue.create () in
  Slice.exists2 (fun i j -> Queue.add (i, j) q; i = 3)
    (Slice.sub slice6 1 4) (Slice.sub slice6 2 4) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1, 2; 2, 3; 3, 4]);
assert (
  let q = Queue.create () in
  not (Slice.exists2 (fun i j -> Queue.add (i, j) q; i < j)
    (Slice.sub slice6 1 5) (Slice.sub slice6 0 5)) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1, 0; 2, 1; 3, 2; 4, 3; 5, 4]);
assert (
  try
    ignore (Slice.exists2 (fun i j -> false)
      (Slice.sub slice6 0 3) (Slice.sub slice6 1 2));
    false
  with Invalid_argument _ ->
    true);
    ]} *)

  val exists2_opt : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool option
  (** [exists2_opt p a b] checks that [a] and [b] are the same length,
      and checks if at least one index for which the element of [a]
      and the element of [b] at this same index satisfies the
      predicate [p].
      That is, the function returns [Some true] as soon as [p] returns [true]
      for one such pair of elements, and [p] is not called with the remaining
      elements;
      otherwise, [Some false] is returned since [p] returns [false] for all
      pairs of elements.
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      Return [None] if the slices are not the same length: in this
      case, the function [p] is never called.
    {[
assert (
  let q = Queue.create () in
  Slice.exists2_opt (fun i j -> Queue.add (i, j) q; i = 3)
    (Slice.sub slice6 1 4) (Slice.sub slice6 2 4) = Some true &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1, 2; 2, 3; 3, 4]);
assert (
  let q = Queue.create () in
  Slice.exists2_opt (fun i j -> Queue.add (i, j) q; i < j)
    (Slice.sub slice6 1 5) (Slice.sub slice6 0 5) = Some false &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [1, 0; 2, 1; 3, 2; 4, 3; 5, 4]);
assert (
  Slice.exists2_opt (fun i j -> false)
    (Slice.sub slice6 0 3) (Slice.sub slice6 1 2) = None);
    ]} *)

  val unsafe_exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** [unsafe_exists2 p a b] checks if at least one index for which the element
      of [a] and the element of [b] at this same index satisfies the
      predicate [p].
      That is, the function returns [true] as soon as [p] returns [true] for one
      such pair of elements, and [p] is not called with the remaining elements;
      otherwise, [false] is returned since [p] returns [false] for all pairs of
      elements.
      The first argument passed to [f] is the element of [a], and the second
      argument is the element of [b].
      The behaviour is unspecified if the slices are not the same length. *)

  val exists2i : (int -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** [exists2i p a b] checks if at least one index for which the element of [a]
      and the element of [b] at this same index satisfies the
      predicate [p].
      That is, the function returns [true] as soon as [p] returns [true] for one
      such pair of elements, and [p] is not called with the remaining elements;
      otherwise, [false] is returned since [p] returns [false] for all pairs of
      elements.
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      Raise [Invalid_argument] if the slices are not the same length: in this
      case, the function [p] is never called.
    {[
assert (
  let q = Queue.create () in
  Slice.exists2i (fun i j k -> Queue.add (i, j, k) q; i = 2)
    (Slice.sub slice6 1 4) (Slice.sub slice6 2 4) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1, 2; 1, 2, 3; 2, 3, 4]);
assert (
  let q = Queue.create () in
  not (Slice.exists2i (fun i j k -> Queue.add (i, j, k) q; i > j)
    (Slice.sub slice6 1 5) (Slice.sub slice6 0 5)) &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1, 0; 1, 2, 1; 2, 3, 2; 3, 4, 3; 4, 5, 4]);
assert (
  try
    ignore (Slice.exists2i (fun i j k -> false)
      (Slice.sub slice6 0 3) (Slice.sub slice6 1 2));
    false
  with Invalid_argument _ ->
    true);
    ]} *)

  val exists2i_opt : (int -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool option
  (** [exists2i_opt p a b] checks that [a] and [b] are the same length,
      and checks if at least one index for which the element of [a]
      and the element of [b] at this same index satisfies the
      predicate [p].
      That is, the function returns [Some true] as soon as [p] returns [true]
      for one such pair of elements, and [p] is not called with the remaining
      elements;
      otherwise, [Some false] is returned since [p] returns [false] for all
      pairs of elements.
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      Return [None] if the slices are not the same length: in this
      case, the function [p] is never called.
    {[
assert (
  let q = Queue.create () in
  Slice.exists2i_opt (fun i j k -> Queue.add (i, j, k) q; i = 2)
    (Slice.sub slice6 1 4) (Slice.sub slice6 2 4) = Some true &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1, 2; 1, 2, 3; 2, 3, 4]);
assert (
  let q = Queue.create () in
  Slice.exists2i_opt (fun i j k -> Queue.add (i, j, k) q; i > j)
    (Slice.sub slice6 1 5) (Slice.sub slice6 0 5) = Some false &&
  Stdcompat.List.of_seq (Stdcompat.Queue.to_seq q) =
    [0, 1, 0; 1, 2, 1; 2, 3, 2; 3, 4, 3; 4, 5, 4]);
assert (
  Slice.exists2i_opt (fun i j k -> false)
    (Slice.sub slice6 0 3) (Slice.sub slice6 1 2) = None);
    ]} *)

  val unsafe_exists2i : (int -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** [unsafe_exists2i p a b] checks if at least one index for which the element
      of [a] and the element of [b] at this same index satisfies the
      predicate [p].
      That is, the function returns [true] as soon as [p] returns [true] for one
      such pair of elements, and [p] is not called with the remaining elements;
      otherwise, [false] is returned since [p] returns [false] for all pairs of
      elements.
      The first argument passed to [f] is the index, the second argument
      is the element of [a], and the third argument is the element of [b].
      The behaviour is unspecified if the slices are not the same length. *)

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold_left f init s] computes [f (...(f (f init e0) e1)...) en], where
      [e0], ..., [en] are the elements of [s], and [n] is [length s - 1].
    {[
assert (
  Slice.fold_left (fun accu i -> i :: accu) []
    (Slice.sub slice6 1 4) = [4; 3; 2; 1]);
    ]} *)

  val fold_lefti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold_lefti f init s] computes [f n (...(f 1 (f 0 init e0) e1)...) en],
      where [e0], ..., [en] are the elements of [s],
      and [n] is [length s - 1].
    {[
assert (
  Slice.fold_lefti (fun i accu j -> (i, j) :: accu) []
    (Slice.sub slice6 1 4) = [3, 4; 2, 3; 1, 2; 0, 1]);
    ]} *)

  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold_right f s init] computes [f e0 (f e1 (...(f en init)))], where
      [e0], ..., [en] are the elements of [s], and [n] is [length s - 1].
    {[
assert (
  Slice.fold_right (fun i accu -> i :: accu)
    (Slice.sub slice6 1 4) [] = [1; 2; 3; 4]);
    ]} *)

  val fold_righti : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold_righti f s init] computes [f 0 e0 (f 1 e1 (...(f n en init)))],
      where [e0], ..., [en] are the elements of [s],
      and [n] is [length s - 1].
    {[
assert (
  Slice.fold_righti (fun i j accu -> (i, j) :: accu)
    (Slice.sub slice6 1 4) [] = [0, 1; 1, 2; 2, 3; 3, 4]);
    ]} *)

  val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
  (** [compare cmp a b], given an order [cmp] over elements, compares
      the two slices [a] and [b] and provides an order over slices:
      that is to say, the function returns [0] is [a] equals [b],
      a strictly negative value if [a] is less than [b] and
      a strictly positive value if [a] is greater than [b].
      Slices are first ordered by increasing length, and [cmp] is called
      only if [a] and [b] are the same length. Moreover, [cmp] is called until
      finding a
      pair of elements for which the result is not null, and in this
      case the result of [cmp] is returned. [0] is returned if and only
      if the two slices are the same length and [cmp] returns [0] for
      every pair of elements. The order is therefore total if and only if
      [cmp] is a total order.
    {[
assert(
  Slice.compare compare (Slice.sub slice6 1 3) (Slice.sub slice6 2 2) > 0);
assert(
  Slice.compare compare (Slice.sub slice6 2 3) (Slice.sub slice6 1 4) < 0);
assert(
  Slice.compare compare (Slice.sub slice6 2 3) (Slice.sub slice6 3 3) < 0);
assert(
  Slice.compare compare (Slice.sub slice6 2 3) (Slice.sub slice6 0 3) > 0);
assert(
  Slice.compare compare (Slice.sub slice6 2 3) (Slice.sub slice6 2 3) = 0);
assert(
  Slice.compare (fun i j -> compare j i)
    (Slice.sub slice6 2 3) (Slice.sub slice6 0 3) < 0);
    ]} *)

  val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** [equal eq a b], given an equality predicate [eq] over elements,
      checks whether [a] and [b] are equal. Lengths are checked first, and
      [eq] is called only if [a] and [b] are the same length. Moreover, [eq] is
      called until finding a pair of elements which are not equal for [eq].
      [true] is returned if and only if the two slices are the same length
      and [eq] returns [true] for every pair of elements.
    {[
assert(
  not (Slice.equal ( = ) (Slice.sub slice6 1 3) (Slice.sub slice6 2 2)));
assert(
  not (Slice.equal ( = ) (Slice.sub slice6 1 3) (Slice.sub slice6 2 3)));
assert(
  Slice.equal ( = ) (Slice.sub slice6 1 3) (Slice.sub slice6 1 3));
assert(
  Slice.equal (fun i j -> i mod 2 = j mod 2)
    (Slice.sub slice6 1 3) (Slice.sub slice6 3 3));
    ]} *)

  val hash : ('a -> int) -> 'a t -> int
  (** [hash h s] returns a hash for [s], given an hash function [h] over
      elements.
    {[
assert (
  Slice.hash Hashtbl.hash (Slice.sub slice6 1 3) =
  Slice.hash Hashtbl.hash (Slice.init 3 succ));
assert (
  Slice.hash Hashtbl.hash (Slice.sub slice6 1 4) <>
  Slice.hash Hashtbl.hash (Slice.init 3 succ));
assert (
  Slice.hash Hashtbl.hash (Slice.sub slice6 1 5) <>
  Slice.hash Hashtbl.hash (Slice.init 5 pred));
    ]} *)

  val to_seq : 'a t -> 'a Stdcompat.Seq.t
  (** [to_seq s] iterates on the elements of [s].
      If the slice is mutable, modifications of the slice during iteration
      will be reflected in the iterator.
    {[
assert (
  List.of_seq (Slice.to_seq (Slice.sub slice6 1 4)) =
    [1; 2; 3; 4]);
    ]} *)

  val to_seqi : 'a t -> (int * 'a) Stdcompat.Seq.t
  (** [to_seqi s] iterates on the elements of [s], yielding indices as
      first components and the elements themselves as second components.
      If the slice is mutable, modifications of the slice during iteration
      will be reflected in the iterator.
    {[
assert (
  List.of_seq (Slice.to_seqi (Slice.sub slice6 1 4)) =
    [0, 1; 1, 2; 2, 3; 3, 4]);
    ]} *)

  val of_seq : 'a Stdcompat.Seq.t -> 'a t
  (** [of_seq s] creates a slice from the sequence [s].
    {[
assert (
  Slice.equal ( = ) (Slice.of_seq (List.to_seq [1; 2; 3]))
    (Slice.sub slice6 1 3));
    ]} *)

  val of_list : 'a list -> 'a t
  (** [of_list l] creates a slice from the list [l].
    {[
assert (
  Slice.equal ( = ) (Slice.of_list [2; 3; 4; 5])
    (Slice.sub slice6 2 4));
    ]} *)

  val to_list : 'a t -> 'a list
  (** [to_list s] returns the list of the elements of [s].
    {[
assert (Slice.to_list (Slice.sub slice6 1 5) = [1; 2; 3; 4; 5]);
    ]} *)

  val of_array : 'a array -> 'a t
  (** [of_array a] returns a slice covering all the elements of [a].
      Elements are copied if and only the slice is immutable.
      If the slice is mutable, elements are not copied
      and modifications are reflected between the array and the slice.
    {[
let array15 = [| 1; 2; 3; 4; 5 |] in
assert (
  Slice.equal ( = ) (Slice.of_array array15)
    (Slice.sub slice6 1 5));
    ]} *)

  val to_array : 'a t -> 'a array
  (** [to_array s] creates a fresh array with the elements of [s].
      If [s] is mutable and covers all the elements of an array, you may
      use {!MutGenS.to_array_preserve} to get the original array instead of
      copying the elements.
    {[
assert (Slice.to_array (Slice.of_array array15) = array15);
assert (Slice.to_array (Slice.of_array array15) != array15);
let a = [| 0; 1; 2; 3; 4; 5 |] in
assert (MutSlice.to_array (MutSlice.of_array a) != a);
    ]} *)

  val map_to_array : ('a -> 'b) -> 'a t -> 'b array
  (** [map_to_array f s] applies function [f] to all the elements of [s], and
      buids an array with the results returned by [f].
    {[
assert (
  Slice.map_to_array (fun i -> i * 2) (Slice.sub slice6 2 4)
    = [| 4; 6; 8; 10 |]);
    ]} *)
end

module type MutS = sig
  include S

  val create : int -> t

  val set : t -> int -> item -> unit

  val try_set : t -> int -> item -> bool

  val unsafe_set : t -> int -> item -> unit

  val copy : t -> t

  val recopy : src:t -> tgt:t -> unit

  val unsafe_recopy : src:t -> tgt:t -> unit

  val try_recopy : src:t -> tgt:t -> bool

  val append_preserve : t -> t -> t

  val concat_preserve : t list -> t

  val map_preserve : (item -> item) -> t -> t

  val mapi_preserve : (int -> item -> item) -> t -> t

  val filter_preserve : (item -> bool) -> t -> t

  val filteri_preserve : (int -> item -> bool) -> t -> t
end

module type MutGenS = sig
  include GenS

  val set : 'a t -> int -> 'a -> unit
  (** [set s n x] modifies the slice [s] in place,
      replacing element number [n] with [x].
      Raise [Invalid_argument "index out of bounds"] if [n] is
      outside the range 0 to [(Slice.length s - 1)].
    {[
let a = [| 0; 1; 2; 3; 4; 5 |] in
let mut = MutSlice.sub (MutSlice.of_array a) 2 3 in
let slice234 = Slice.of_mut mut in
assert (
  MutSlice.set mut 2 1;
  a = [| 0; 1; 2; 3; 1; 5 |]);
assert (Slice.to_list slice234 = [2; 3; 4]);
assert (
  try
    MutSlice.set mut 3 1;
    false
  with Invalid_argument _ ->
    true);
    ]} *)

  val try_set : 'a t -> int -> 'a -> bool
  (** [try_set s n x] modifies the slice [s] in place,
      replacing element number [n] with [x].
      Return [true] in case of success, and [false] if [n] is
      outside the range 0 to [(Slice.length s - 1)].
    {[
assert (
  MutSlice.try_set mut 1 4 &&
  a = [| 0; 1; 2; 4; 1; 5 |]);
assert (not (MutSlice.try_set mut (-1) 1));
    ]} *)

  val unsafe_set : 'a t -> int -> 'a -> unit
  (** [unsafe_set s n x] modifies the slice [s] in place,
      replacing element number [n] with [x].
      The behavior is unspecified if [n] is
      outside the range 0 to [(Slice.length s - 1)]. *)

  val copy : 'a t -> 'a t
  (** [copy s] returns a fresh slice containing the
      same elements as [s].
    {[
let mut = MutSlice.of_array a in
let copy = MutSlice.copy mut in
assert (
  MutSlice.set copy 1 7;
  MutSlice.to_list copy = [0; 7; 2; 4; 1; 5] &&
  MutSlice.to_list mut = [0; 1; 2; 4; 1; 5]
);
    ]} *)

  val recopy : src:('a t) -> tgt:('a t) -> unit
  (** [recopy ~src ~tgt] checks that [src] and [tgt] are the
      same length and copies the elements of [src] to [tgt].
      Raise [Invalid_argument] if lengths differ: in this case,
      no element is copied.
    {[
assert (
  MutSlice.recopy ~src:(MutSlice.sub mut 4 2)
    ~tgt:(MutSlice.sub copy 3 2);
  MutSlice.to_list copy = [0; 7; 2; 1; 5; 5] &&
  MutSlice.to_list mut = [0; 1; 2; 4; 1; 5]
);
assert (
  MutSlice.recopy ~src:(MutSlice.sub copy 1 3)
    ~tgt:(MutSlice.sub copy 2 3);
  MutSlice.to_list copy = [0; 7; 7; 2; 1; 5]
);
assert (
  MutSlice.recopy ~src:(MutSlice.sub copy 3 3)
    ~tgt:(MutSlice.sub copy 2 3);
  MutSlice.to_list copy = [0; 7; 2; 1; 5; 5]
);
assert (
  try
    MutSlice.recopy ~src:(MutSlice.sub mut 4 2)
      ~tgt:(MutSlice.sub copy 3 3);
    false
  with Invalid_argument _ ->
    true
);
    ]} *)

  val try_recopy : src:('a t) -> tgt:('a t) -> bool
  (** [try_recopy ~src ~tgt] checks that [src] and [tgt] are the
      same length and copies the elements of [src] to [tgt].
      Return [false] if lengths differ: in this case,
      no element is copied.
    {[
assert (
  MutSlice.try_recopy ~src:(MutSlice.sub mut 2 3)
    ~tgt:(MutSlice.sub copy 1 3) &&
  MutSlice.to_list copy = [0; 2; 4; 1; 5; 5] &&
  MutSlice.to_list mut = [0; 1; 2; 4; 1; 5]
);
assert (
  MutSlice.try_recopy ~src:(MutSlice.sub copy 1 2)
    ~tgt:(MutSlice.sub copy 2 2) &&
  MutSlice.to_list copy = [0; 2; 2; 4; 5; 5]
);
assert (
  MutSlice.try_recopy ~src:(MutSlice.sub copy 2 3)
    ~tgt:(MutSlice.sub copy 1 3) &&
  MutSlice.to_list copy = [0; 2; 4; 5; 5; 5]
);
assert (
  not (MutSlice.try_recopy ~src:(MutSlice.sub mut 1 3)
      ~tgt:(MutSlice.sub copy 2 2))
);
    ]} *)

  val unsafe_recopy : src:('a t) -> tgt:('a t) -> unit
  (** [recopy ~src ~tgt] checks that [src] and [tgt] are the
      same length and copies the elements of [src] to [tgt].
      The behavior is unspecified if lengths differ. *)

  val append_preserve : 'a t -> 'a t -> 'a t
  (** [append_preserve a b] returns a slice containing the concatenation of the
      slices [a] and [b].
      The elements are copied if and only if the
      slices [a] and [b] are not contiguous and neither [a] nor [b] are empty.
      In particular, if [a] or [b] is empty, then the other slice is returned
      unchanged.
      Use {!val:GenS.append} to get a fresh slice in every case.
    {[
assert (
  let mut_slice6 = MutSlice.init 6 (fun i -> i) in
  let mut_slice6' = MutSlice.append_preserve (MutSlice.sub mut_slice6 0 2)
    (MutSlice.sub mut_slice6 2 2) in
  MutSlice.set mut_slice6' 2 3;
  MutSlice.get mut_slice6 2 = 3);
    ]} *)

  val concat_preserve : 'a t list -> 'a t
  (** [concat_preserve l] returns a slice containing the concatenation of the
      slices listed in [l].
      The elements are copied if and only if the
      non-empty slices in [l] are not contiguous. In particular, if there is
      only one non-empty slice in [l], then this slice is returned unchanged.
      Use {!val:GenS.concat} to get a fresh slice in every case.
    {[
assert (
  let mut_slice6 = MutSlice.init 6 (fun i -> i) in
  let mut_slice6' = MutSlice.concat_preserve [MutSlice.sub mut_slice6 0 3;
    MutSlice.sub mut_slice6 3 2] in
  MutSlice.set mut_slice6' 2 3;
  MutSlice.get mut_slice6 2 = 3);
    ]} *)

  val to_array_preserve : 'a t -> 'a array
  (** [to_array_preserve s] returns an array with the elements of [s].
      If [s] covers all the elements of an array, the function returns the
      original array and no element are copied. If [s] is a strict sub-range,
      then the elements are copied to a fresh array.
      Use {!val:GenS.to_array} to get a fresh array in every case.
    {[
assert (MutSlice.to_array_preserve (MutSlice.of_array a) == a);
assert (
  let a = [| 0; 1; 2; 3; 4; 5 |] in
  let mut = MutSlice.sub (MutSlice.of_array a) 2 3 in
  MutSlice.to_array_preserve mut = [| 2; 3; 4 |]);
    ]} *)

  val filter_preserve : ('a -> bool) -> 'a t -> 'a t

  val filteri_preserve : (int -> 'a -> bool) -> 'a t -> 'a t
end

module type IndexedCollection = sig
  type t

  type item

  val empty : t

  val make : int -> item -> t

  val init : int -> (int -> item) -> t

  val unsafe_get : t -> int -> item

  type mut

  val create_mut : int -> mut

  val unsafe_blit : t -> int -> mut -> int -> int -> unit

  val unsafe_set : mut -> int -> item -> unit

  val unsafe_of_mut : mut -> t
end

module type MutIndexedCollection = sig
  type t

  include IndexedCollection with type t := t and type mut := t
end

module type CharSliceExtS = sig
  type t

  val split_on_char : char -> t -> t list

  val trim : t -> t
end
