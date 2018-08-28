module S = MonoSlice.Make (struct
  type t = string

  type item = char

  let empty = ""

  let make = String.make

  let init = String.init

  let unsafe_get = String.unsafe_get

  type mut = Stdcompat.bytes

  let create_mut = Stdcompat.Bytes.create

  let unsafe_blit = Stdcompat.String.unsafe_blit

  let unsafe_set = Stdcompat.Bytes.unsafe_set

  let unsafe_of_mut = Stdcompat.Bytes.unsafe_to_string
end)

include S

let of_string base =
  let length = String.length base in
  if length = 0 then
    empty
  else
    { base; offset = 0; length }

let of_bytes bytes =
  of_string (Bytes.to_string bytes)

let to_bytes { base; offset; length } =
  Bytes.sub (Bytes.unsafe_of_string base) offset length

let to_string { base; offset; length } =
  if offset = 0 && String.length base = length then
    base
  else
    String.sub base offset length

let of_mut slice =
  let length = MutCharSlice.length slice in
  if length = 0 then
    empty
  else
    { base = MutCharSlice.to_string slice; offset = 0; length }

let compare = compare Pervasives.compare

let equal = equal ( = )

let hash = hash Hashtbl.hash

include MonoSlice.CharSliceExt (S)

let add_to_buffer buffer { base; offset; length } =
  Buffer.add_substring buffer base offset length
