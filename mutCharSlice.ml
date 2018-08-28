module S = MonoSlice.MakeMut (struct
  type t = bytes

  type item = char

  let empty = Stdcompat.Bytes.empty

  let make = Stdcompat.Bytes.make

  let init = Stdcompat.Bytes.init

  let unsafe_get = Stdcompat.Bytes.unsafe_get

  let create_mut = Stdcompat.Bytes.create

  let unsafe_blit = Stdcompat.Bytes.unsafe_blit

  let unsafe_sub bytes offset length =
    let result = Stdcompat.Bytes.create length in
    Stdcompat.Bytes.unsafe_blit bytes offset result 0 length;
    result

  let unsafe_set = Stdcompat.Bytes.unsafe_set

  let unsafe_of_mut x = x
end)

include S

let of_bytes base =
  let length = Stdcompat.Bytes.length base in
  if length = 0 then
    empty
  else
    { base; offset = 0; length }

let of_string s =
  of_bytes (Stdcompat.Bytes.of_string s)

let to_bytes { base; offset; length } =
  Stdcompat.Bytes.sub base offset length

let to_string { base; offset; length } =
  Stdcompat.Bytes.sub_string base offset length

let compare = compare Pervasives.compare

let equal = equal ( = )

let hash = hash Hashtbl.hash

include MonoSlice.CharSliceExt (S)

let add_to_buffer buffer { base; offset; length } =
  Buffer.add_subbytes buffer base offset length
