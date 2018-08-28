include MutFloatSlice

let unsafe_of_floatarray = of_floatarray

let of_floatarray floatarray =
  let length = Stdcompat.Float.Array.length floatarray in
  if length = 0 then
    empty
  else
    let copy = Stdcompat.Float.Array.create length in
    floatarray_blit floatarray 0 copy 0 length;
    unsafe_of_floatarray copy

let unsafe_to_floatarray = to_floatarray

let to_floatarray = copy_to_floatarray

let unsafe_of_mut slice = slice

let of_mut slice =
  unsafe_of_floatarray (to_floatarray slice)

let append = append_preserve

let concat = concat_preserve

let map = map_preserve

let mapi = mapi_preserve

let filter = filter_preserve

let filteri = filteri_preserve
