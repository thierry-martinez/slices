include MutSlice

let unsafe_of_array = of_array

let of_array array =
  unsafe_of_array (Array.copy array)

let unsafe_to_array = to_array_preserve

let unsafe_of_mut slice =
  slice

let of_mut = copy

let append = append_preserve

let concat = concat_preserve

let filter = filter_preserve

let filteri = filteri_preserve
