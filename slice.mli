include SliceS.GenS

val unsafe_of_mut : 'a MutSlice.t -> 'a t

val of_mut : 'a MutSlice.t -> 'a t

val unsafe_of_array : 'a array -> 'a t

val unsafe_to_array : 'a t -> 'a array
