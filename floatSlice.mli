include SliceS.S with type item := float

val of_floatarray : Stdcompat.floatarray -> t

val unsafe_of_floatarray : Stdcompat.floatarray -> t

val to_floatarray : t -> Stdcompat.floatarray

val unsafe_to_floatarray : t -> Stdcompat.floatarray

val of_slice : float Slice.t -> t

val to_slice : t -> float Slice.t

val unsafe_of_mut : MutFloatSlice.t -> t

val of_mut : MutFloatSlice.t -> t

val compare : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int
