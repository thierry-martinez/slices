val floatarray_blit :
    Stdcompat.floatarray -> int -> Stdcompat.floatarray -> int -> int -> unit

include SliceS.MutS with type item := float

val of_floatarray : Stdcompat.floatarray -> t

val to_floatarray : t -> Stdcompat.floatarray

val copy_to_floatarray : t -> Stdcompat.floatarray

val of_slice : float Slice.t -> t

val to_slice : t -> float Slice.t

val compare : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int
