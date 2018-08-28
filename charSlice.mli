include SliceS.S with type item := char

val of_string : string -> t

val of_bytes : Stdcompat.bytes -> t

val to_string : t -> string

val to_bytes : t -> Stdcompat.bytes

val of_slice : char Slice.t -> t

val to_slice : t -> char Slice.t

val of_mut : MutCharSlice.t -> t

val compare : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int

include SliceS.CharSliceExtS with type t := t

val add_to_buffer : Buffer.t -> t -> unit
