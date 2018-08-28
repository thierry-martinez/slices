module Make (X : SliceS.IndexedCollection) : sig
  type t = {
      base : X.t;
      offset : int;
      length : int;
    }

  include SliceS.S with type item = X.item and type t := t

  val of_slice : item Slice.t -> t

  val to_slice : t -> item Slice.t
end

module MakeMut (X : SliceS.MutIndexedCollection) : sig
  type t = {
      base : X.t;
      offset : int;
      length : int;
    }

  include SliceS.MutS with type item = X.item and type t := t

  val of_slice : item Slice.t -> t

  val to_slice : t -> item Slice.t
end

module CharSliceExt (S : SliceS.S with type item := char) : SliceS.CharSliceExtS
with type t := S.t
