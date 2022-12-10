open Core

module Printer : sig
  type t

  val of_int : int -> t
  val of_unit : unit -> t
  val of_string : string -> t
  val print : t -> unit
end

module Xy : sig
  type t = int * int [@@deriving hash, sexp]

  val x : t -> int
  val y : t -> int

  include Comparable.S with type t := t
end

val foldn : n:int -> init:'a -> f:('a -> 'b list -> 'a) -> 'b list -> 'a