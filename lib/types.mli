open Core

module Xy : sig
  type t = int * int

  val x : t -> int
  val y : t -> int

  include Comparable.S with type t := t
end
