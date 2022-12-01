open Core

module Xy = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]

    let x = fst 
    let y = snd
  end

  include T
  include Comparable.Make (T)
end
