open Core

module Xy = struct
  module T = struct
    type t = int * int [@@deriving compare, hash, sexp]

    let x = fst
    let y = snd
  end

  include T
  include Comparable.Make (T)
end

let rec foldn ~n ~init ~f l =
  let curr, tl = List.split_n l n in
  match tl with
  | [] ->
      if List.length curr <> n then
        failwithf "list length is not a multiple of %d" n ()
      else f init curr
  | tl ->
      let init = f init curr in
      foldn ~n ~init ~f tl
