open Core

module Printer = struct
  type t = Int : int -> t | String : string -> t | Unit : t

  let of_int i = Int i
  let of_string s = String s
  let of_unit _ = Unit

  let print = function
    | Int i -> Int.to_string i |> print_endline
    | String s -> print_endline s
    | Unit -> ()
end

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
