open Containers
open! Core
open Types

let nabes (x, y) = [ (x + 1, y); (x, y - 1); (x, y + 1); (x - 1, y) ]

module PrioXy = struct
  module T = struct
    type t = int * Xy.t [@@deriving compare, sexp]
  end

  include Comparable.Make (T)
  module Heap = Heap.Make_from_compare (T)
end

let solve ls is_start =
  let grid = Hashtbl.create (module Xy) in
  let starts, goal =
    List.foldi ls
      ~init:([], (0, 0))
      ~f:(fun y (starts, goal) line ->
        String.to_list line
        |> List.foldi ~init:(starts, goal) ~f:(fun x (starts, goal) c ->
               let starts =
                 is_start c
                 |> Option.value_map ~default:starts ~f:(fun _ ->
                        (x, y) :: starts)
               in
               let goal, c =
                 match c with
                 | 'S' -> (goal, 'a')
                 | 'E' -> ((x, y), 'z')
                 | v -> (goal, v)
               in
               Hashtbl.set grid ~key:(x, y) ~data:(Char.to_int c);
               (starts, goal)))
  in
  let seen = Hash_set.create (module Xy) in
  let rec seek q =
    let q, (s, p) = PrioXy.Heap.take_exn q in
    if Xy.equal p goal then s
    else if Hash_set.mem seen p then seek q
    else (
      Hash_set.add seen p;
      let ht = Hashtbl.find_exn grid p in
      let ns =
        nabes p
        |> List.filter_map ~f:(fun n ->
               Hashtbl.find grid n
               |> Option.bind ~f:(fun nht ->
                      if nht <= ht + 1 then Some (s + 1, n) else None))
      in
      let q = PrioXy.Heap.add_list q ns in
      seek q)
  in
  List.map starts ~f:(fun p -> (0, p))
  |> PrioXy.Heap.of_list |> seek |> Printer.of_int

let parta ls = solve ls (function 'S' -> Some 'a' | _ -> None)
let partb ls = solve ls (function 'S' | 'a' -> Some 'a' | _ -> None)

let sample =
  {|Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi|} |> String.split ~on:'\n'

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| 31 |}]

let%expect_test "b" =
  partb sample |> Printer.print;
  [%expect {| 29 |}]
