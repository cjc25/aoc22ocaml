open! Core
open Types

let nabes (x, y) = [ (x + 1, y); (x, y - 1); (x, y + 1); (x - 1, y) ]

module PrioXy = struct
  module T = struct
    type t = int * Xy.t [@@deriving compare, sexp]
  end

  include Comparable.Make (T)
end

let parta ls =
  let start = ref (0, 0) in
  let goal = ref (0, 0) in
  let grid = Hashtbl.create (module Xy) in
  List.iteri ls ~f:(fun y line ->
      String.to_list line
      |> List.iteri ~f:(fun x c ->
             let c =
               match c with
               | 'S' ->
                   start := (x, y);
                   'a'
               | 'E' ->
                   goal := (x, y);
                   'z'
               | v -> v
             in
             let v = Char.to_int c in
             Hashtbl.set grid ~key:(x, y) ~data:v));
  let rec search seen es =
    let res =
      List.fold_until es ~init:(seen, [])
        ~f:(fun (seen, news) (steps, pos) ->
          if Xy.equal pos !goal then Stop (`Found steps)
          else
            let ht = Hashtbl.find_exn grid pos in
            let es =
              nabes pos
              |> List.filter_map ~f:(fun nabe ->
                     let cango =
                       Hashtbl.find grid nabe
                       |> Option.value_map ~default:false ~f:(fun nht ->
                              nht <= ht + 1)
                     in
                     if Set.mem seen nabe || not cango then None
                     else Some (steps + 1, nabe))
            in
            let nseen =
              List.fold es ~init:seen ~f:(fun s (_, p) -> Set.add s p)
            in
            Continue (nseen, es @ news))
        ~finish:(fun s -> `Cont s)
    in
    match res with
    | `Found steps -> steps
    | `Cont (seen, news) -> search seen news
  in
  search Xy.Set.empty [ (0, !start) ] |> Printer.of_int

let partb ls =
  let starts = ref [] in
  let goal = ref (0, 0) in
  let grid = Hashtbl.create (module Xy) in
  List.iteri ls ~f:(fun y line ->
      String.to_list line
      |> List.iteri ~f:(fun x c ->
             let c =
               match c with
               | 'S' | 'a' ->
                   starts := (x, y) :: !starts;
                   'a'
               | 'E' ->
                   goal := (x, y);
                   'z'
               | v -> v
             in
             let v = Char.to_int c in
             Hashtbl.set grid ~key:(x, y) ~data:v));
  let found = ref None in
  let q = List.map !starts ~f:(fun p -> (0, p)) |> PrioXy.Set.of_list |> ref in
  let seen = ref Xy.Set.empty in
  while Option.is_none !found do
    let s, p = Set.min_elt_exn !q in
    q := Set.remove_index !q 0;
    if Xy.equal p !goal then found := Some s
    else if Set.mem !seen p then ()
    else
      let ht = Hashtbl.find_exn grid p in
      let ns =
        nabes p
        |> List.filter_map ~f:(fun n ->
               let cango =
                 Hashtbl.find grid n
                 |> Option.value_map ~default:false ~f:(fun nht ->
                        nht <= ht + 1)
               in
               if cango then Some (s + 1, n) else None)
      in
      q := PrioXy.Set.of_list ns |> Set.union !q
  done;
  Option.value_exn !found |> Printer.of_int

let _sample =
  {|Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi|} |> String.split ~on:'\n'

let%expect_test "a" =
  parta _sample |> Printer.print;
  [%expect {| 31 |}]

let%expect_test "b" =
  partb _sample |> Printer.print;
  [%expect {| 29 |}]
