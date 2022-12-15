open! Containers
open! Core
open! Types

let mandist (x1, y1) (x2, y2) = Int.abs (x1 - x2) + Int.abs (y1 - y2)

let parta' row ls =
  let grid = Xy.Hash_set.create () in
  let beacons =
    List.fold ~init:[] ls ~f:(fun acc line ->
        let xs, ys, xb, yb =
          Scanf.sscanf line
            "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
            (fun xs ys xb yb -> (xs, ys, xb, yb))
        in
        (* set all points <= manhattan distance *)
        let dist = mandist (xs, ys) (xb, yb) in
        for x = xs - dist to xs + dist do
          if mandist (xs, ys) (x, row) <= dist then
            Hash_set.add grid (x, row)
          else ()
        done;
        (xb, yb) :: acc)
  in
  List.iter beacons ~f:(fun b -> Hash_set.remove grid b);
  Hash_set.count grid ~f:(fun (_, y) -> y = row) |> Printer.of_int

let parta ls = parta' 2000000 ls

module DistXy = struct
  module T = struct
    type t = int * Xy.t [@@deriving compare, hash, sexp]
  end

  include T
  include Hashable.Make (T)
end

let partb' max ls =
  let grid = DistXy.Hash_set.create () in
  List.iter ls ~f:(fun line ->
      let xs, ys, xb, yb =
        Scanf.sscanf line
          "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
          (fun xs ys xb yb -> (xs, ys, xb, yb))
      in
      let dist = mandist (xs, ys) (xb, yb) in
      Hash_set.add grid (dist, (xs, ys)));
  let x, y =
    Hash_set.find_map grid ~f:(fun (d, (x, y)) ->
        (* walk its edges *)
        let edges =
          List.concat_map
            [
              ((x - d - 1, y), (1, 1));
              ((x, y + d + 1), (1, -1));
              ((x + d + 1, y), (-1, -1));
              ((x, y - d - 1), (-1, 1));
            ]
            ~f:(fun ((sx, sy), (dx, dy)) ->
              List.init d ~f:(fun i -> (sx + (i * dx), sy + (i * dy))))
        in
        List.find edges ~f:(fun ((x, y) as e) ->
            x >= 0 && x <= max && y >= 0 && y <= max
            && Hash_set.for_all grid ~f:(fun (d, c) -> mandist e c > d)))
    |> Option.value_exn
  in
  (x * 4000000) + y |> Printer.of_int

let partb ls = partb' 4000000 ls

let sample =
  {|Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3|}
  |> String.split ~on:'\n'

let%expect_test "a" =
   parta' 10 sample |> Printer.print;
   [%expect {| 26 |}]

let%expect_test "b" =
  partb' 20 sample |> Printer.print;
  [%expect {| 56000011 |}]
