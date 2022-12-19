open! Containers
open! Core
open! Types

let side_nabes (x, y, z) =
  [
    (x + 1, y, z);
    (x, y + 1, z);
    (x, y, z + 1);
    (x - 1, y, z);
    (x, y - 1, z);
    (x, y, z - 1);
  ]

let parta ls =
  let grid = Xyz.Hash_set.create () in
  List.iter ls ~f:(fun l ->
      match String.split l ~on:',' |> List.map ~f:Int.of_string with
      | [ x; y; z ] -> Hash_set.add grid (x, y, z)
      | _ -> failwith "bad line");
  Hash_set.sum
    (module Int)
    grid
    ~f:(fun c ->
      side_nabes c |> List.count ~f:(fun n -> Hash_set.mem grid n |> not))
  |> Printer.of_int

let partb ls =
  let grid = Xyz.Hash_set.create () in
  let minx, miny, minz, maxx, maxy, maxz =
    ( ref Int.max_value,
      ref Int.max_value,
      ref Int.max_value,
      ref Int.min_value,
      ref Int.min_value,
      ref Int.min_value )
  in
  (* Get min & max xyz*)
  List.iter ls ~f:(fun l ->
      match String.split l ~on:',' |> List.map ~f:Int.of_string with
      | [ x; y; z ] ->
          Hash_set.add grid (x, y, z);
          minx := Int.min !minx x;
          miny := Int.min !miny y;
          minz := Int.min !minz z;
          maxx := Int.max !maxx x;
          maxy := Int.max !maxy y;
          maxz := Int.max !maxz z
      | _ -> failwith "bad line");
  decr minx;
  decr miny;
  decr minz;
  incr maxx;
  incr maxy;
  incr maxz;
  (* Explore from min-1 of all three to find walls *)
  let seen = Xyz.Hash_set.create () in
  let rec explore walls q =
    match q with
    | [] -> walls
    | q ->
        let walls, newq =
          List.fold_map q ~init:walls ~f:(fun walls ((x, y, z) as point) ->
              let outside =
                x < !minx || y < !miny || z < !minz || x > !maxx || y > !maxy
                || z > !maxz
              in
              match (outside, Hash_set.mem seen point) with
              | true, _ -> (walls, [])
              | _, true -> (walls, [])
              | false, false ->
                  Hash_set.add seen point;
                  let walls, nps =
                    side_nabes point
                    |> List.fold_map ~init:walls ~f:(fun walls n ->
                           match Hash_set.mem grid n with
                           | true -> (walls + 1, None)
                           | false -> (walls, Some n))
                  in
                  (walls, List.filter_opt nps))
        in
        explore walls (List.concat newq)
  in
  (* Count of walls is surface area *)
  explore 0 [ (!minx, !miny, !minz) ] |> Printer.of_int

let sample =
  {|2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5|}
  |> String.split ~on:'\n'

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| 64 |}]

let%expect_test "b" =
  partb sample |> Printer.print;
  [%expect {| 58 |}]
