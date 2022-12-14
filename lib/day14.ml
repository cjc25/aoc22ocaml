open! Containers
open! Core
open! Types

let ptiter ~f (xf, yf) (xt, yt) =
  let dx, dy =
    ( Int.clamp_exn ~min:(-1) ~max:1 (xt - xf),
      Int.clamp_exn ~min:(-1) ~max:1 (yt - yf) )
  in
  let rec loop (x, y) =
    match Xy.equal (x, y) (xt, yt) with
    | true -> f (x, y)
    | false ->
        f (x, y);
        loop (x + dx, y + dy)
  in
  loop (xf, yf)

let mkgrid_with_lowest ls =
  let grid = Xy.Hash_set.create () in
  let lowest = ref 0 in
  List.iter ls ~f:(fun line ->
      let pts =
        Input.split_on_string line ~sep:" -> "
        |> List.map ~f:(String.split ~on:',')
        |> List.map ~f:(function
             | [ xs; ys ] -> (Int.of_string xs, Int.of_string ys)
             | _ -> failwith "bad line")
      in
      let rec set start = function
        | [] -> ()
        | next :: tl ->
            ptiter start next ~f:(fun p ->
                Hash_set.add grid p;
                lowest := Int.max !lowest (Xy.y p));
            set next tl
      in
      set (List.hd_exn pts) (List.tl_exn pts));
  (grid, !lowest)

let rec drop ~stop grid (x, y) =
  let d = (x, y + 1) and dl = (x - 1, y + 1) and dr = (x + 1, y + 1) in
  match
    ( stop (x, y),
      Hash_set.mem grid d,
      Hash_set.mem grid dl,
      Hash_set.mem grid dr )
  with
  | true, _, _, _ ->
      Hash_set.add grid (x, y);
      (x, y)
  | _, false, _, _ -> drop ~stop grid d
  | _, _, false, _ -> drop ~stop grid dl
  | _, _, _, false -> drop ~stop grid dr
  | _ ->
      Hash_set.add grid (x, y);
      (x, y)

let parta ls =
  let grid, lowest = mkgrid_with_lowest ls in
  let rec loop grain =
    if drop ~stop:(fun (_, y) -> y = lowest) grid (500, 0) |> Xy.y = lowest then
      grain
    else loop (grain + 1)
  in
  loop 0 |> Printer.of_int

let partb ls =
  let grid, lowest = mkgrid_with_lowest ls in
  let rec loop grain =
    if
      drop ~stop:(fun (_, y) -> y = lowest + 1) grid (500, 0)
      |> Xy.equal (500, 0)
    then grain
    else loop (grain + 1)
  in
  loop 1 |> Printer.of_int

let _sample =
  {|498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9|}
  |> String.split ~on:'\n'

let%expect_test "a" =
  parta _sample |> Printer.print;
  [%expect {| 24 |}]

let%expect_test "b" =
  partb _sample |> Printer.print;
  [%expect {| 93 |}]
