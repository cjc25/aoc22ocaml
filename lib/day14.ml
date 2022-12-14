open! Containers
open! Core
open! Types

let iter (xf, yf) (xt, yt) ~f =
  let dx, dy =
    ( Int.clamp_exn ~min:(-1) ~max:1 (xt - xf),
      Int.clamp_exn ~min:(-1) ~max:1 (yt - yf) )
  in
  let rec loop (x, y) =
    if Xy.equal (x, y) (xt, yt) then f (x, y)
    else (
      f (x, y);
      loop (x + dx, y + dy))
  in
  loop (xf, yf)

let parta ls =
  let grid = Hash_set.create (module Xy) in
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
            iter start next ~f:(fun p ->
                Hash_set.add grid p;
                lowest := Int.max !lowest (Xy.y p));
            set next tl
      in
      set (List.hd_exn pts) (List.tl_exn pts));
  let rec loop grain =
    let rec drop (x, y) =
      if y = !lowest then y
      else
        match
          ( Hash_set.mem grid (x, y + 1),
            Hash_set.mem grid (x - 1, y + 1),
            Hash_set.mem grid (x + 1, y + 1) )
        with
        | false, _, _ -> drop (x, y + 1)
        | _, false, _ -> drop (x - 1, y + 1)
        | _, _, false -> drop (x + 1, y + 1)
        | _ ->
            Hash_set.add grid (x, y);
            y
    in
    if drop (500, 0) = !lowest then grain else loop (grain + 1)
  in
  loop 0 |> Printer.of_int

let partb ls =
  let grid = Hash_set.create (module Xy) in
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
            iter start next ~f:(fun p ->
                Hash_set.add grid p;
                lowest := Int.max !lowest (Xy.y p));
            set next tl
      in
      set (List.hd_exn pts) (List.tl_exn pts));
  let rec loop grain =
    let rec drop (x, y) =
      if y = !lowest + 1 then (
        Hash_set.add grid (x, y);
        (x, y))
      else
        match
          ( Hash_set.mem grid (x, y + 1),
            Hash_set.mem grid (x - 1, y + 1),
            Hash_set.mem grid (x + 1, y + 1) )
        with
        | false, _, _ -> drop (x, y + 1)
        | _, false, _ -> drop (x - 1, y + 1)
        | _, _, false -> drop (x + 1, y + 1)
        | _ ->
            Hash_set.add grid (x, y);
            (x, y)
    in
    if Xy.equal (drop (500, 0)) (500, 0) then grain else loop (grain + 1)
  in
  loop 1 |> Printer.of_int

let _sample = {|498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9|} |> String.split ~on:'\n'

let%expect_test "a" =
   parta _sample |> Printer.print;
   [%expect {| 24 |}]

let%expect_test "b" =
   partb _sample |> Printer.print;
   [%expect {| 93 |}]
