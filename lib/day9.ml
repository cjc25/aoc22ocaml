open! Core

type result = int

let oneaway (x1, y1) (x2, y2) = Int.abs (x1 - x2) <= 1 && Int.abs (y1 - y2) <= 1
let mv ~times (x1, y1) (dx, dy) = (x1 + (dx * times), y1 + (dy * times))

let mvtowards (xf, yf) (xt, yt) =
  ( Int.clamp_exn ~min:(xf - 1) ~max:(xf + 1) xt,
    Int.clamp_exn ~min:(yf - 1) ~max:(yf + 1) yt )

let tracktl ~len ls =
  let hdposes = List.folding_map ls ~init:(0, 0) ~f:(fun hd l ->
      let d, ct =
        match String.split ~on:' ' l with
        | [ "U"; ct ] -> ((0, 1), Int.of_string ct)
        | [ "R"; ct ] -> ((1, 0), Int.of_string ct)
        | [ "L"; ct ] -> ((-1, 0), Int.of_string ct)
        | [ "D"; ct ] -> ((0, -1), Int.of_string ct)
        | _ -> failwith "bad line"
      in
      (mv hd d ~times:ct, List.init ct ~f:(fun i -> mv hd d ~times:(i + 1))))
  |> List.concat in
  let tlposes = List.init len ~f:(fun _ -> (0, 0)) in
  List.folding_map hdposes ~init:tlposes ~f:(fun tlposes nh ->
      let tl, tlposes =
        List.fold_map tlposes ~init:nh ~f:(fun target p ->
            if oneaway p target then (p, p)
            else
              let newp = mvtowards p target in
              (newp, newp))
      in
      (tlposes, tl))
  |> Types.Xy.Set.of_list |> Set.length

let parta ls = tracktl ~len:1 ls

let partb ls = tracktl ~len:9 ls

let sample = {|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2|} |> String.split ~on:'\n'

let%expect_test "a" =
  parta sample |> Int.to_string |> print_endline;
  [%expect {| 13 |}]

let%expect_test "b" =
  partb sample |> Int.to_string |> print_endline;
  [%expect {| 1 |}]

let sample2 = {|R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20|} |> String.split ~on:'\n'

let%expect_test "b2" =
  partb sample2 |> Int.to_string |> print_endline;
  [%expect {| 36 |}]