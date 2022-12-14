open! Core
open Types

let sample =
  {|1000
2000
3000

4000

5000
6000

7000
8000
9000

10000|}
  |> String.split ~on:'\n'

let calorie_counts_decreasing bags =
  List.map bags ~f:(List.sum (module Int) ~f:Int.of_string)
  |> List.sort ~compare:(Fn.flip Int.compare)

let parta ls =
  Input.to_sections ls |> calorie_counts_decreasing |> List.hd_exn
  |> Printer.of_int

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| 24000 |}]

let partb ls =
  let sorted = Input.to_sections ls |> calorie_counts_decreasing in
  List.take sorted 3 |> List.sum (module Int) ~f:Fn.id |> Printer.of_int

let%expect_test "b" =
  partb sample |> Printer.print;
  [%expect {| 45000 |}]
