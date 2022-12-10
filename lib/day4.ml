open! Core
open Types

let line_to_int_pairs line =
  String.split line ~on:','
  |> List.map ~f:(fun s -> String.split s ~on:'-' |> List.map ~f:Int.of_string)

let parta ls =
  List.count ls ~f:(fun line ->
      match line_to_int_pairs line with
      | [ [ a; b ]; [ c; d ] ] -> (a <= c && d <= b) || (c <= a && b <= d)
      | _ -> failwith "a")
  |> Printer.of_int

let partb ls =
  List.count ls ~f:(fun line ->
      match line_to_int_pairs line with
      | [ [ a; b ]; [ c; d ] ] -> (a <= c && b >= c) || (c <= a && d >= a)
      | _ -> failwith "a")
  |> Printer.of_int

let sample =
  {|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8|} |> String.split ~on:'\n'

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| 2 |}]

let%expect_test "b" =
  partb sample |> Printer.print;
  [%expect {| 4 |}]
