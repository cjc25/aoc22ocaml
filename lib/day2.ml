open! Core
open Types

let sample = {|A Y
B X
C Z|} |> String.split ~on:'\n'

let score1 line =
  match String.split line ~on:' ' with
  | [ "A"; "X" ] -> 4
  | [ "A"; "Y" ] -> 8
  | [ "A"; "Z" ] -> 3
  | [ "B"; "X" ] -> 1
  | [ "B"; "Y" ] -> 5
  | [ "B"; "Z" ] -> 9
  | [ "C"; "X" ] -> 7
  | [ "C"; "Y" ] -> 2
  | [ "C"; "Z" ] -> 6
  | _ -> failwith "bad line"

let parta ls = List.sum (module Int) ls ~f:score1 |> Printer.of_int

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| 15 |}]

let score2 line =
  match String.split line ~on:' ' with
  | [ "A"; "X" ] -> 3
  | [ "A"; "Y" ] -> 4
  | [ "A"; "Z" ] -> 8
  | [ "B"; "X" ] -> 1
  | [ "B"; "Y" ] -> 5
  | [ "B"; "Z" ] -> 9
  | [ "C"; "X" ] -> 2
  | [ "C"; "Y" ] -> 6
  | [ "C"; "Z" ] -> 7
  | _ -> failwith "bad line"

let partb ls = List.sum (module Int) ls ~f:score2 |> Printer.of_int

let%expect_test "b" =
  partb sample |> Printer.print;
  [%expect {| 12 |}]
