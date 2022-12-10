open! Core
open Types

let solve ct line =
  let rec check seen l =
    let chars = List.take l ct |> Char.Set.of_list in
    if Set.length chars = ct then seen + ct
    else check (seen + 1) (List.tl_exn l)
  in
  check 0 line

let parta ls = List.hd_exn ls |> String.to_list |> solve 4 |> Printer.of_int
let partb ls = List.hd_exn ls |> String.to_list |> solve 14 |> Printer.of_int
let sample = {|mjqjpqmgbljsphdztnvjfqwrcgsmlb|} |> String.split ~on:'\n'

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| 7 |}]

let%expect_test "b" =
  partb sample |> Printer.print;
  [%expect {| 19 |}]
