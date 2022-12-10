open! Core
open Types

let sample =
  {|vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw|}
  |> String.split ~on:'\n'

let charpriority c =
  if Char.is_uppercase c then Char.to_int c - Char.to_int 'A' + 27
  else Char.to_int c - Char.to_int 'a' + 1

let charset s = String.to_list s |> Char.Set.of_list

let parta ls =
  List.sum
    (module Int)
    ls
    ~f:(fun sack ->
      let sidelen = String.length sack / 2 in
      let inleft = String.prefix sack sidelen |> charset in
      let inright = String.suffix sack sidelen |> charset in
      let elem = Set.inter inleft inright |> Set.min_elt_exn in
      charpriority elem)
  |> Printer.of_int

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| 157 |}]

let partb ls =
  Types.foldn ls ~n:3 ~init:0 ~f:(fun acc sacks ->
      let badge =
        List.map sacks ~f:charset |> List.reduce ~f:Set.inter
        |> Option.value_exn |> Set.min_elt_exn
      in
      acc + charpriority badge)
  |> Printer.of_int

let%expect_test "b" =
  partb sample |> Printer.print;
  [%expect {| 70 |}]