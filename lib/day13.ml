open Containers
open! Core
open Types

type v = Int of int | List of v list

let maybe_prefix running acc =
  match running with None -> acc | Some i -> Int i :: acc

let to_charval c = Char.to_int c - 48

let to_rep line =
  let rec to_v running acc i =
    if i = String.length line then (List.rev acc, i)
    else
      match String.get line i with
      | ',' -> to_v None (maybe_prefix running acc) (i + 1)
      | ']' -> (maybe_prefix running acc |> List.rev, i + 1)
      | '[' ->
          let subv, j = to_v None [] (i + 1) in
          to_v None (List subv :: acc) j
      | c ->
          let running =
            match running with
            | Some v -> (v * 10) + to_charval c
            | None -> to_charval c
          in
          to_v (Some running) acc (i + 1)
  in
  let l, _ = to_v None [] 0 in
  l

let rec cmp left right =
  let subcmp ltl rtl = function 0 -> cmp ltl rtl | v -> v in
  match (left, right) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | List ls :: ltl, List rs :: rtl -> cmp ls rs |> subcmp ltl rtl
  | List ls :: ltl, Int r :: rtl -> cmp ls [ Int r ] |> subcmp ltl rtl
  | Int l :: ltl, List rs :: rtl -> cmp [ Int l ] rs |> subcmp ltl rtl
  | Int l :: ltl, Int r :: rtl -> Int.compare l r |> subcmp ltl rtl

let parta ls =
  Input.to_sections ls
  |> List.foldi ~init:0 ~f:(fun i sum lines ->
         let l = List.nth_exn lines 0 |> to_rep in
         let r = List.nth_exn lines 1 |> to_rep in
         if cmp l r < 0 then sum + i + 1 else sum)
  |> Printer.of_int

let partb ls =
  "[[2]]" :: "[[6]]" :: List.filter ls ~f:(fun l -> String.is_empty l |> not)
  |> List.map ~f:to_rep |> List.sort ~compare:cmp
  |> List.foldi ~init:1 ~f:(fun i prod -> function
       | [ List [ List [ Int 2 ] ] ] | [ List [ List [ Int 6 ] ] ] ->
           prod * (i + 1)
       | _ -> prod)
  |> Printer.of_int

let sample =
  {|[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]|}
  |> String.split ~on:'\n'

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| 13 |}]

let%expect_test "b" =
  partb sample |> Printer.print;
  [%expect {| 140 |}]
