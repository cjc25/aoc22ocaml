open! Containers
open! Core
open! Types

let char_to_int = function
  | '2' -> 2
  | '1' -> 1
  | '0' -> 0
  | '-' -> -1
  | '=' -> -2
  | _ -> failwith "bad char"

let int_to_char = function
  | 2 -> '2'
  | 1 -> '1'
  | 0 -> '0'
  | -1 -> '-'
  | -2 -> '='
  | _ -> failwith "bad int"

let parta ls =
  let sum =
    List.sum
      (module Int)
      ls
      ~f:(fun line ->
        String.to_list line |> List.rev
        |> List.foldi ~init:0 ~f:(fun i sum c ->
               sum + (Int.pow 5 i * char_to_int c)))
  in
  (* Construct the base-5 representation of sum as an int list *)
  let rec to_b5 acc = function
    | 0 -> acc
    | v -> to_b5 ((v % 5) :: acc) (v / 5)
  in
  (* reverse the digits so we start with the ones place *)
  let rep = to_b5 [] sum |> List.rev in
  (* For each place, if it's 3, 4, or 5, carry one forward and subtract *)
  let rec to_snafu carry acc n =
    match (carry, n) with
    | true, [] -> 1 :: acc
    | false, [] -> acc
    | c, hd :: tl -> (
        (* hd is always between 0 and 5 due to the mod above *)
        let d = if c then hd + 1 else hd in
        match d with
        | d when d <= 2 -> to_snafu false (d :: acc) tl
        | d when d <= 5 -> to_snafu true ((d - 5) :: acc) tl
        | _ -> failwith "unexpected digit")
  in
  to_snafu false [] rep |> List.map ~f:int_to_char |> String.of_char_list
  |> Printer.of_string

let partb _ = Printer.of_unit ()

let sample =
  {|1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122|}
  |> String.split ~on:'\n'

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| 2=-1=0 |}]

let%expect_test "b" =
  partb sample |> Printer.print;
  [%expect {| |}]
