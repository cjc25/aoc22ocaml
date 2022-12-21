open! Containers
open! Core
open! Types

type t = Val of int | Op of string * string * string

let t_of_string op =
  match String.split op ~on:' ' with
  | [ lm; action; rm ] -> Op (lm, action, rm)
  | [ v ] -> Val (Int.of_string v)
  | _ -> failwithf "bad op %s" op ()

let eval_op lv action rv =
  match action with
  | "+" -> lv + rv
  | "-" -> lv - rv
  | "*" -> lv * rv
  | "/" -> lv / rv
  | _ -> failwithf "bad action %s" action ()

let rec eval ms m =
  match Hashtbl.find_exn ms m with
  | Val v -> v
  | Op (lm, action, rm) ->
      let lv = eval ms lm in
      let rv = eval ms rm in
      eval_op lv action rv

let parta ls =
  let ms = String.Table.create () in
  List.iter ls ~f:(fun line ->
      match Input.split_on_string line ~sep:": " with
      | [ monkey; op ] -> Hashtbl.set ms ~key:monkey ~data:(t_of_string op)
      | _ -> failwithf "bad line %s" line ());
  eval ms "root" |> Printer.of_int

let rec has_hum ms m =
  match (m, Hashtbl.find_exn ms m) with
  | "humn", _ -> true
  | _, Val _ -> false
  | _, Op (l, _, r) -> has_hum ms l || has_hum ms r

let invert_left_f invert action rv =
  match action with
  | "+" -> fun target -> invert (target - rv)
  | "-" -> fun target -> invert (target + rv)
  | "*" -> fun target -> invert (target / rv)
  | "/" -> fun target -> invert (target * rv)
  | _ -> failwith "bad action"

let invert_right_f lv action invert =
  match action with
  | "+" -> fun target -> invert (target - lv)
  | "-" -> fun target -> invert (lv - target)
  | "*" -> fun target -> invert (target / lv)
  | "/" -> fun target -> invert (lv / target)
  | _ -> failwith "bad action"

let rec eval_with_humn ms m =
  match (m, Hashtbl.find_exn ms m) with
  | "humn", _ -> `Humn Fn.id
  | _, Val v -> `Val v
  | _, Op (l, action, r) -> (
      let lv = eval_with_humn ms l in
      let rv = eval_with_humn ms r in
      match (lv, rv) with
      | `Humn _, `Humn _ -> failwith "humn on both sides. Too hard."
      | `Val lv, `Val rv -> `Val (eval_op lv action rv)
      | `Humn invert, `Val rv -> `Humn (invert_left_f invert action rv)
      | `Val lv, `Humn invert -> `Humn (invert_right_f lv action invert))

let partb ls =
  let ms = String.Table.create () in
  List.iter ls ~f:(fun line ->
      match Input.split_on_string line ~sep:": " with
      | [ monkey; op ] -> Hashtbl.set ms ~key:monkey ~data:(t_of_string op)
      | _ -> failwithf "bad line %s" line ());
  let l, r =
    match Hashtbl.find_exn ms "root" with
    | Op (lm, _, rm) -> (lm, rm)
    | _ -> failwith "bad root"
  in
  let result =
    match (eval_with_humn ms l, eval_with_humn ms r) with
    | `Val _, `Val _ | `Humn _, `Humn _ -> failwith "uh oh!"
    | `Val v, `Humn invert | `Humn invert, `Val v -> invert v
  in
  result |> Printer.of_int

let _sample =
  {|root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32|}
  |> String.split ~on:'\n'

let%expect_test "a" =
  parta _sample |> Printer.print;
  [%expect {| 152 |}]

let%expect_test "b" =
  partb _sample |> Printer.print;
  [%expect {| 301 |}]
