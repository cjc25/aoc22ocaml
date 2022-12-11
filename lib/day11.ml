open! Core
open! Types

type monkey = {
  items : int list;
  op : int -> int;
  to_monkey : score:int -> int;
  inspections : int;
}

let to_last_elem ~sep v =
  Input.split_on_string v ~sep |> List.tl_exn |> List.hd_exn |> Int.of_string

let mk_monkey ls =
  match ls with
  | [ _; is; op; test; t; f ] ->
      let items =
        Input.split_on_string is ~sep:": "
        |> List.tl_exn |> List.hd_exn
        |> Input.split_on_string ~sep:", "
        |> List.map ~f:Int.of_string
      in
      let op =
        match
          Input.split_on_string op ~sep:": new = "
          |> List.tl_exn |> List.hd_exn
          |> Input.split_on_string ~sep:" "
        with
        | [ a; op; b ] ->
            fun old ->
              let left = if String.(a = "old") then old else Int.of_string a in
              let right = if String.(b = "old") then old else Int.of_string b in
              let res =
                if String.(op = "+") then left + right else left * right
              in
              if res < old then printf "%d -> %d\n" old res;
              res
        | _ -> failwith "bad op"
      in
      let tmon = to_last_elem t ~sep:" monkey " in
      let fmon = to_last_elem f ~sep:" monkey " in
      let to_monkey ~score =
        let div =
          Input.split_on_string test ~sep:" by "
          |> List.tl_exn |> List.hd_exn |> Int.of_string
        in
        if score % div = 0 then tmon else fmon
      in
      { items; op; to_monkey; inspections = 0 }
  | _ -> failwith "bad monkey"

let do_monkey1 ms id =
  let m = ms.(id) in
  List.iter m.items ~f:(fun score ->
      let score = m.op score / 3 in
      let tom = m.to_monkey ~score in
      ms.(tom) <- { (ms.(tom)) with items = ms.(tom).items @ [ score ] });
  ms.(id) <-
    { m with items = []; inspections = m.inspections + List.length m.items }

let parta ls =
  let sections = Input.to_sections ls in
  let ms = Array.of_list_map sections ~f:mk_monkey in
  for _ = 1 to 20 do
    for i = 0 to Array.length ms - 1 do
      do_monkey1 ms i
    done
  done;
  Array.sort ms ~compare:(fun l r -> Int.compare r.inspections l.inspections);
  ms.(0).inspections * ms.(1).inspections |> Printer.of_int

let do_monkey2 ms id =
  let m = ms.(id) in
  List.iter m.items ~f:(fun score ->
      let score = m.op score in
      let tom = m.to_monkey ~score in
      let score = score % (2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23) in
      ms.(tom) <- { (ms.(tom)) with items = ms.(tom).items @ [ score ] });
  ms.(id) <-
    { m with items = []; inspections = m.inspections + List.length m.items }

let partb ls =
  let sections = Input.to_sections ls in
  let ms = Array.of_list_map sections ~f:mk_monkey in
  for _ = 1 to 10000 do
    for i = 0 to Array.length ms - 1 do
      do_monkey2 ms i
    done
  done;
  Array.sort ms ~compare:(fun l r -> Int.compare r.inspections l.inspections);
  ms.(0).inspections * ms.(1).inspections |> Printer.of_int

let _sample =
  {|Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1|}
  |> String.split ~on:'\n'

let%expect_test "a" =
  parta _sample |> Printer.print;
  [%expect {| 10605 |}]

let%expect_test "b" =
  partb _sample |> Printer.print;
  [%expect {| 2713310158 |}]
