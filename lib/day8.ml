open! Core
open Types

type view = { line : char list; toxy : int -> Types.Xy.t }

let parta ls =
  let ls = List.map ls ~f:String.to_list in
  let views =
    List.concat
      [
        List.concat_mapi ls ~f:(fun y line ->
            [
              { line; toxy = (fun x -> (x, y)) };
              {
                line = List.rev line;
                toxy = (fun negx -> (List.length line - negx - 1, y));
              };
            ]);
        List.transpose_exn ls
        |> List.concat_mapi ~f:(fun x line ->
               [
                 { line; toxy = (fun y -> (x, y)) };
                 {
                   line = List.rev line;
                   toxy = (fun negy -> (x, List.length line - negy - 1));
                 };
               ]);
      ]
  in
  List.fold views ~init:Types.Xy.Set.empty ~f:(fun s v ->
      (* '/' = '0' - 1 *)
      List.foldi v.line ~init:('/', s) ~f:(fun i (maxht, s) tree ->
          if Char.(tree > maxht) then (tree, v.toxy i |> Set.add s)
          else (maxht, s))
      |> snd)
  |> Set.length |> Printer.of_int

let partb ls =
  let tbl = Hashtbl.create (module Types.Xy) in
  List.iteri ls ~f:(fun y line ->
      String.to_list line
      |> List.iteri ~f:(fun x c -> Hashtbl.set tbl ~key:(x, y) ~data:c));
  let bestscore = ref 0 in
  let maxx = List.hd_exn ls |> String.length in
  let maxy = List.length ls in
  for x = 1 to maxx - 2 do
    for y = 1 to maxy - 2 do
      let h = Hashtbl.find_exn tbl (x, y) in
      let upct = ref 0 in
      let y2 = ref (y - 1) in
      while !y2 >= 0 do
        incr upct;
        if Char.(Hashtbl.find_exn tbl (x, !y2) < h) then decr y2 else y2 := -1
      done;
      let downct = ref 0 in
      y2 := y + 1;
      while !y2 < maxy do
        incr downct;
        if Char.(Hashtbl.find_exn tbl (x, !y2) < h) then incr y2 else y2 := maxy
      done;
      let leftct = ref 0 in
      let x2 = ref (x - 1) in
      while !x2 >= 0 do
        incr leftct;
        if Char.(Hashtbl.find_exn tbl (!x2, y) < h) then decr x2 else x2 := -1
      done;
      let rightct = ref 0 in
      x2 := x + 1;
      while !x2 < maxx do
        incr rightct;
        if Char.(Hashtbl.find_exn tbl (!x2, y) < h) then incr x2 else x2 := maxx
      done;
      bestscore := Int.max !bestscore (!upct * !downct * !leftct * !rightct)
    done
  done;
  !bestscore |> Printer.of_int

let sample = {|30373
25512
65332
33549
35390|} |> String.split ~on:'\n'

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| 21 |}]

let%expect_test "b" =
  partb sample |> Printer.print;
  [%expect {| 8 |}]
