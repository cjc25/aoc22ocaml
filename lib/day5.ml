open! Core
open Types

let buildstacks ls =
  let width = (List.hd_exn ls |> String.length |> succ) / 4 in
  let rev = Array.create ~len:width [] in
  List.iter ls ~f:(fun line ->
      let carr = String.to_array line in
      for i = 0 to width - 1 do
        let c = carr.((i * 4) + 1) in
        if Char.(c <> ' ') then rev.(i) <- c :: rev.(i) else ()
      done);
  Array.map rev ~f:List.rev

let buildinstrs =
  List.map ~f:(fun line ->
      match String.split line ~on:' ' with
      | [ _; ct; _; src; _; dst ] ->
          (Int.of_string ct, Int.of_string src - 1, Int.of_string dst - 1)
      | _ -> failwith "bad instruction line")

let preprocess ls =
  match Input.to_sections ls with
  | [ stacks; instrs ] -> (buildstacks stacks, buildinstrs instrs)
  | _ -> failwith "bad input"

let parta ls =
  let stacks, instrs = preprocess ls in
  List.iter instrs ~f:(fun (ct, src, dst) ->
      for _ = 1 to ct do
        let hd, tl = List.split_n stacks.(src) 1 in
        stacks.(dst) <- hd @ stacks.(dst);
        stacks.(src) <- tl
      done);
  Array.map stacks ~f:List.hd_exn
  |> Array.to_list |> String.of_char_list |> Printer.of_string

let partb ls =
  let stacks, instrs = preprocess ls in
  List.iter instrs ~f:(fun (ct, src, dst) ->
      let hd, tl = List.split_n stacks.(src) ct in
      stacks.(dst) <- hd @ stacks.(dst);
      stacks.(src) <- tl);
  Array.map stacks ~f:List.hd_exn
  |> Array.to_list |> String.of_char_list |> Printer.of_string

let sample =
  {|    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2|}
  |> String.split ~on:'\n'

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| CMZ |}]

let%expect_test "b" =
  partb sample |> Printer.print;
  [%expect {| MCD |}]
