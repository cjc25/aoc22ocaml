open! Core

type result = unit

let reg_at_cycles ls =
  List.folding_map ls ~init:1 ~f:(fun reg l ->
      match String.split l ~on:' ' with
      | [ "noop" ] -> (reg, [ reg ])
      | [ "addx"; sz ] -> (reg + Int.of_string sz, [ reg; reg ])
      | _ -> failwith "bad line")
  |> List.concat

let parta ls =
  reg_at_cycles ls
  |> List.foldi ~init:0 ~f:(fun i sum reg ->
         if (i + 1 - 20) % 40 = 0 then sum + (reg * (i + 1))
         else sum)
  |> Int.to_string |> print_endline

let partb ls =
  reg_at_cycles ls
  |> List.iteri ~f:(fun i c ->
         let p = i % 40 in
         if p = c - 1 || p = c || p = c + 1 then print_string "#"
         else print_string ".";
         if (i + 1) % 40 = 0 then print_endline "" else ())

let sample =
  {|addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop|}
  |> String.split ~on:'\n'

let%expect_test "a" =
  parta sample;
  [%expect {| 13140 |}]

let%expect_test "b" =
  partb sample;
  [%expect {|
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....
   |}]
