open! Core
open! Core_bench
open! Async
open Aoc22ocaml

module type Day = sig
  type result

  val parta : string list -> result
  val partb : string list -> result
end

module type Printing_day = sig
  val parta : string list -> unit
  val partb : string list -> unit
end

module Of_unit_day (D : Day with type result = unit) : Printing_day = struct
  let parta = D.parta
  let partb = D.partb
end

module Of_int_day (D : Day with type result = int) : Printing_day = struct
  let parta s = D.parta s |> Int.to_string |> print_endline
  let partb s = D.partb s |> Int.to_string |> print_endline
end

module Of_string_day (D : Day with type result = string) : Printing_day = struct
  let parta s = D.parta s |> print_endline
  let partb s = D.partb s |> print_endline
end

let daymods =
  Int.Map.of_alist_exn
    [
      (1, ((module Day1 : Day), (module Of_int_day (Day1) : Printing_day)));
      (2, ((module Day2), (module Of_int_day (Day2))));
      (3, ((module Day3), (module Of_int_day (Day3))));
      (4, ((module Day4), (module Of_int_day (Day4))));
      (5, ((module Day5), (module Of_string_day (Day5))));
      (6, ((module Day6), (module Of_int_day (Day6))));
      (7, ((module Day7), (module Of_int_day (Day7))));
      (8, ((module Day8), (module Of_int_day (Day8))));
      (9, ((module Day9), (module Of_int_day (Day9))));
    ]

type day_to_run = {
  day : (module Day);
  printing_day : (module Printing_day);
  infile : string;
  part : [ `A | `B ];
}

let day_arg =
  Command.Arg_type.create (fun s ->
      let daystr = String.length s - 1 |> String.prefix s in
      let day, printing_day = Int.of_string daystr |> Map.find_exn daymods in
      let infile = "day" ^ daystr in
      let part =
        match String.suffix s 1 with
        | "a" | "A" -> `A
        | "b" | "B" -> `B
        | _ -> failwithf "bad day arg %s" s ()
      in
      { day; printing_day; infile; part })

let day_param =
  let open Command.Param in
  flag ~doc:"day part" "day" (one_or_more_as_list day_arg)

let base_dir_param =
  let open Command.Param in
  flag ~doc:"input files directory" "input-dir"
    (optional_with_default "inputs" string)

let runday b { day = _; printing_day = (module D); infile; part } =
  let f = Filename.concat b infile in
  let%map lines = Reader.file_lines f in
  match part with `A -> D.parta lines | `B -> D.partb lines

let aoc_cmd =
  Command.async ~summary:"run advent of code!"
    (let%map_open.Command d = day_param and base_dir = base_dir_param in
     fun () -> Deferred.List.iter d ~f:(fun d -> runday base_dir d))

(** [bm_of_day basedir day_to_run] returns a benchmark for [Bench.Test.create_with_initialization] *)
let make_benchmark b { day = (module D); printing_day = _; infile; part } =
  let partstr = match part with `A -> "a" | `B -> "b" in
  let name = infile ^ partstr in
  let f = match part with `A -> D.parta | `B -> D.partb in
  Bench.Test.create_with_initialization ~name (fun `init ->
      let infile = Filename.concat b infile in
      let lines = Stdio.In_channel.read_lines infile in
      fun () -> f lines)

let display_config =
  Bench.Display_config.create ~display:Ascii_table.Display.column_titles
    ~ascii_table:true ()

let bm_cmd =
  Command.async ~summary:"benchmark advent of code days"
    (let%map_open.Command d = day_param and base_dir = base_dir_param in
     fun () ->
       List.map d ~f:(make_benchmark base_dir)
       |> Bench.bench ~display_config
       |> return)

let () =
  Command.group ~summary:"Run once or benchmark the advent of code"
    [ ("aoc", aoc_cmd); ("bm", bm_cmd) ]
  |> Command_unix.run