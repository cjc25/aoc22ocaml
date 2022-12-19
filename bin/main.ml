open! Core
open! Core_bench
open! Async
open Aoc22ocaml
open Types

module type Day = sig
  val parta : string list -> Printer.t
  val partb : string list -> Printer.t
end

let daymods =
  [|
    (module Day1 : Day);
    (module Day2);
    (module Day3);
    (module Day4);
    (module Day5);
    (module Day6);
    (module Day7);
    (module Day8);
    (module Day9);
    (module Day10);
    (module Day11);
    (module Day12);
    (module Day13);
    (module Day14);
    (module Day15);
    (module Day16);
    (module Day17);
    (module Day18);
  |]

type day_to_run = { day : (module Day); infile : string; part : [ `A | `B ] }

let day_arg =
  Command.Arg_type.create (fun s ->
      let daystr = String.length s - 1 |> String.prefix s in
      let day = Int.of_string daystr - 1 |> Array.get daymods in
      let infile = "day" ^ daystr in
      let part =
        match String.suffix s 1 with
        | "a" | "A" -> `A
        | "b" | "B" -> `B
        | _ -> failwithf "bad day arg %s" s ()
      in
      { day; infile; part })

let day_param =
  let open Command.Param in
  flag ~doc:"day part" "day" (one_or_more_as_list day_arg)

let base_dir_param =
  let open Command.Param in
  flag ~doc:"input files directory" "input-dir"
    (optional_with_default "inputs" string)

let runday b { day = (module D); infile; part } =
  let f = Filename.concat b infile in
  let%map lines = Reader.file_lines f in
  match part with
  | `A -> D.parta lines |> Printer.print
  | `B -> D.partb lines |> Printer.print

let aoc_cmd =
  Command.async ~summary:"run advent of code!"
    (let%map_open.Command d = day_param and base_dir = base_dir_param in
     fun () -> Deferred.List.iter d ~f:(fun d -> runday base_dir d))

(** [make_benchmark basedir day_to_run] returns a benchmark for [Bench.Test.create_with_initialization] *)
let make_benchmark b { day = (module D); infile; part } =
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