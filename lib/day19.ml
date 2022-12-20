open! Containers
open! Core
open! Types

module State = struct
  module T = struct
    (* minute, robots, resources *)
    (* Always ore, then clay, then obsidian, then geodes *)
    type t = int * int list * int list [@@deriving compare, hash, sexp]
  end

  include T
  include Hashable.Make (T)

  let geodes r = List.nth_exn r 3

  let geodes_by (minute, robots, resources) ~time =
    let current_geodes = geodes resources in
    let geode_robots = geodes robots in
    current_geodes + ((time - minute) * geode_robots)

  let max_possible_geodes_by (minute, robots, resources) ~time =
    let baseline = geodes_by (minute, robots, resources) ~time in
    let remaining = time - minute in
    baseline + (((remaining * remaining) - 1) / 2)
end

let time_for_resource target current robots =
  if current >= target then Some 0
  else
    match robots with
    | 0 -> None
    | rs ->
        (* Integer division rounded up *)
        let to_make = target - current in
        let to_add = if to_make % rs = 0 then 0 else 1 in
        to_add + (to_make / rs) |> Option.some

let solve ls minutes =
  List.map ls ~f:(fun line ->
      let o_ore, c_ore, b_ore, b_clay, g_ore, g_obs =
        match Input.token_ints line with
        | [ b_ore; c_ore; o_ore; o_clay; g_ore; g_obs ] ->
            (b_ore, c_ore, o_ore, o_clay, g_ore, g_obs)
        | v ->
            failwithf "bad: %s %s" line
              (List.map v ~f:Int.to_string |> String.concat ~sep:",")
              ()
      in
      let recipes =
        [
          [ o_ore; 0; 0; 0 ];
          [ c_ore; 0; 0; 0 ];
          [ b_ore; b_clay; 0; 0 ];
          [ g_ore; 0; g_obs; 0 ];
        ]
      in
      let max_robot =
        (* No limit on geode robots *)
        List.init 3 ~f:(fun i ->
            List.fold recipes ~init:0 ~f:(fun m rs ->
                List.nth_exn rs i |> Int.max m))
        @ [ Int.max_value ]
      in
      let successors (minute, robots, resources) =
        (* Assume we can build at most one robot per round. *)
        (* For each recipe... *)
        List.filter_mapi recipes ~f:(fun robot_i recipe ->
            let rtc_by_resource = Types.zip3_exn robots recipe resources in
            let open Option.Let_syntax in
            (* Do we need more of that robot? *)
            let%bind () =
              match
                List.nth_exn robots robot_i < List.nth_exn max_robot robot_i
              with
              | true -> return ()
              | false -> None
            in
            (* Can we build that robot in time with the robots we have? *)
            let%bind time =
              List.map rtc_by_resource ~f:(fun (robot_ct, target, current) ->
                  time_for_resource target current robot_ct)
              |> Option.all
              >>= List.max_elt ~compare:Int.compare
            in
            let%map minute =
              match minute + time < minutes with
              | true -> Some (minute + time + 1)
              | false -> None
            in
            (* If so, emit a state where we build that robot. *)
            ( minute,
              List.mapi robots ~f:(fun i rc ->
                  if i = robot_i then rc + 1 else rc),
              List.map rtc_by_resource ~f:(fun (robot_ct, target, current) ->
                  current - target + (robot_ct * (time + 1))) ))
      in
      let seen = State.Hash_set.create () in
      let rec seek geodes = function
        | [] -> geodes
        | q ->
            let geodes, q =
              List.fold q ~init:(geodes, []) ~f:(fun (geodes, q) state ->
                  match
                    Hash_set.mem seen state
                    || State.max_possible_geodes_by state ~time:minutes < geodes
                  with
                  | true -> (geodes, q)
                  | false ->
                      Hash_set.add seen state;
                      let geodes =
                        State.geodes_by state ~time:minutes |> Int.max geodes
                      in
                      let new_q = successors state in
                      (geodes, new_q :: q))
            in
            let q = List.concat q in
            seek geodes q
      in
      seek 0 [ (0, [ 1; 0; 0; 0 ], [ 0; 0; 0; 0 ]) ])

let parta ls =
  solve ls 24
  |> List.mapi ~f:(fun i score -> (i + 1) * score)
  |> List.sum (module Int) ~f:Fn.id
  |> Printer.of_int

let partb ls =
  solve (List.take ls 3) 32
  |> List.fold ~init:1 ~f:(fun p s -> p * s)
  |> Printer.of_int

(* These tests take a long time to run. *)
(* let sample =
  {|Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.|}
  |> String.split ~on:'\n'

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| 33 |}]

let%expect_test "b" =
  partb sample |> Printer.print;
  [%expect {| 3472 |}] *)
