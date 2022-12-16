open! Containers
open! Core
open! Types

module LToFro = struct
  module T = struct
    type t = string * string [@@deriving compare, hash, sexp]
  end

  include T
  include Hashable.Make (T)
end

module DPK = struct
  module T = struct
    type t = string * string list [@@deriving compare, hash, sexp]
  end

  include T
  include Hashable.Make (T)
end

let build_dp ls =
  let flows = String.Table.create () in
  let dists = LToFro.Table.create () in
  let ns =
    List.map ls ~f:(fun s ->
        let l, lflow =
          Scanf.sscanf s "Valve %s has flow rate=%d; %s" (fun n r _ -> (n, r))
        in
        let ts =
          Input.split_on_string s ~sep:", "
          |> List.map ~f:(fun e -> String.suffix e 2)
        in
        Hashtbl.set flows ~key:l ~data:lflow;
        Hashtbl.set dists ~key:(l, l) ~data:0;
        List.iter ts ~f:(fun t -> Hashtbl.set dists ~key:(l, t) ~data:1);
        l)
  in
  (* Floyd-Warshall *)
  List.iter ns ~f:(fun k ->
      List.iter ns ~f:(fun i ->
          List.iter ns ~f:(fun j ->
              match
                ( Hashtbl.find dists (i, j),
                  Option.both
                    (Hashtbl.find dists (i, k))
                    (Hashtbl.find dists (k, j)) )
              with
              | _, None -> ()
              | e, Some (ik, kj) ->
                  let c = ik + kj in
                  Hashtbl.set dists ~key:(i, j)
                    ~data:(Option.value_map e ~default:c ~f:(Int.min c)))));
  (* Drop non-AA, zero-flow locations *)
  let good_valve l f = String.(l = "AA") || f > 0 in
  Hashtbl.filter_keys_inplace dists ~f:(fun (i, j) ->
      let if_ = Hashtbl.find_exn flows i in
      let jf = Hashtbl.find_exn flows j in
      good_valve i if_ && good_valve j jf);
  (* A Set of all non-zero flow locations *)
  let valid_valves =
    Hashtbl.fold flows ~init:String.Set.empty ~f:(fun ~key:l ~data:f s ->
        if f > 0 then Set.add s l else s)
  in
  (* DP: the end of each minute has a table of max seen (loc, on) -> cumulative
     flow. Work forward by minutes. *)
  let total_mins = 30 in
  let dp = Array.init (total_mins + 1) ~f:(fun _ -> DPK.Table.create ()) in
  Hashtbl.set dp.(0) ~key:("AA", []) ~data:0;
  for i = 0 to total_mins - 1 do
    Hashtbl.iteri dp.(i) ~f:(fun ~key:(l, on) ~data:cf ->
        let fr =
          List.sum (module Int) on ~f:(fun v -> Hashtbl.find_exn flows v)
        in
        (* Do nothing *)
        Hashtbl.update
          dp.(i + 1)
          (l, on)
          ~f:
            (Option.value_map ~default:(cf + fr) ~f:(fun ef ->
                 Int.max ef (cf + fr)));
        (* Turn on a new valve *)
        let onset = String.Set.of_list on in
        Set.diff valid_valves onset
        |> Set.iter ~f:(fun v ->
               let t = Hashtbl.find_exn dists (l, v) + 1 in
               if t + i > total_mins then ()
               else
                 let new_on = Set.add onset v |> Set.to_list in
                 let newcf = cf + (fr * t) in
                 Hashtbl.update
                   dp.(t + i)
                   (v, new_on)
                   ~f:
                     (Option.value_map ~default:newcf ~f:(fun ef ->
                          Int.max ef newcf))))
  done;
  dp

let parta ls =
  let dp = build_dp ls in
  Hashtbl.fold dp.(30) ~init:0 ~f:(fun ~key:_ ~data:f mf -> Int.max f mf)
  |> Printer.of_int

let partb ls =
  let dp = build_dp ls in
  (* For all pairs of results after minute 26 with disjoint ons, take the max.
     We do that by sorting them, get the best f1, f2 pair where f1 >= f2. *)
  let paths_desc =
    Hashtbl.to_alist dp.(26)
    |> List.map ~f:(fun ((l, on), f) -> (String.Set.of_list on, f))
    |> List.sort ~compare:(fun (_, f1) (_, f2) -> Int.compare f2 f1)
  in
  let rec find_best_in_tl on1 tl =
    match tl with
    | [] -> 0 (* No usable paths *)
    | (on2, f2) :: tl ->
        if Set.inter on1 on2 |> Set.is_empty then f2 else find_best_in_tl on1 tl
  in
  let rec find_best (bf1, bf2) = function
    | [] -> (bf1, bf2)
    | (on1, f1) :: tl ->
        if f1 <= bf2 then (bf1, bf2)
        else
          let f2 = find_best_in_tl on1 tl in
          let bf1, bf2 = if bf1 + bf2 < f1 + f2 then (f1, f2) else (bf1, bf2) in
          find_best (bf1, bf2) tl
  in
  let bf1, bf2 = find_best (0, 0) paths_desc in
  bf1 + bf2 |> Printer.of_int

let sample =
  {|Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II|}
  |> String.split ~on:'\n'

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| 1651 |}]

let%expect_test "b" =
  partb sample |> Printer.print;
  [%expect {| 1707 |}]
