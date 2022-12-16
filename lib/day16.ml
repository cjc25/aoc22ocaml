open! Containers
open! Core
open! Types

module DState = struct
  type t = {
    cflow : int;
    flow : int;
    loc : string;
    on : String.Set.t;
    minute : int;
  }
end

module Loc = struct
  type t = { lflow : int; ts : string list }
end

module PosSteps = struct
  module T = struct
    type t = { l : string; s : int } [@@deriving compare, hash, sexp]
  end

  include T
  include Hashable.Make (T)
end

let parta ls =
  let g = String.Table.create () in
  List.iter ls ~f:(fun s ->
      let l, r =
        match Input.split_on_string s ~sep:" valve" with
        | [ l; r ] ->
            ( l,
              r
              |> String.chop_prefix_if_exists ~prefix:"s"
              |> String.chop_prefix_if_exists ~prefix:" " )
        | _ -> failwithf "bad line %s" s ()
      in
      let ts = Input.split_on_string r ~sep:", " in
      let l, lflow =
        Scanf.sscanf l "Valve %s has flow rate=%d; %s" (fun n r _ -> (n, r))
      in
      Hashtbl.set g ~key:l ~data:{ Loc.lflow; ts });
  let bestat = PosSteps.Table.create () in
  let rec seek max = function
    | [] -> max
    | { DState.cflow; flow; loc; on; minute } :: tl -> (
        let cflow = cflow + flow in
        let max = Int.max max cflow in
        if minute = 30 then seek max tl
        else
          match
            Hashtbl.find bestat { l = loc; s = minute }
            |> Option.value_map ~default:true ~f:(fun bestflow ->
                   flow >= bestflow)
          with
          | false -> seek max tl
          | true ->
              Hashtbl.set bestat ~key:{ l = loc; s = minute } ~data:flow;
              let { Loc.lflow; ts } = Hashtbl.find_exn g loc in
              let on_here =
                match Set.mem on loc || lflow = 0 with
                | true ->
                    [ { DState.cflow; flow; loc; on; minute = minute + 1 } ]
                | false ->
                    [
                      {
                        DState.cflow;
                        flow = flow + lflow;
                        loc;
                        on = Set.add on loc;
                        minute = minute + 1;
                      };
                    ]
              in
              let moves =
                List.map ts ~f:(fun loc ->
                    { DState.cflow; flow; loc; on; minute = minute + 1 })
              in
              let news = on_here @ moves @ tl in
              seek max news)
  in
  seek 0
    [ { cflow = 0; flow = 0; loc = "AA"; on = String.Set.empty; minute = 1 } ]
  |> Printer.of_int

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

let partb ls =
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
  (* DP: each minute has a table of max seen (loc, on) -> cumulative flow. Work forward by minutes. *)
  let total_mins = 27 in
  let dp = Array.init total_mins ~f:(fun _ -> DPK.Table.create ()) in
  Hashtbl.set dp.(0) ~key:("AA", []) ~data:0;
  for i = 0 to total_mins - 2 do
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
               if t + i >= total_mins then ()
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
  (* Finally, for all pairs of results at minute 26 (index 25) with disjoint ons, take the max. *)
  (* sort them, get the best f1, f2 pair where f1 >= f2. *)
  let paths_desc =
    Hashtbl.to_alist dp.(total_mins - 1)
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
