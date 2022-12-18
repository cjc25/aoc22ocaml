open! Containers
open! Core
open! Types

(* Bottom to top *)
let shape i =
  match i % 5 with
  | 0 -> [ Int.Set.of_list [ 2; 3; 4; 5 ] ]
  | 1 ->
      [
        Int.Set.of_list [ 3 ];
        Int.Set.of_list [ 2; 3; 4 ];
        Int.Set.of_list [ 3 ];
      ]
  | 2 ->
      [
        Int.Set.of_list [ 2; 3; 4 ];
        Int.Set.of_list [ 4 ];
        Int.Set.of_list [ 4 ];
      ]
  | 3 ->
      [
        Int.Set.of_list [ 2 ];
        Int.Set.of_list [ 2 ];
        Int.Set.of_list [ 2 ];
        Int.Set.of_list [ 2 ];
      ]
  | 4 -> [ Int.Set.of_list [ 2; 3 ]; Int.Set.of_list [ 2; 3 ] ]
  | _ -> failwith "unreachable"

module Observed = struct
  module T = struct
    type t = { si : int; ji : int; lines : int list list }
    [@@deriving compare, hash, sexp]
  end

  include T
  include Hashable.Make (T)
end

let simulate target_oneindexed_i pushes =
  (* When do we have a repeat of shapei, jeti, and top N lines? *)
  let push_ct = String.length pushes in
  let seen = Observed.Table.create () in
  let hts = Int.Table.create () in
  let fsi, fht, cl, ch =
    let rec find_cycle si ji tower =
      let rec drop cmp ji tower shape =
        let adj =
          match String.get pushes (ji % push_ct) with
          | '<' -> -1
          | '>' -> 1
          | _ -> failwith "unexpected"
        in
        let shapeht = List.length shape in
        (* Sideways *)
        let shapec = List.map shape ~f:(Int.Set.map ~f:(fun v -> v + adj)) in
        let ok =
          List.zip_exn shapec (List.take cmp shapeht)
          |> List.for_all ~f:(fun (s, c) ->
                 Set.min_elt_exn s >= 0
                 && Set.max_elt_exn s <= 6
                 && Set.inter s c |> Set.is_empty)
        in
        let shape = if ok then shapec else shape in
        (* Down *)
        let cd, td = (List.hd_exn tower :: cmp, List.tl_exn tower) in
        let ok =
          List.zip_exn shape (List.take cd shapeht)
          |> List.for_all ~f:(fun (s, c) -> Set.inter s c |> Set.is_empty)
        in
        match ok with
        | true -> drop cd (ji + 1) td shape
        | false ->
            (* done! union shape into cmp and put back on tower *)
            let overlap, tl = List.split_n cmp shapeht in
            let overlap =
              List.zip_exn shape overlap
              |> List.map ~f:(fun (s, o) -> Set.union s o)
            in
            let tower =
              List.rev_append tl (List.rev_append overlap tower)
              |> List.filter ~f:(fun s -> Set.is_empty s |> not)
            in
            (ji + 1, tower)
      in
      let shape = shape (si % 5) in
      let tower = List.init 3 ~f:(fun _ -> Int.Set.empty) @ tower in
      let cmp = List.init (List.length shape) ~f:(fun _ -> Int.Set.empty) in
      let nji, tower = drop cmp ji tower shape in
      (* after dropping it, have we ever generated this pattern of top N lines
         at this point in the shape/jet cycles? *)
      (* Use N = 100, empirically this seems to work with the samples. *)
      let key =
        {
          Observed.si = si % 5;
          ji = ji % push_ct;
          lines = List.take tower 100 |> List.map ~f:Set.to_list;
        }
      in
      match Hashtbl.find seen key with
      | Some (fsi, ht) ->
          (* found a cycle! *) (fsi, ht, si - fsi, List.length tower - 1 - ht)
      | None ->
          (* nope, update seen and keep digging *)
          Hashtbl.set seen ~key ~data:(si, List.length tower - 1);
          Hashtbl.set hts ~key:si ~data:(List.length tower - 1);
          find_cycle (si + 1) nji tower
    in
    find_cycle 0 0 [ List.init 7 ~f:Fn.id |> Int.Set.of_list ]
  in
  (* T = first_shape_i + (cycle_len * K) + offset_in_cycle *)
  (* ht_at(T) = (ht_per_cycle * K) + (ht_at(fsi_plus_offset_in_cycle)) *)
  (* when we find a cycle, we can find K via:
     offset_in_cycle = (T - first_shape_i) % cycle_len
     K = (T - first_shape_i - offset_in_cycle) / cycle_len *)
  (* N.B. simulation is zero-indexed *)
  let t = target_oneindexed_i - 1 in
  let offset_in_cycle = (t - fsi) % cl in
  let k = (t - fsi - offset_in_cycle) / cl in
  Hashtbl.find_exn hts (offset_in_cycle + fsi) + (k * ch)

let parta ls = simulate 2022 (List.hd_exn ls) |> Printer.of_int
let partb ls = simulate 1000000000000 (List.hd_exn ls) |> Printer.of_int

let sample =
  {|>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>|} |> String.split ~on:'\n'

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| 3068 |}]

let%expect_test "b" =
  partb sample |> Printer.print;
  [%expect {| 1514285714288 |}]
