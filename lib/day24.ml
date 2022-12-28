open! Containers
open! Core
open! Types

let parse ls =
  let grid = Xy.Table.create () in
  List.iteri ls ~f:(fun y line ->
      String.iteri line ~f:(fun x -> function
        | ('<' as d) | ('>' as d) | ('^' as d) | ('v' as d) ->
            Hashtbl.add_exn grid ~key:(x - 1, y - 1) ~data:d
        | '.' | '#' -> ()
        | _ -> failwith "bad char"));
  (grid, ((List.hd_exn ls |> String.length) - 3, List.length ls - 2))

module State = struct
  module T = struct
    type t = Xy.t * int [@@deriving compare, hash, sexp_of]
  end

  include T
  include Hashable.Make_plain (T)
end

let in_grid (x, y) ~width ~height =
  Xy.equal (x, y) (0, -1)
  || Xy.equal (x, y) (width - 1, height)
  || (x >= 0 && x <= width - 1 && y >= 0 && y < height)

let nabes (x, y) ~width ~height =
  List.filter
    [ (x, y); (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]
    ~f:(fun p -> in_grid p ~width ~height)

let has_blizzval grid key value =
  Hashtbl.find grid key
  |> Option.value_map ~default:false ~f:(fun d -> Char.equal value d)

let for_safe_nabes grid ~width ~height p newm ~f =
  let ns = nabes p ~width ~height in
  List.iter ns ~f:(fun (x, y) ->
      match
        has_blizzval grid ((x + newm) % width, y) '<'
        || has_blizzval grid ((x - newm) % width, y) '>'
        || has_blizzval grid (x, (y + newm) % height) '^'
        || has_blizzval grid (x, (y - newm) % height) 'v'
      with
      | true -> ()
      | false -> f (x, y))

let minutes_at_exit grid ~start ~exit ~width ~height start_min =
  let cycle = width * height in
  let seen = State.Hash_set.create () in
  let q = Deque.create () in
  Deque.enqueue_back q (start, start_min);
  let minute = ref start_min in
  while Deque.is_empty q |> not do
    let p, m = Deque.dequeue_front_exn q in
    let c = m % cycle in
    match (Hash_set.mem seen (p, c), Xy.equal p exit) with
    | _, true ->
        minute := m;
        Deque.clear q
    | true, _ -> ()
    | false, false ->
        Hash_set.add seen (p, c);
        for_safe_nabes grid ~width ~height p (m + 1) ~f:(fun p ->
            Deque.enqueue_back q (p, m + 1))
  done;
  !minute

let parta ls =
  let grid, ((exitx, exity) as exit) = parse ls in
  let width, height = (exitx + 1, exity) in
  minutes_at_exit grid ~start:(0, -1) ~exit ~width ~height 0 |> Printer.of_int

let partb ls =
  let grid, ((exitx, exity) as exit) = parse ls in
  let width, height = (exitx + 1, exity) in
  let m2e = minutes_at_exit grid ~start:(0, -1) ~exit ~width ~height 0 in
  let m2s = minutes_at_exit grid ~start:exit ~exit:(0, -1) ~width ~height m2e in
  let mb2e = minutes_at_exit grid ~start:(0, -1) ~exit ~width ~height m2s in
  Printer.of_int mb2e

let sample =
  {|#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#|}
  |> String.split ~on:'\n'

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| 18 |}]

let%expect_test "b" =
  partb sample |> Printer.print;
  [%expect {| 54 |}]
