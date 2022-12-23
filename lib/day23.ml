open! Containers
open! Core
open! Types

let nnabes (x, y) = [ (x - 1, y - 1); (x, y - 1); (x + 1, y - 1) ]
let snabes (x, y) = [ (x - 1, y + 1); (x, y + 1); (x + 1, y + 1) ]
let wnabes (x, y) = [ (x - 1, y - 1); (x - 1, y); (x - 1, y + 1) ]
let enabes (x, y) = [ (x + 1, y - 1); (x + 1, y); (x + 1, y + 1) ]

let mkgrid ls =
  let grid = Xy.Hash_set.create () in
  List.iteri ls ~f:(fun y line ->
      String.iteri line ~f:(fun x c ->
          match c with
          | '#' -> Hash_set.add grid (x, y)
          | '.' -> ()
          | _ -> failwith "bad line"));
  grid

let round grid dirs =
  (* key is proposed position, val is list of source positions *)
  let proposals = Xy.Table.create () in
  Hash_set.iter grid ~f:(fun elfpos ->
      match
        List.for_all dirs ~f:(fun nf ->
            List.count (nf elfpos) ~f:(Hash_set.mem grid) = 0)
      with
      | true -> Hashtbl.add_exn proposals ~key:elfpos ~data:[ elfpos ]
      | false ->
          let p =
            List.fold_until dirs ~init:None
              ~finish:(fun _ -> elfpos)
              ~f:(fun _ nabes ->
                let ns = nabes elfpos in
                match List.count ns ~f:(Hash_set.mem grid) with
                | 0 -> Stop (List.nth_exn ns 1)
                | _ -> Continue None)
          in
          Hashtbl.update proposals p ~f:(function
            | None -> [ elfpos ]
            | Some ps -> elfpos :: ps));
  let newgrid = Xy.Hash_set.create () in
  Hashtbl.iteri proposals ~f:(fun ~key:target ~data:srcs ->
      match srcs with
      | [ _ ] -> Hash_set.add newgrid target
      | srcs -> List.iter srcs ~f:(Hash_set.add newgrid));
  let hd, tl = List.split_n dirs 1 in
  (newgrid, tl @ hd)

let parta ls =
  let grid = ref (mkgrid ls) in
  let dirs = ref [ nnabes; snabes; wnabes; enabes ] in
  for _ = 1 to 10 do
    let ng, nd = round !grid !dirs in
    grid := ng;
    dirs := nd
  done;
  let minx, maxx, miny, maxy =
    Hash_set.fold !grid ~init:(0, 0, 0, 0)
      ~f:(fun (minx, maxx, miny, maxy) (x, y) ->
        (Int.min minx x, Int.max maxx x, Int.min miny y, Int.max maxy y))
  in
  ((maxx - minx + 1) * (maxy - miny + 1)) - Hash_set.length !grid
  |> Printer.of_int

let grids_equal a b =
  Hash_set.for_all a ~f:(Hash_set.mem b)
  && Hash_set.for_all b ~f:(Hash_set.mem a)

let partb ls =
  let grid = mkgrid ls in
  let dirs = [ nnabes; snabes; wnabes; enabes ] in
  let rec tilnomove i grid dirs =
    let ng, nd = round grid dirs in
    if grids_equal grid ng then i else tilnomove (i + 1) ng nd
  in
  tilnomove 1 grid dirs |> Printer.of_int

let sample =
  {|....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..|}
  |> String.split ~on:'\n'

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| 110 |}]

let%expect_test "b" =
   partb sample |> Printer.print;
   [%expect {| 20 |}]
