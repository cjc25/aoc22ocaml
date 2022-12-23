open! Containers
open! Core
open! Types

let nnabes (x, y) = [ (x - 1, y - 1); (x, y - 1); (x + 1, y - 1) ]
let snabes (x, y) = [ (x - 1, y + 1); (x, y + 1); (x + 1, y + 1) ]
let wnabes (x, y) = [ (x - 1, y - 1); (x - 1, y); (x - 1, y + 1) ]
let enabes (x, y) = [ (x + 1, y - 1); (x + 1, y); (x + 1, y + 1) ]

let nabes (x, y) =
  [
    (x - 1, y - 1);
    (x - 1, y);
    (x - 1, y + 1);
    (x, y + 1);
    (x, y - 1);
    (x + 1, y - 1);
    (x + 1, y);
    (x + 1, y + 1);
  ]

let add (x, y) (dx, dy) = (x + dx, y + dy)

let parta ls =
  let grid = Xy.Hash_set.create () in
  List.iteri ls ~f:(fun y line ->
      String.iteri line ~f:(fun x c ->
          match c with
          | '#' -> Hash_set.add grid (x, y)
          | '.' -> ()
          | _ -> failwith "bad line"));
  let grid = ref grid in
  let dirs = ref [ nnabes; snabes; wnabes; enabes ] in
  for _ = 1 to 10 do
    (* key is proposed position, val is list of source positions *)
    let proposals = Xy.Table.create () in
    Hash_set.iter !grid ~f:(fun elfpos ->
        if List.count (nabes elfpos) ~f:(Hash_set.mem !grid) = 0 then
          Hashtbl.add_exn proposals ~key:elfpos ~data:[ elfpos ]
        else
          let p =
            List.fold_until !dirs ~init:None
              ~finish:(fun _ -> elfpos)
              ~f:(fun _ nabes ->
                let ns = nabes elfpos in
                match List.count ns ~f:(Hash_set.mem !grid) with
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
    grid := newgrid;
    let hd, tl = List.split_n !dirs 1 in
    dirs := tl @ hd
  done;
  let minx, maxx, miny, maxy =
    Hash_set.fold !grid ~init:(0, 0, 0, 0)
      ~f:(fun (minx, maxx, miny, maxy) (x, y) ->
        (Int.min minx x, Int.max maxx x, Int.min miny y, Int.max maxy y))
  in
  ((maxx - minx + 1) * (maxy - miny + 1)) - Hash_set.length !grid
  |> Printer.of_int

let gridseq a b =
  Hash_set.for_all a ~f:(Hash_set.mem b)
  && Hash_set.for_all b ~f:(Hash_set.mem a)

let partb ls =
  let grid = Xy.Hash_set.create () in
  List.iteri ls ~f:(fun y line ->
      String.iteri line ~f:(fun x c ->
          match c with
          | '#' -> Hash_set.add grid (x, y)
          | '.' -> ()
          | _ -> failwith "bad line"));
  let grid = ref grid in
  let dirs = ref [ nnabes; snabes; wnabes; enabes ] in
  let rec tilnomove i =
    (* key is proposed position, val is list of source positions *)
    let proposals = Xy.Table.create () in
    Hash_set.iter !grid ~f:(fun elfpos ->
        if List.count (nabes elfpos) ~f:(Hash_set.mem !grid) = 0 then
          Hashtbl.add_exn proposals ~key:elfpos ~data:[ elfpos ]
        else
          let p =
            List.fold_until !dirs ~init:None
              ~finish:(fun _ -> elfpos)
              ~f:(fun _ nabes ->
                let ns = nabes elfpos in
                match List.count ns ~f:(Hash_set.mem !grid) with
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
    if gridseq !grid newgrid then i
    else (
      grid := newgrid;
      let hd, tl = List.split_n !dirs 1 in
      dirs := tl @ hd;
      tilnomove (i + 1))
  in
  tilnomove 1 |> Printer.of_int

let _sample =
  {|....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..|}
  |> String.split ~on:'\n'

let%expect_test "a" =
  parta _sample |> Printer.print;
  [%expect {| 110 |}]

(* My grid is different from the sample's, so this test won't pass *)
(* let%expect_test "b" =
   partb sample |> Printer.print;
   [%expect {| 301 |}] *)
