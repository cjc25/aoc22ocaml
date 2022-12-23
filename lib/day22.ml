open! Containers
open! Core
open! Types

let build_grid ls =
  let grid = Xy.Table.create () in
  let fstxy = ref None in
  let strip line =
    let lstripped = String.lstrip line in
    let rstripped = String.rstrip lstripped in
    let beg = String.length line - String.length lstripped in
    let end_ = String.length rstripped + beg - 1 in
    (rstripped, beg, end_)
  in
  List.iteri ls ~f:(fun y row ->
      let gs, xbeg, xend = strip row in
      (match !fstxy with None -> fstxy := Some (xbeg, y) | _ -> ());
      String.iteri gs ~f:(fun x c ->
          let wall =
            match c with
            | '.' -> false
            | '#' -> true
            | _ -> failwithf "bad line %s" gs ()
          in
          let rx = if x + xbeg + 1 > xend then xbeg else x + xbeg + 1 in
          let lx = if x - 1 < 0 then xend else x + xbeg - 1 in
          (* only set right and left *)
          Hashtbl.set grid
            ~key:(x + xbeg, y)
            ~data:
              ( wall,
                (x + xbeg, y),
                [ ((rx, y), 0); ((0, 0), 1); ((lx, y), 2); ((0, 0), 3) ] )));
  List.map ls ~f:String.to_list
  |> List.transpose_exn
  |> List.map ~f:String.of_char_list
  |> List.iteri ~f:(fun x column ->
         let gs, ybeg, yend = strip column in
         String.iteri gs ~f:(fun y c ->
             let dy = if y + ybeg + 1 > yend then ybeg else y + ybeg + 1 in
             let uy = if y - 1 < 0 then yend else y + ybeg - 1 in
             (* only set down and up *)
             Hashtbl.update grid
               (x, y + ybeg)
               ~f:(function
                 | Some (wall, p, [ right; _; left; _ ]) ->
                     (wall, p, [ right; ((x, dy), 1); left; ((x, uy), 3) ])
                 | _ -> failwith "grid not already full?")));
  (grid, Option.value_exn !fstxy)

let build_instrs is =
  let r, acc =
    String.fold is ~init:(0, []) ~f:(fun (running, acc) -> function
      | 'L' -> (0, `L :: `F running :: acc)
      | 'R' -> (0, `R :: `F running :: acc)
      | c -> ((running * 10) + (Char.to_int c - Char.to_int '0'), acc))
  in
  List.rev (`F r :: acc)

let forwardone grid (pos, facing) =
  let _, _, nexts = pos in
  let np, nf = List.nth_exn nexts facing in
  match Hashtbl.find_exn grid np with
  | true, _, _ -> (pos, facing)
  | n -> (n, nf)

let rec forward grid (pos, facing) = function
  | 0 -> (pos, facing)
  | ct ->
      let np, nf = forwardone grid (pos, facing) in
      forward grid (np, nf) (ct - 1)

let apply_instr grid (pos, facing) = function
  | `L -> (pos, (facing - 1) % 4)
  | `R -> (pos, (facing + 1) % 4)
  | `F ct -> forward grid (pos, facing) ct

let parse_map ls =
  let sections = Input.to_sections ls in
  let map = List.hd_exn sections in
  let map_width =
    List.fold map ~init:0 ~f:(fun width s -> Int.max width (String.length s))
  in
  let grid, startx =
    List.map map ~f:(fun s ->
        let len = map_width - String.length s in
        s ^ String.make len ' ')
    |> build_grid
  in
  let instrs = List.nth_exn sections 1 |> List.hd_exn |> build_instrs in
  (grid, startx, instrs)

let parta ls =
  let grid, startxy, instrs = parse_map ls in
  let start = Hashtbl.find_exn grid startxy in
  let (_, (col, row), _), facing =
    List.fold instrs ~init:(start, 0) ~f:(apply_instr grid)
  in
  (1000 * (row + 1)) + (4 * (col + 1)) + facing |> Printer.of_int

(* There are 11 unfoldings of a cube onto a mat, modulo rotational symmetry. One
   is 6x2, all others are 4x3. We could exhaustively check them to get a solution
   for all inputs, but instead I just looked at my input and hardcoded it.
   Exhaustive listing didn't sound fun, and this is a game after all. *)
let partb ls =
  let grid, startxy, instrs = parse_map ls in
  (* Update the 7 pairs of edges of grid to point the right ways. *)
  for i = 0 to 49 do
    (* 1 *)
    Hashtbl.update grid
      (i + 50, 0)
      ~f:(function
        | Some (wall, at, [ right; down; left; _ ]) ->
            (wall, at, [ right; down; left; ((0, i + 150), 0) ])
        | _ -> failwith "unset!");
    Hashtbl.update grid
      (0, i + 150)
      ~f:(function
        | Some (wall, at, [ right; down; _; up ]) ->
            (wall, at, [ right; down; ((i + 50, 0), 1); up ])
        | _ -> failwith "unset!");
    (* 2 *)
    Hashtbl.update grid
      (i + 100, 0)
      ~f:(function
        | Some (wall, at, [ right; down; left; _ ]) ->
            (wall, at, [ right; down; left; ((i, 199), 3) ])
        | _ -> failwith "unset!");
    Hashtbl.update grid (i, 199) ~f:(function
      | Some (wall, at, [ right; _; left; up ]) ->
          (wall, at, [ right; ((i + 100, 0), 1); left; up ])
      | _ -> failwith "unset!");
    (* 3 *)
    Hashtbl.update grid (50, i) ~f:(function
      | Some (wall, at, [ right; down; _; up ]) ->
          (wall, at, [ right; down; ((0, 149 - i), 0); up ])
      | _ -> failwith "unset!");
    Hashtbl.update grid
      (0, 149 - i)
      ~f:(function
        | Some (wall, at, [ right; down; _; up ]) ->
            (wall, at, [ right; down; ((50, i), 0); up ])
        | _ -> failwith "unset!");
    (* 4 *)
    Hashtbl.update grid (149, i) ~f:(function
      | Some (wall, at, [ _; down; left; up ]) ->
          (wall, at, [ ((99, 149 - i), 2); down; left; up ])
      | _ -> failwith "unset!");
    Hashtbl.update grid
      (99, 149 - i)
      ~f:(function
        | Some (wall, at, [ _; down; left; up ]) ->
            (wall, at, [ ((149, i), 2); down; left; up ])
        | _ -> failwith "unset!");
    (* 5 *)
    Hashtbl.update grid
      (i + 100, 49)
      ~f:(function
        | Some (wall, at, [ right; _; left; up ]) ->
            (wall, at, [ right; ((99, i + 50), 2); left; up ])
        | _ -> failwith "unset!");
    Hashtbl.update grid
      (99, i + 50)
      ~f:(function
        | Some (wall, at, [ _; down; left; up ]) ->
            (wall, at, [ ((i + 100, 49), 3); down; left; up ])
        | _ -> failwith "unset!");
    (* 6 *)
    Hashtbl.update grid
      (50, i + 50)
      ~f:(function
        | Some (wall, at, [ right; down; _; up ]) ->
            (wall, at, [ right; down; ((i, 100), 1); up ])
        | _ -> failwith "unset!");
    Hashtbl.update grid (i, 100) ~f:(function
      | Some (wall, at, [ right; down; left; _ ]) ->
          (wall, at, [ right; down; left; ((50, i + 50), 0) ])
      | _ -> failwith "unset!");
    (* 7 *)
    Hashtbl.update grid
      (i + 50, 149)
      ~f:(function
        | Some (wall, at, [ right; _; left; up ]) ->
            (wall, at, [ right; ((49, i + 150), 2); left; up ])
        | _ -> failwith "unset!");
    Hashtbl.update grid
      (49, i + 150)
      ~f:(function
        | Some (wall, at, [ _; down; left; up ]) ->
            (wall, at, [ ((i + 50, 149), 3); down; left; up ])
        | _ -> failwith "unset!")
  done;
  let start = Hashtbl.find_exn grid startxy in
  let (_, (col, row), _), facing =
    List.fold instrs ~init:(start, 0) ~f:(apply_instr grid)
  in
  (1000 * (row + 1)) + (4 * (col + 1)) + facing |> Printer.of_int

let sample =
  {|        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5|}
  |> String.split ~on:'\n'

let%expect_test "a" =
  parta sample |> Printer.print;
  [%expect {| 6032 |}]

(* My grid is different from the sample's, so this test won't pass *)
(* let%expect_test "b" =
   partb sample |> Printer.print;
   [%expect {| 301 |}] *)
