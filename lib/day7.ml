open! Core

type result = int

let updatemap d m sz =
  String.concat d
  |> Map.update m ~f:(function None -> sz | Some ct -> ct + sz)

let rec account_to_tree dir_rev m sz =
  match dir_rev with
  | [] -> updatemap [] m sz
  | _ :: tl as curr ->
      let m = updatemap curr m sz in
      account_to_tree tl m sz

let buildtree ls =
  List.fold ls ~init:([], String.Map.empty)
    ~f:(fun ((dir_rev, m) as curr) line ->
      match line with
      | "$ cd /" -> ([], m)
      | "$ ls" -> curr
      | "$ cd .." -> (List.tl_exn dir_rev, m)
      | l ->
          if String.is_prefix ~prefix:"dir" l then curr
          else if String.is_prefix l ~prefix:"$ cd " then
            let subdir = String.length l - 5 |> String.suffix l in
            (subdir :: dir_rev, m)
          else
            let sz = String.split l ~on:' ' |> List.hd_exn |> Int.of_string in
            (dir_rev, account_to_tree dir_rev m sz))
  |> snd

let parta ls =
  let tree = buildtree ls in
  Map.fold tree ~init:0 ~f:(fun ~key:_ ~data ct ->
      if data <= 100000 then ct + data else ct)

let partb ls =
  let tree = buildtree ls in
  let to_remove = Map.find_exn tree "" - 40000000 in
  Map.fold tree ~init:40000000 ~f:(fun ~key:_ ~data sz ->
      if data >= to_remove && data < sz then data else sz)

let sample =
  {|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k|}
  |> String.split ~on:'\n'

let%expect_test "a" =
   parta sample |> Int.to_string |> print_endline;
   [%expect {| 95437 |}]

let%expect_test "b" =
  partb sample |> Int.to_string |> print_endline;
  [%expect {| 24933642 |}]
