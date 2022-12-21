open! Containers
open! Core
open! Types

let to_array_backed_dll ?(f = Fn.id) ls =
  let ct = List.length ls in
  Array.of_list_mapi ls ~f:(fun i line ->
      ((i - 1) % ct, Int.of_string line |> f, (i + 1) % ct))

let chase ~len ~from ~ct ll =
    let i = ref from in
    let to_mv = ct % len in
    for k = 0 to to_mv - 1 do
      i := Xyz.z ll.(!i)
    done;
    !i

let mix ll =
  let ct = Array.length ll - 1 in
  for i = 0 to ct do
    let pi, v, ni = ll.(i) in
    let ppi, pv, _ = ll.(pi) in
    let _, nv, nni = ll.(ni) in
    ll.(pi) <- (ppi, pv, ni);
    ll.(ni) <- (pi, nv, nni);
    (* ll.(j) will be our new predecessor for i *)
    let j = chase ll ~len:ct ~from:pi ~ct:v in
    let ppi, pv, pni = ll.(j) in
    let _, nv, nni = ll.(pni) in
    ll.(j) <- (ppi, pv, i);
    ll.(i) <- (j, v, pni);
    ll.(pni) <- (i, nv, nni)
  done

let result ll =
  let len = Array.length ll in
  let zi, _ = Array.findi_exn ll ~f:(fun _ (_, v, _) -> v = 0) in
  List.fold [ 1000; 1000; 1000 ] ~init:(0, zi) ~f:(fun (acc, i) ct ->
      let j = chase ll ~from:i ~ct ~len in
      (acc + Xyz.y ll.(j), j))
  |> fst

let parta ls =
  let ll = to_array_backed_dll ls in
  mix ll;
  result ll |> Printer.of_int

let partb ls =
  let ll = to_array_backed_dll ls ~f:(fun v -> v * 811589153) in
  for _ = 1 to 10 do
    mix ll
  done;
  result ll |> Printer.of_int

let _sample = {|1
2
-3
3
-2
0
4|} |> String.split ~on:'\n'

let%expect_test "a" =
  parta _sample |> Printer.print;
  [%expect {| 3 |}]

let%expect_test "b" =
   partb _sample |> Printer.print;
   [%expect {| 1623178306 |}]
