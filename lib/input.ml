open Core

let to_sections lines =
  let result, current =
    List.fold lines ~init:([], []) ~f:(fun (result, current) l ->
        match l with
        | "" -> (List.rev current :: result, [])
        | contents -> (result, contents :: current))
  in
  let result =
    match current with
    | [] -> result
    | last_section -> List.rev last_section :: result
  in
  List.rev result

let to_tokens ?(on = ' ') line =
  String.split ~on line
  |> List.filter ~f:(fun tok -> String.is_empty tok |> not)

let split_on_string s ~sep =
  let p = String.Search_pattern.create sep in
  String.Search_pattern.split_on p s

let token_ints line =
  to_tokens line
  |> List.filter_map ~f:(fun t -> Option.try_with (fun () -> Int.of_string t))
