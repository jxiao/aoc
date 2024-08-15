let print_list f = List.iter (fun v -> f v |> print_endline)
let char_list_of_string s = s |> String.to_seq |> List.of_seq

let file_lines name =
  let channel = open_in name in
  let read_line () = try Some (input_line channel) with End_of_file -> None in
  let rec read_lines acc =
    match read_line () with
    | None -> List.rev acc
    | Some line -> line :: acc |> read_lines
  in
  read_lines []
