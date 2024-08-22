let print_list f l =
  Printf.printf "[%!";
  List.iter (fun v -> f v |> Printf.printf "%s, %!") l;
  Printf.printf "]\n%!"

let char_list_of_string s = s |> String.to_seq |> List.of_seq
let sum = List.fold_left ( + ) 0

let digit_opt c =
  match c with
  | '0' .. '9' -> Some (int_of_char c - int_of_char '0')
  | _ -> None

let file_lines name =
  let channel = open_in name in
  let read_line () = try Some (input_line channel) with End_of_file -> None in
  let rec read_lines acc =
    match read_line () with
    | None -> List.rev acc
    | Some line -> line :: acc |> read_lines
  in
  read_lines []
