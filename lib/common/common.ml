let hello_world () = print_endline "Hello world!"

let extract_opt = function
  | None -> raise @@ Invalid_argument "Cannot extract value from None."
  | Some v -> v

let file_lines name =
  let channel = open_in name in
  let read_line () = try Some (input_line channel) with End_of_file -> None in
  let rec read_lines acc =
    match read_line () with
    | None -> List.rev acc
    | Some line -> line :: acc |> read_lines
  in
  read_lines []
