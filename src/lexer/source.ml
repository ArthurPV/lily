open Lily_common.Error

type source = {
  filename : string;
  content : string;
  mutable c : char;
  len : int;
  mutable pos : int;
}

let new_source filename content =
  let c = if String.length content > 0 then content.[0] else ' ' in
  { filename; content; c; len = String.length content; pos = 0 }

let read_file filename =
  if Sys.file_exists filename <> true then Error (FileNotExists filename)
  else if Sys.is_directory filename <> false then
    Error (FileIsDirectory filename)
  else if Filename.extension filename <> ".lily" then
    Error (FileHasBadExtension filename)
  else
    let ic = open_in filename in
    let try_read () = try Some (input_line ic) with End_of_file -> None in
    let rec loop ?(acc = []) () =
      match try_read () with
      | Some s -> loop ~acc:(s :: acc) ()
      | None ->
          close_in ic;
          acc |> List.rev
    in
    let content_list = loop () in
    match content_list with
    | [] -> Ok ""
    | _ -> Ok (List.map (fun x -> x ^ "\n") content_list |> String.concat "")
