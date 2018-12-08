open Core

let header =
  let example = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" in
  match String.split example ~on:' ' with
  | number_of_children :: metadata_length :: rest -> Some (number_of_children, metadata_length, rest)
  | _ -> None

