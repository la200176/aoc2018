open Core

let rec parse s =
  let lst = String.split s ~on:' ' in
  match header lst with
  | Some (number_of_children, metadata_length, rest)
    -> (match children rest number_of_children [] with
        | Some (children, rest)
          -> (match metadata rest metadata_length [] with
              | Some (metadata, rest)
                -> Some (children, metadata, rest)
              | _ -> None)
        | _ -> None)
  | _ -> None

and header lst =
  match lst with
  | number_of_children :: metadata_length :: rest
    -> Some ((int_of_string number_of_children),
             (int_of_string metadata_length),
             rest )
  | _ -> None
       
and children lst number_of_children accu =
  if number_of_children = 0
  then Some (List.rev accu, lst)
  else None
     
and metadata lst metadata_length accu =
  if metadata_length = 0
  then Some (List.rev accu, lst)
  else match lst with
       | x :: rest
         -> metadata rest (metadata_length-1) (x :: accu)
       | [] -> None


let _ = 
  let example = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" in
  None
