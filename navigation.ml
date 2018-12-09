open Core

type node = { children : node list;
              metadata : int list }
   
let rec read_node lst =
  match read_header lst with
  | Some (number_of_children, metadata_length, rest)
    -> (match read_children rest number_of_children [] with
        | Some (children, rest)
          -> (match read_metadata rest metadata_length [] with
              | Some (metadata, rest)
                -> Some ({children = children;
                          metadata = metadata},
                         rest)
              | _ -> None)
        | _ -> None)
  | _ -> None

and read_header lst =
  match lst with
  | number_of_children :: metadata_length :: rest
    -> Some (number_of_children,
             metadata_length,
             rest)
  | _ -> None
       
and read_children lst number_of_children accu =
  if number_of_children = 0
  then Some (List.rev accu, lst)
  else match read_node lst with
       | Some (child, rest)
         -> read_children rest (number_of_children - 1) (child :: accu)
       | _ -> None
     
and read_metadata lst metadata_length accu =
  if metadata_length = 0
  then Some (List.rev accu, lst)
  else match lst with
       | x :: rest
         -> read_metadata rest (metadata_length-1) (x :: accu)
       | [] -> None

let rec sum_tree tree =
  let sum lst = List.fold_left ~f:(+) ~init:0 lst in
  match tree with
  | {children; metadata}
    -> let sub_sums = List.map ~f:sum_tree children in
       let my_sum = sum metadata in
       my_sum + (sum sub_sums)
             
let ()  =
  let example = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" in
  let code = List.map ~f:int_of_string (String.split example ~on:' ') in
  match read_node code with
  | Some (tree, unparsed)
    -> Printf.printf "The sum of the tree is %d\n" (sum_tree tree)
  | _
    -> Printf.printf "input is not a proper tree"
