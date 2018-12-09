open Core

type node = { children : node list;
              metadata : int list }
   
let rec read_node lst =
  match read_header lst with
  | Ok (number_of_children, metadata_length, rest)
    -> (match read_children rest number_of_children [] with
        | Ok (children, rest)
          -> (match read_metadata rest metadata_length [] with
              | Ok (metadata, rest)
                -> Ok ({children = children;
                          metadata = metadata},
                         rest)
              | Error st -> Error (("metadata", lst) :: st))
        | Error st -> Error (("children", lst) :: st))
  | Error st -> Error (("header", lst) :: st)

and read_header lst =
  match lst with
  | number_of_children :: metadata_length :: rest
    -> Ok (number_of_children,
           metadata_length,
           rest)
  | _ -> Error [("read_header", lst)]
       
and read_children lst number_of_children accu =
  if number_of_children = 0
  then Ok (List.rev accu, lst)
  else match read_node lst with
       | Ok (child, rest)
         -> read_children rest (number_of_children - 1) (child :: accu)
       | _ -> Error [("read_children", lst)]
     
and read_metadata lst metadata_length accu =
  if metadata_length = 0
  then Ok (List.rev accu, lst)
  else match lst with
       | x :: rest
         -> read_metadata rest (metadata_length-1) (x :: accu)
       | [] -> Error [("read_metadata", lst)]

let rec sum_tree tree =
  let sum lst = List.fold_left ~f:(+) ~init:0 lst in
  match tree with
  | {children; metadata}
    -> let sub_sums = List.map ~f:sum_tree children in
       let my_sum = sum metadata in
       my_sum + (sum sub_sums)

let example = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
let ex2 = "4 3 2 1 4 2 0 1 3 0 1 4 0 1 5 0 1 6 100 101 3 4 0 1 77 0 1 78 0 1 79 1001 1002 1003 1004 19 0 3 13 13 13 0 3 14 14 14 0 7 1 1 1 1 1 1 1 201 202 203"
            
let calculate input =
  let code = List.map ~f:int_of_string (String.split input ~on:' ') in
  match read_node code with
  | Ok (tree, unparsed)
    -> let sum = sum_tree tree in
       Ok (Printf.sprintf "The sum of the tree is %d" sum,
           sum,
           tree)
  | Error st
    -> Error st
