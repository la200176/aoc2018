open Core

type node = { children : node list;
              metadata : int list }

let rec read_node lst =

  let read_header (number_of_children :: metadata_length :: rest) =
    (number_of_children, metadata_length, rest) in

  let rec read_children lst number_of_children accu =
    (if number_of_children = 0
     then (List.rev accu, lst)
     else let child, rest = read_node lst in
          read_children rest (number_of_children-1) (child::accu)) in

  let rec read_metadata (x::rest as lst) metadata_length accu =
    (if metadata_length = 0
     then (List.rev accu, lst)
     else read_metadata rest (metadata_length-1) (x :: accu)) in
  
  let number_of_children, metadata_length, rest = read_header lst in
  let children, rest = read_children rest number_of_children [] in
  let metadata, rest = read_metadata rest metadata_length [] in
  {children; metadata}, rest

let input_as_list s =
  List.map ~f:int_of_string (String.split s ~on:' ')

let rec sum_tree tree =
  let sum lst = List.fold_left ~f:(+) ~init:0 lst in
  match tree with
  | {children; metadata}
    -> let sub_sums = List.map ~f:sum_tree children in
       let my_sum = sum metadata in
       my_sum + (sum sub_sums)

let ()  =
  let code = input_as_list "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" in
  let tree, unparsed = read_node code in
  Printf.printf "The sum of the tree is %d\n" (sum_tree tree)
  
