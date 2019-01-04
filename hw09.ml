let todo _ = failwith "TODO"

type behavior = Nice | Naughty
type notes = (string * behavior) list
type selection_alg = (string * int * int) list -> int -> string list

exception Invalid_file_format of string

let noteName = "santas_notes.txt"

let debugString str = Printf.printf "%s" str

let debugInt int = Printf.printf "%i," int

let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

let debugList list = 
  let rec printElement list = 
    match list with
      | (child,behavior)::xs -> (match behavior with
                                | Nice -> debugString ("("^child^",NICE)"); printElement xs
                                | Naughty -> debugString ("("^child^",NAUGHTY)"); printElement xs)
      | [] -> ""
  in
  printElement list

  let debugPresentList list =
    let rec printEle nList = 
      match nList with
      | (name,imp,weight)::xs -> debugString (name^","); debugInt imp; debugInt weight; debugString "|";printEle xs
      | [] -> ()
    in
    printEle list
 
(* 9.3 - 1 *)
let read_notes fName = 
  let file = open_in fName in
  let rec readChildren list line = 
    match String.split_on_char ':' line with
      | [child;behavior] -> (if(child <> "" )then (match behavior with
                            | "nice" -> ((child,Nice)::list)
                            | "naughty" ->((child,Naughty)::list)
                            | _ -> raise (Invalid_file_format (fName) ))
                            else raise (Invalid_file_format (fName) ) )
      | _ -> raise (Invalid_file_format (fName) )
  in 
  let rec read list =
    try 
    let line = input_line file in
    let nList = readChildren list line in read nList
    with End_of_file -> list
  in
  try
  let l = List.rev (read []) in
  close_in file;
  l
  with e -> close_in file; raise e

(* 9.3 - 2 *)
let read_wishlist fName = 
  let file = open_in fName in
  let rec readWish list line = 
    match String.split_on_char ':' line with
      | [toy;imp] -> (if (toy <> "") 
                      then (  if(is_int imp) 
                              then ((toy, (int_of_string imp))::list )
                              else raise (Invalid_file_format (fName)))
                      else raise (Invalid_file_format (fName) ))
      | _ -> raise (Invalid_file_format (fName) )
  in
  let rec read list =
    try
    let line = input_line file in
    let nList = readWish list line in read nList
    with End_of_file -> list
  in
  try
  let l = List.rev (read []) in
  close_in file;
  l
  with e -> close_in file; raise e

(* 9.3 - 3 *)
let load_catalogue fName= 
  let file = open_in fName in
  let rec readCat list line = 
    match String.split_on_char ':' line with
    | [toy;weight] -> (if (toy <> "")
                       then (if(is_int weight)
                            then ((toy,(int_of_string weight))::list)
                            else raise (Invalid_file_format fName))
                       else raise (Invalid_file_format fName))
    | _ -> raise (Invalid_file_format fName)
  in
  let rec read list = 
    try
    let line = input_line file in
    let nList = readCat list line in read nList
    with End_of_file -> list
  in
  try
  let l = List.rev (read []) in
  close_in file;
  l
  with e -> close_in file; raise e

(* 9.3 - 4 *)
let write_list fName list = 
  let file = open_out fName in
  let rec printItem nlist = 
    (match nlist with
    | x::xs -> (if (xs<>[]) 
                then (Printf.fprintf file "%s;" x;debugString (x^";") ;printItem xs)
                else (Printf.fprintf file "%s" x; debugString x))
    | [] -> close_out file )
  in
  printItem list; close_out file;debugString "\n"

(* 9.3 - 5 *)
let write_letter fName =
  let file = open_out fName in
  Printf.fprintf file "Some harsh Words"; close_out file

let rec recWeight toyName sToyCat =
  match sToyCat with
  | (name,weight)::xs -> (if (name = toyName)
                          then (weight)
                          else (recWeight toyName xs))
  | [] -> (-1)


let getWeight toyName toyCat =
  recWeight toyName toyCat

let workNiceChild childName toyCat = 
  let wishList = read_wishlist (childName^"_wishlist.txt") in
  debugString (childName^"\n");
  let rec constructList l wishL=
    match wishL with
    | (name, imp)::xs -> let weight = getWeight name toyCat in 
                     if(weight>=0)
                     then(constructList ((name,imp,weight)::l) xs)
                     else(constructList l xs)
    | [] -> l
  in
  constructList [] wishList


(* 9.3 - 6 *)
let run_santas_factory mWeight selectionAlg = 
  let toyCat = load_catalogue "toys_catalogue.txt" in
  let santaNotes = read_notes "santas_notes.txt" in
  let rec evalNotes notes = match notes with
    | (name,Nice)::xs -> let childList = (workNiceChild name toyCat)
                         in debugPresentList childList; if (childList <> [] )
                                                        then (write_list (name^"_presents.txt") (selectionAlg childList mWeight))
                                                        else ()
                         ; evalNotes xs
    | (name,Naughty)::xs -> (write_letter (name^"_letter.txt") ); evalNotes xs
    | [] -> ()
  in
  evalNotes santaNotes


(* 9.3 - 7 *)
let knapsack = todo



(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)
(* example inputs, you may use them to test your implementations,
   but [do not change] *)
let a933_ex1 = ["penguin doll",1; "ocaml book",2; "time machine",53; "bike",7; "barbie's dream house",5;
  "guitar",6; "colorful pencils",2; "socks",1; "shawl",2; "karaoke machine",13; "superman action doll set",3;
  "guinea pig",3; "horse",10; "unicorn",8; "sand toys",4; "soccer shoes",3]

(*****************************************************************************)
(* TESTS [do not change] *)
let (=^) a b =
    (List.sort compare a) = (List.sort compare b)
let (=|) a b =
    let a = List.sort_uniq (fun x y -> compare (fst x) (fst y)) a in
    let b = List.sort_uniq (fun x y -> compare (fst x) (fst y)) b in
    a = b
let check_throws e f =
  try f (); false with e' -> e' = e

let check_file filename content =
  let file = open_in filename in
  let rec read acc =
    try
      read ((input_line file)::acc)
    with End_of_file -> acc
  in
  let c = read [] in
  close_in file;
  (List.sort_uniq compare c) = (List.sort_uniq compare content)

let check_letter filename =
  let file = open_in filename in
  let rec read () =
    try
      let line = input_line file in
      if line <> "" then true else
      read ()
    with End_of_file -> false
  in
  let r = read () in
  close_in file;
  r

let raise' = function Failure f ->
  Printf.printf "TEST FAILURE: %s\n" f;
  raise (Failure f)
  | e -> raise e

let check_run_santas_factory () =
  let test_selection_alg wishes capacity =
    if capacity <> 13 then raise' (Failure "wrong capacity passed to selection_alg");
    (match List.find_opt (fun (t,_,_) -> t = "ocaml book") wishes with
    | None -> raise' (Failure "wrong list passed to selection_alg: EMPTY")
    | Some (_,_,w) -> if w <> 2 then raise' (Failure "wrong list passed to selection_alg: UNSURE"));
    match List.sort (fun (_,i,_) (_,j,_) -> compare j i) wishes with
    | (w1,_,_)::(w2,_,_)::_ -> [w1;w2]
    | _ -> raise' (Failure "wrong list passed to selection_alg")
  in
  ignore(run_santas_factory 13 test_selection_alg);
  if not (check_letter "marta_letter.txt") then raise (Failure "no correct letter produced for marta");
  if not (check_letter "bruno_letter.txt") then raise (Failure "no correct letter produced for bruno");
  if not (check_file "frida_presents.txt" ["colorful pencils";"ocaml book"]) then raise (Failure "no correct present list produced for frida");
  if not (check_file "tommy_presents.txt" ["sand toys";"superman action doll set"]) then raise (Failure "no correct present list produced for tommy");
  if not (check_file "caren_presents.txt" ["penguin doll";"unicorn"]) then raise (Failure "no correct present list produced for caren");
  true

let tests = [
  (* tests for 9.3 - 1 *)
  __LINE_OF__ (fun () -> (read_notes "examples/santas_notes.txt") =| ["tommy",Nice;"bruno",Naughty;"frida",Nice;"caren",Nice;"marta",Naughty]);
  __LINE_OF__ (fun () -> let fn = "examples/santas_notes_broken1.txt" in check_throws (Invalid_file_format fn) (fun () -> read_notes fn));
  __LINE_OF__ (fun () -> let fn = "examples/santas_notes_broken2.txt" in check_throws (Invalid_file_format fn) (fun () -> read_notes fn));
  (* tests for 9.3 - 2 *)
  __LINE_OF__ (fun () -> (read_wishlist "examples/frida_wishlist.txt") =| ["ocaml book",10;"horse",3;"colorful pencils",12]);
  __LINE_OF__ (fun () -> let fn = "examples/wishlist_broken1.txt" in check_throws (Invalid_file_format fn) (fun () -> read_wishlist fn));
  __LINE_OF__ (fun () -> let fn = "examples/wishlist_broken2.txt" in check_throws (Invalid_file_format fn) (fun () -> read_wishlist fn));
  __LINE_OF__ (fun () -> let fn = "examples/wishlist_broken3.txt" in check_throws (Invalid_file_format fn) (fun () -> read_wishlist fn));
  (* tests for 9.3 - 3 *)
  __LINE_OF__ (fun () -> (load_catalogue "examples/toys_catalogue.txt") =| a933_ex1);
  __LINE_OF__ (fun () -> let fn = "examples/toys_catalogue_broken1.txt" in check_throws (Invalid_file_format fn) (fun () -> load_catalogue fn));
  __LINE_OF__ (fun () -> let fn = "examples/toys_catalogue_broken2.txt" in check_throws (Invalid_file_format fn) (fun () -> load_catalogue fn));
  __LINE_OF__ (fun () -> let fn = "examples/toys_catalogue_broken3.txt" in check_throws (Invalid_file_format fn) (fun () -> load_catalogue fn));
  (* tests for 9.3 - 4 *)
  __LINE_OF__ (fun () -> let l = ["socks";"colorful pencils";"horse"] in let fn = "examples/testout_list1.txt" in write_list fn l; check_file fn l);
  (* tests for 9.3 - 5 *)
  __LINE_OF__ (fun () -> let fn = "examples/testout_letter1.txt" in write_letter fn; check_letter fn);
  (* tests for 9.3 - 6 *)
  __LINE_OF__ (fun () -> check_run_santas_factory ());
  (* tests for 9.3 - 7 *)
  __LINE_OF__ (fun () -> (knapsack ["a",5,4; "b",2,2; "b",2,2; "d",4,5; "b",2,2; "e",8,2] 10) =^ ["a";"b";"b";"e"]);
  __LINE_OF__ (fun () -> (knapsack ["a",5,4; "a",5,4; "c",11,6; "d",4,5; "e",8,2; "a",5,4] 10) =^ ["c";"e"]);
]

let () =
  let rec input_lines ch =
    (try Some (input_line ch) with _ -> None) (* catch stupid EOF exception *)
    |> function Some line -> line :: input_lines ch | None -> []
  in
  let lines = input_lines (open_in __FILE__) in
  let open List in
  let open Printf in
  let fail l =
    let line = nth lines (l-1) in
    let test = String.sub line 25 (String.length line - 27) in
    printf "test \027[31;m%s\027[0;m (line %d) failed!\n" test l;
  in
  let test (l, t) =
    let ok = try t () with e -> print_endline (Printexc.to_string e); false in
    if not ok then fail l;
    ok
  in
  let passed = filter (fun x -> x) (map test tests) in
  printf "passed %d/%d tests\n" (length passed) (length tests)









