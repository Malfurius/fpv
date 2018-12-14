let todo _ = failwith "TODO"

(* Existing definitions from tutorial assignments *)
type student = {
  first_name : string;
  last_name : string;
  id : int;
  semester : int;
  grades : (int * float) list;
}

type database = student list

let insert s db = s::db

let rec find_by_id id db = match db with [] -> []
  | x::xs -> if x.id = id then [x] else find_by_id id xs

let rec find_by_last_name name db = match db with [] -> []
  | x::xs -> if x.last_name = name 
    then x::find_by_last_name name xs
    else find_by_last_name name xs


(*****************************************************************************)
(**************************** HOMEWORK STARTS HERE ***************************)
(*****************************************************************************)

(*****************************************************************************)
(* Assignment 5.5 [6 Points] *)
let remove_by_id id db = todo()

let rec count_in_semester sem db = todo()

let course_avg_grade course db = todo()



(*****************************************************************************)
(* Assignment 5.6 [3 Points] *)
let rec interleave3 l1 l2 l3 = 
  let rec interleave2 l1 l2 =
    match l1 with [] -> l2
    | x::xs -> x::interleave2 l2 xs
  in
  match l1 with [] -> interleave2 l2 l3
  | x::xs -> x::interleave3 l2 l3 xs

(*****************************************************************************)
(* Assignment 5.7 [3 Points] *)
let foo x y b = 
  let rec myWhile x y b = if x < y then if b then myWhile (x+1) y (not b) else myWhile x (y-1) (not b) else x
  in
  if x>y then myWhile y x b  else myWhile x y b



(*****************************************************************************)
(* Assignment 5.8 [4 Points] *)
let eval_poly x coeffs = todo()

let derive_poly coeffs = todo()


(*****************************************************************************)
(* Assignment 5.9 [4 Points] *)
let lt_seq l = todo()


(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)
(* example inputs, you may use them to test your implementations,
   but [do not change] *)
let a55_ex1 = [
  { first_name = "Anton"; last_name = "Maier"; id=173; semester=3; grades=[1, 1.7; 4, 2.3; 18, 3.0] };
  { first_name = "Betty"; last_name = "Schmidt"; id=418; semester=1; grades=[] };
  { first_name = "Carla"; last_name = "Kurz"; id=223; semester=2; grades=[1, 4.0; 3, 1.0; 7, 1.3; 12, 1.0] };
  { first_name = "Denis"; last_name = "Uler"; id=19; semester=3; grades=[1, 2.2; 7, 1.0; 8, 5.0] }
]

type 'a a56_test_input = { l1 : 'a list; l2 : 'a list; l3 : 'a list }
let a56_ex1 = { l1 = [0;1;2]; l2 = [10;11;12]; l3 = [20;21;22] }
let a56_ex2 = { l1 = ['a';'b']; l2 = ['A';'B';'C';'D']; l3 = ['!'] }
let a56_ex3 = { l1 = []; l2 = []; l3 = [] }

type a57_test_input = { x : int; y : int; b : bool }
let a57_ex1 = { x = 0; y = 0; b = false }
let a57_ex2 = { x = 3; y = 18; b = true }
let a57_ex3 = { x = 22; y = -4; b = true }
let a57_ex4 = { x = -100; y = -100; b = false }
let a57_ex5 = { x = 7; y = 8; b = false }

type a58_test_input = { poly : float list; x : float }
let a58_ex1 = { poly = [0.]; x = 3. }
let a58_ex2 = { poly = [1.]; x = 8. }
let a58_ex3 = { poly = [1.;0.]; x = -14. }
let a58_ex4 = { poly = [1.;0.;1.;0.]; x = -3.5 }
let a58_ex5 = { poly = [2.;-1.;8.]; x = 10.8 }
let a58_ex6 = { poly = [23.;-103.;13.;1.;0.;0.;52.]; x = 2.2 }

let a59_ex1 = [1;2;2;3;4;2;2;2;3;1]
let a59_ex2 = [true;false;false;true]
let a59_ex3 = ['a';'a';'b';'b';'a';'b';'b';'a';'a']
let a59_ex4 = [0.;1.;2.;0.;2.;1.;2.;1.;2.;3.]


(*****************************************************************************)
(* TESTS [do not change] *)
let (=.) a b = (abs_float (a -. b)) < 0.01
let tests = [
  (* tests for 5.5 *)
  __LINE_OF__ (fun () -> (remove_by_id 42 a55_ex1) = a55_ex1);
  __LINE_OF__ (fun () -> (remove_by_id 173 a55_ex1) = List.tl a55_ex1);
  __LINE_OF__ (fun () -> (remove_by_id 418 a55_ex1) = (List.hd a55_ex1) :: (List.tl (List.tl a55_ex1)));
  __LINE_OF__ (fun () -> (count_in_semester 4 a55_ex1) = 0);
  __LINE_OF__ (fun () -> (count_in_semester 1 a55_ex1) = 1);
  __LINE_OF__ (fun () -> (count_in_semester 3 a55_ex1) = 2);
  __LINE_OF__ (fun () -> (student_avg_grade 42 a55_ex1) =. 0.0);
  __LINE_OF__ (fun () -> (student_avg_grade 418 a55_ex1) =. 0.0);
  __LINE_OF__ (fun () -> (student_avg_grade 173 a55_ex1) =. 7./.3.0);
  __LINE_OF__ (fun () -> (student_avg_grade 223 a55_ex1) =. 7.3/.4.0);
  __LINE_OF__ (fun () -> (course_avg_grade 22 a55_ex1) =. 0.0);
  __LINE_OF__ (fun () -> (course_avg_grade 8 a55_ex1) =. 5.0);
  __LINE_OF__ (fun () -> (course_avg_grade 7 a55_ex1) =. (2.3/.2.));
  __LINE_OF__ (fun () -> (course_avg_grade 1 a55_ex1) =. (7.9/.3.));
  (* tests for 5.6 *)
  __LINE_OF__ (fun () -> (interleave3 a56_ex1.l1 a56_ex1.l2 a56_ex1.l3) = [0;10;20;1;11;21;2;12;22]);
  __LINE_OF__ (fun () -> (interleave3 a56_ex2.l1 a56_ex2.l2 a56_ex2.l3) = ['a';'A';'!';'b';'B';'C';'D']);
  __LINE_OF__ (fun () -> (interleave3 a56_ex3.l1 a56_ex3.l2 a56_ex3.l3) = []);
  (* tests for 5.7 *)
  __LINE_OF__ (fun () -> ((foo a57_ex1.x a57_ex1.y a57_ex1.b) = 0));
  __LINE_OF__ (fun () -> ((foo a57_ex2.x a57_ex2.y a57_ex2.b) = 11));
  __LINE_OF__ (fun () -> ((foo a57_ex3.x a57_ex3.y a57_ex3.b) = 9));
  __LINE_OF__ (fun () -> ((foo a57_ex4.x a57_ex4.y a57_ex4.b) = -100));
  __LINE_OF__ (fun () -> ((foo a57_ex5.x a57_ex5.y a57_ex5.b) = 7));
  (* tests for 5.8 *)
  __LINE_OF__ (fun () -> (eval_poly a58_ex1.x a58_ex1.poly) =. 0.);
  __LINE_OF__ (fun () -> (eval_poly a58_ex2.x a58_ex2.poly) =. 1.);
  __LINE_OF__ (fun () -> (eval_poly a58_ex3.x a58_ex3.poly) =. -14.);
  __LINE_OF__ (fun () -> (eval_poly a58_ex4.x a58_ex4.poly) =. -46.375);
  __LINE_OF__ (fun () -> (eval_poly a58_ex5.x a58_ex5.poly) =. 230.48);
  __LINE_OF__ (fun () -> (eval_poly a58_ex6.x a58_ex6.poly) =. -2333.322368);
  __LINE_OF__ (fun () -> (derive_poly a58_ex1.poly) = []);
  __LINE_OF__ (fun () -> (derive_poly a58_ex2.poly) = []);
  __LINE_OF__ (fun () -> (derive_poly a58_ex3.poly) = [1.]);
  __LINE_OF__ (fun () -> (derive_poly a58_ex4.poly) = [3.;0.;1.]);
  __LINE_OF__ (fun () -> (derive_poly a58_ex5.poly) = [4.;-1.]);
  __LINE_OF__ (fun () -> (derive_poly a58_ex6.poly) = [138.;-515.;52.;3.;0.;0.]);
  (* tests for 5.9 *)
  __LINE_OF__ (fun () -> (lt_seq a59_ex1) = [2;2;3]);
  __LINE_OF__ (fun () -> (lt_seq a59_ex2) = [true]);
  __LINE_OF__ (fun () -> (lt_seq a59_ex3) = ['a';'b';'b']);
  __LINE_OF__ (fun () -> (lt_seq a59_ex4) = [1.;2.]);
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

