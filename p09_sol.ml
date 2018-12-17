
type student = {
  first_name : string;
  last_name : string;
  id : int;
  semester : int;
  grades : (int * float) list;
}

type database = student list

exception Corrupt_database_file

let store_db filename db =
  let file = open_out filename in
  let rec write_grade (c,g) = Printf.fprintf file "%d;%.2f\n" c g
  in
  let write_student s =
    Printf.fprintf file "%s;%s;%d;%d;%d\n" s.first_name s.last_name s.id s.semester (List.length s.grades);
    List.iter write_grade s.grades;
  in
  List.iter write_student db;
  close_out file

let load_db filename =
  let file = open_in filename in
  let rec read_grades gc grades =
    if gc <= 0 then List.rev grades else
    try
      let line = input_line file in
      match String.split_on_char ';' line with
      | [course_s;grade_s] ->
        let course,grade = (try
        int_of_string course_s, float_of_string grade_s
        with _ -> raise Corrupt_database_file) in
        if course < 0 || grade < 1.0 || grade > 5.0 then raise Corrupt_database_file
        else read_grades (gc-1) ((course,grade)::grades)
      | _ -> raise Corrupt_database_file
    with End_of_file -> raise Corrupt_database_file
  in
  let rec read_students db =
    try
      let line = input_line file  in
      match String.split_on_char ';' line with
      | ""::_ | _::""::_ -> raise Corrupt_database_file
      | [first_name;last_name;id_s;sem_s;gc_s] ->
        let id,semester,gc = try
          int_of_string id_s, int_of_string sem_s, int_of_string gc_s
        with _ -> raise Corrupt_database_file in
        if id < 0 || semester < 0 || gc < 0 || gc > 100 then raise Corrupt_database_file
        else if List.find_opt (fun s -> s.id = id) db <> None then raise Corrupt_database_file
        else
        let grades = read_grades gc [] in
        read_students ({ first_name; last_name; id; semester; grades }::db)
      | _ -> raise Corrupt_database_file
    with End_of_file -> db
  in
  try
    let db = read_students [] |> List.rev in
    close_in file;
    db
  with e -> close_in file; raise e



let a55_ex1 = [
  { first_name = "Anton"; last_name = "Maier"; id=173; semester=3; grades=[1, 1.7; 4, 2.3; 18, 3.0] };
  { first_name = "Betty"; last_name = "Schmidt"; id=418; semester=1; grades=[] };
  { first_name = "Carla"; last_name = "Kurz"; id=223; semester=2; grades=[1, 4.0; 3, 1.0; 7, 1.3; 12, 1.0] };
  { first_name = "Denis"; last_name = "Uler"; id=19; semester=3; grades=[1, 2.2; 7, 1.0; 8, 5.0] }
]
