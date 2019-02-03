(* testing utilities [do not change] *)

exception SyncDeadlocked
module Event = struct
  include Event

  let tsync t e =
    let timer = new_channel () in
    let run_timer () =
      Thread.delay t;
      poll (send timer None)
    in
    let _ = Thread.create run_timer () in
    match (select [wrap e (fun x -> Some x); receive timer]) with
    | Some x -> x
    | None -> raise SyncDeadlocked

  let tselect t es =
    tsync t (choose es)

  let sync e = tsync 2. e
  let select es = tselect 2. es
end

module Thread = struct
  include Thread

  let tc = ref 0

  let create f a =
    tc := !tc + 1;
    create f a
end


(*****************************************************************************)
(*************************** START OF HOMEWORK *******************************)
(*****************************************************************************)
open Thread
open Event

let debugString str = Printf.printf "%s" str

module Future = struct
  type 'a msg = Result of 'a | Ex of exn
  type 'a t = 'a msg channel

  let create f a =
    let c = new_channel () in
    let task () =
      let r = try Result (f a) with e -> Ex e in
      sync (send c r)
    in
    let _ = Thread.create task () in
    c

    let bicreate f a b=
    let c = new_channel () in
    let task () =
      let r = try Result (f a b) with e -> Ex e in
      sync (send c r)
    in
    let _ = Thread.create task () in
    c

  let get c =
    match sync (receive c) with
    | Result r -> r
    | Ex e -> raise e

  let then_ f c =
    let c' = new_channel () in
    let task () =
      let r = match sync (receive c) with
      | Result r -> Result (f r)
      | Ex e -> Ex e
      in
      sync (send c' r)
    in
    let _ = Thread.create task () in
    c'

  let when_any cs =
    let c' = new_channel () in
    let task () =
      let r = select (List.map receive cs) in
      sync (send c' r)
    in
    let _ = Thread.create task () in
    c'

  let when_all cs =
    let c' = new_channel () in
    let task () =
      let r = List.fold_left (fun a c -> sync (receive c)::a) [] cs |> List.rev in
      match List.find_opt (function Ex _ -> true | _ -> false) r with
      | Some (Ex e) -> sync (send c' (Ex e))
      | _ -> sync (send c' (Result (List.map (function Result r -> r | _ -> failwith "unreachable") r)))
    in
    let _ = Thread.create task () in
    c'

  (* additional stuff *)
  let memoize c =
    let c' = new_channel () in
    let task () =
      let r = sync (receive c') in
      let rec repeat () =
        sync (send c' r);
        repeat ()
      in
      repeat ()
    in
    let _ = Thread.create task () in
    c'

  let result_to receiver_c c =
    let task () =
      match sync (receive c) with
      | Result r -> sync (send receiver_c r)
      | Ex e -> raise e
    in
    let _ = Thread.create task () in
    ()

  let get_opt c = poll (receive c)
end

(* 13.4 *)
let par_unary f a = 
  let creatChannel e = Future.create f e
  in
  let channels = (List.map creatChannel a)
  in
  List.map Future.get channels

let par_binary f a b = 
  let createChannel e1 e2 = Future.bicreate f e1 e2
  in
  let channels = (List.map2 createChannel a b)
  in
  List.map Future.get channels


(* 13.5 *)
exception OutOfBounds

module Array = struct
  type 'a t = 'a channel
  type 'a answer =  SizeAns of int|GetAns of 'a|Exc of exn| Conf
  type 'a message = Size of ('a answer channel) |Destroy of int|Set of int*'a*'a answer channel| Get of int* 'a answer channel|Resize of int*'a


  let make s v =
    let rec sublist s l nl = match (l,s) with
    | (x::xs,_) -> sublist (s-1) (x::nl) xs
    | (x::xs,0) -> nl
    | ([],_) -> raise OutOfBounds
    in
    let c = new_channel () in
      let rec array_fun a = 
        match sync(receive c) with
        | Size(a_channel) ->  sync(send a_channel (SizeAns(List.length a))); array_fun a
        | Destroy(i) -> (fun a -> ())
        | Set(i,v, a_channel) -> let na = (if( (i>=0) && (i<List.length a) ) then (sync (send a_channel Conf);(List.mapi (fun idx e -> if(idx=i)then v else e) a))  else (sync(send a_channel (Exc(OutOfBounds) ));a)  ) in array_fun na
        | Get(i,a_channel) -> (if( (i>=0) && (i<List.length a) ) then sync(send a_channel (GetAns(List.nth a i))) else sync (send a_channel (Exc(OutOfBounds)))) ; array_fun a
        | Resize(ns, v) -> let na = (if(ns>s) then (a@(List.init (ns-s) (fun _ -> v))) else sublist ns [] a) in array_fun na
      in
      let _ = Thread.create array_fun (List.init s (fun _ -> v))
      in 
    c

  let size a = 
    let a_channel = new_channel () in
    sync (send a (Size(a_channel))); match sync (receive a_channel) with
    | SizeAns(v) -> v
 
  let set i v a =
  let a_channel = new_channel () in
   sync (send a (Set(i,v,a_channel))); match sync(receive a_channel) with
   | Conf -> ()
   | Exc(e) -> raise e

  let get i a = let a_channel = new_channel () in
  sync (send a (Get(i,a_channel))); match sync(receive a_channel) with
  | GetAns(v) -> v
  | Exc(e) -> raise e

  let resize s v a = sync(send a (Resize(s,v)))

  let destroy a = sync (send a (Destroy(1)))

end


(* 13.6 *)
exception InvalidOperation

type 'a t = 'a channel
type 'a docAnswer = DocExc of exn|DocAns|PubAns of int|ViewAns of string
type 'a docMessage = CreateAcc of string*string*'a docAnswer channel|Publish of string*string*string*'a docAnswer channel|View of string*string*int*'a docAnswer channel|ChangeOwner of string*string*string*int*'a docAnswer channel
type serverData = ServerData of ((string*string) list)*((int*string*string*string list) list)

let document_server () = 
  let c = new_channel () in
  let error a_c = sync (send a_c (DocExc(InvalidOperation))) in
  let auth u p l= (List.exists (fun (user,password) -> (u=user && p=password) ) l) in
  let rec server_fun (userList,docList) = 
    match sync(receive c) with
    | CreateAcc(name,pw,a_channel) -> if (List.exists (fun (en,_)->name=en ) userList) then (sync(send a_channel (DocExc(InvalidOperation)));server_fun (userList,docList)) else sync(send a_channel (DocAns));server_fun ((name,pw)::userList,docList)
    | Publish(name,pw,doc,a_channel) -> let nId = List.length docList in  if auth name pw userList then (sync (send a_channel (PubAns(nId)));server_fun (userList,(nId,doc,name,[])::docList)) else (sync (send a_channel (DocExc(InvalidOperation)) );server_fun (userList,docList))
    | View(name,pw,docId,a_channel) ->  if ((auth name pw userList)  && (docId<(List.length docList)))
                                        then match (List.nth docList docId) with
                                              | (id,doc,owner,viewerList) ->  if (List.exists (fun v->v=name) viewerList || name=owner) 
                                                                              then (sync (send a_channel (ViewAns(doc)));server_fun (userList,docList))
                                                                              else (error a_channel;server_fun (userList,docList))
                                                                        
                                        else (error a_channel; server_fun (userList,docList))
    | ChangeOwner(name,pw,docId,nOwner,a_channel) ->  if ((auth name pw userList)  && (docId<(List.length docList)))
                                                      then match (List.nth docList docId) with
                                                          | (id,doc,owner,viewerList) ->  if(name=owner)
                                                                                          then (sync(send a_channel (DocAns));(userList,(List.map (fun (dId,dDoc,dOwner,dViwerList)-> if (dId=docId) then (dId,dDoc,nOwner,dViwerList) else (dId,dDoc,dOwner,dViwerList) ) docList)))
                                                                                          else (error a_channel;server_fun (userList,docList))
                                                      else error a_channel
    | _ -> server_fun (userList,docList)
  in
  let _ = Thread.create server_fun ([],[])
  in
  c

let publish u p doc s = 
  let a_channel = new_channel () in
  sync (send s (Publish(u,p,doc,a_channel)));
  match sync (receive a_channel) with
  | PubAns(i) -> i
  | DocExc(e) -> raise e

let change_owner u p id owner s = 
  let a_channel = new_channel () in
  sync (send s (ChangeOwner(u,p,id,owner,a_channel))); 
  match sync(receive a_channel) with
  | DocAns -> ()
  | DocExc(e) -> raise e

let view u p id s = 
  let a_channel = new_channel () in
  sync (send s (View(u,p,id,a_channel)));
  match sync(receive a_channel) with
  | ViewAns(i) -> i
  | DocExc(e) -> raise e

let add_account u p s =
  let a_channel = new_channel () in
  sync (send s (CreateAcc(u,p,a_channel))); 
  match sync(receive a_channel) with
  | DocAns -> ()
  | DocExc(e) -> raise e

let add_viewer u p id viewer s = failwith "TODO"


(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)

(*****************************************************************************)
(* TESTS [do not change] *)
let reset () =
  Thread.tc := 0
let threads_created () =
  !Thread.tc

let d_server () =
  let s = document_server () in
  add_account "user1" "pass1" s;
  add_account "user2" "pass2" s;
  add_account "user3" "pass3" s;
  s

let tests = [
  (* 13.4 *)
  __LINE_OF__ (fun () -> let pinc = par_unary (fun x -> x + 1) in pinc [8;1;1] = [9;2;2] && threads_created () = 3);
  __LINE_OF__ (fun () -> let psof = par_unary string_of_float in psof [7.;1.] = ["7.";"1."] && threads_created () = 2);
  __LINE_OF__ (fun () -> let pmul = par_binary ( * ) in pmul [1;2;3] [5;6;2] = [5;12;6] && threads_created () = 3);
  __LINE_OF__ (fun () -> let pcon = par_binary ( ^ ) in pcon ["th";"";"ver";"nic"] ["is";"is";"y";"e"] = ["this";"is";"very";"nice"] && threads_created () = 4);
  (* 13.5
  NOTE: Array's functions cannot be tested in isolation, so if a test for size fails it may very well be due to a mistake in your implementation of make *)
  __LINE_OF__ (fun () -> let _ = Array.make 3 "abc" in threads_created () = 1);
  __LINE_OF__ (fun () -> let a = Array.make 3 1. in Array.destroy a; threads_created () = 1);
  __LINE_OF__ (fun () -> let a = Array.make 3 0 in Array.size a = 3);
  __LINE_OF__ (fun () -> let a = Array.make 3 'x' in Array.get 0 a = 'x');
  __LINE_OF__ (fun () -> let a = Array.make 3 'x' in try let _ = Array.get 3 a in false with OutOfBounds -> true);
  __LINE_OF__ (fun () -> let a = Array.make 3 0 in Array.set 1 5 a; Array.get 0 a = 0 && Array.get 1 a = 5 && Array.get 2 a = 0 && threads_created () = 1);
  __LINE_OF__ (fun () -> let a = Array.make 3 'x' in try Array.set 3 'u' a; false with OutOfBounds -> true);
  __LINE_OF__ (fun () -> let a = Array.make 3 0 in Array.resize 5 1 a; Array.size a = 5 && Array.get 2 a = 0 && Array.get 3 a = 1 && Array.get 4 a = 1 && threads_created () = 1);
  __LINE_OF__ (fun () -> let a = Array.make 3 0 in Array.resize 1 1 a; Array.size a = 1 && Array.get 0 a = 0 && threads_created () = 1);
  (* 13.6
  NOTE: Document server functions cannot be tested in isolation, so if a test for view fails it may very well be due to a mistake in your implementation of document_server *)
  __LINE_OF__ (fun () -> let _ = document_server () in threads_created () = 1); (* basic thread creation *)
  __LINE_OF__ (fun () -> let s = document_server () in add_account "user1" "pass1" s; true); (* add correct account *)
  __LINE_OF__ (fun () -> let s = d_server () in try add_account "user1" "***" s; false with InvalidOperation -> true); (* account exists already *)
  __LINE_OF__ (fun () -> let s = d_server () in publish "user2" "pass2" "My Document" s <> publish "user1" "pass1" "My Document" s); (* publish document *)
  __LINE_OF__ (fun () -> let s = d_server () in try let _ = publish "user1" "***" "My Document" s in false with InvalidOperation -> true); (* publish incorrect auth *)
  __LINE_OF__ (fun () -> let s = d_server () in try let _ = view "user1" "pass1" 0 s in false with InvalidOperation -> true); (* view invalid document *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "text" s in "text" = view "user1" "pass1" d s); (* view correct *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "text" s in try let _ = view "user2" "pass2" d s in false with InvalidOperation -> true); (* view, no access *)
  __LINE_OF__ (fun () -> let s = d_server () in try add_viewer "user1" "pass1" 0 "user3" s; false with InvalidOperation -> true); (* add viewer invalid document *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "text" s in try add_viewer "user1" "***" d "user3" s; false with InvalidOperation -> (try let _ = view "user3" "pass3" d s in false with InvalidOperation -> true)); (* add viewer invalid auth *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user2" "pass2" "text" s in add_viewer "user2" "pass2" d "user1" s; view "user1" "pass1" d s = "text"); (* add viewer correct *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "mydoc" s in try change_owner "user1" "***" d "user2" s; false with InvalidOperation -> true); (* change owner invalid auth *)
  __LINE_OF__ (fun () -> let s = d_server () in try change_owner "user1" "pass1" 0 "user3" s; false with InvalidOperation -> true); (* change owner invalid document *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "mydoc" s in try change_owner "user2" "pass2" d "user2" s; false with InvalidOperation -> true); (* change owner, not owner *)
  __LINE_OF__ (fun () -> let s = d_server () in let d = publish "user1" "pass1" "mydoc" s in change_owner "user1" "pass1" d "user3" s; view "user3" "pass3" d s = "mydoc"); (* change owner correct *)
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
    reset ();
    let ok = try t () with e -> print_endline (Printexc.to_string e); false in
    if not ok then fail l;
    ok
  in
  let passed = filter (fun x -> x) (map test tests) in
  printf "passed %d/%d tests\n" (length passed) (length tests)





