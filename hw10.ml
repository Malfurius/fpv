
module type Ring = sig
  type t
  val zero : t
  val one : t
  val compare : t -> t -> int
  val to_string : t -> string
  val add : t -> t -> t
  val mul : t -> t -> t
end

module type Matrix = sig
  type elem
  type t
  val create : int -> int -> t
  val identity : int -> t
  val from_rows : elem list list -> t
  val to_string : t -> string
  val set : int -> int -> elem -> t -> t
  val get : int -> int -> t -> elem
  val transpose : t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
end


(*****************************************************************************)
(**************************** HOMEWORK STARTS HERE ***************************)
(*****************************************************************************)

let debugString str = Printf.printf "%s" str

module IntRing : Ring with type t = int = struct
  type t = int
  let zero = 0
  let one = 1
  let compare a b = (a-b)
  let to_string a = string_of_int a
  let add a b = a+b
  let mul a b = a*b
end

module FloatRing : Ring with type t = float = struct
  type t = float
  let zero = 0.0
  let one = 1.0
  let compare a b = if((a-.b) > 0.0)then 1 else (if (a-.b<0.0)then -1 else 0)
  let to_string a = string_of_float a
  let add a b = a+.b
  let mul a b = a*.b
end

module type FiniteRing = sig
  include Ring
  val elems : t list
end

module BoolRing : FiniteRing with type t = bool = struct
  type t = bool
  let zero = false
  let one = true
  let compare a b = if(a=b)
                    then 0
                    else (if(a=one)then 1 else (-1))
                    
  let add a b = if(b=zero)then a else (not a)
  let mul a b = if (b=one)then a else (not a)
  let to_string = string_of_bool
  let elems = [true;false]
end


  
  let rec recUnion a l=
    match a with
      | x::xs -> if(not (List.mem x l))then (recUnion xs (x::l))else(recUnion xs l)
      | [] -> l
  
  let rec recInter a b l=
    match a with
      | x::xs -> if((List.mem x b))then (recInter xs b (x::l))else(recInter xs b l)
      | [] -> l

  let rec setToString b f s = 
    match b with
    | x::xs -> let ns = s^(f x)^";" in setToString xs f ns
    | [] -> s^"}"

  let rec recCompare a b c = 
    match a, b with
    | x::xs, y::ys -> if(x=y)then(recCompare xs ys c)else(c x y)
    | [], y::ys -> -1
    | x::xs, [] -> 1
    | [], [] -> 0

module SetRing (F : FiniteRing) : Ring with type t = F.t list = struct
  type t = F.t list
  let zero = []
  let one = F.elems
  let add a b = (List.sort F.compare (recUnion a b))
  let mul a b = (recInter a b [])
  let compare a b = let comp = F.compare in recCompare (List.sort comp a) (List.sort comp b) comp
  let to_string a = let st = "{"^(setToString a F.to_string "") in debugString st; st
end

let rec createRows m r v =
  if(m>0)
  then (createRows (m-1) (v::r) v)
  else r 


let rec buildMatrix n m c v=
  if(n>0)
  then(buildMatrix (n-1) m ((createRows m [] v)::c) v)
  else c

let rec createTransposedRow n m r g b =
  if(m>0)
  then (createTransposedRow n (m-1) ((g m n b)::r) g b)
  else r

let rec buildTransposedMatrix n m c g b =
  if(n>0)
  then (buildTransposedMatrix (n-1) m ((createTransposedRow n m [] g b)::c) g b)
  else c

let rec fillIDColumn n m c neut null=
  if(m>0)
  then (if(n=m)
        then (fillIDColumn n (m-1) (neut::c) neut null)
        else (fillIDColumn n (m-1) (null::c) neut null))
  else c

let rec buildIDMatrix n m c neut null =
  if(n>0)
  then (buildIDMatrix (n-1) m ((fillIDColumn n m [] neut null)::c) neut null)
  else c

let rec buildRowMatrix l c =
  match l with
  | x::xs -> buildRowMatrix xs (x::c)
  | [] -> c


let replace c v r = (List.mapi (fun i x -> if(i=c)then v else x) r)

let find c m = (List.nth m c)

let printRow r m = (List.iter (fun x -> debugString ((m x)^" ")) r)

let rec findRow r m n  =
  match m with
  | x::xs -> if(n=r)then(x)else(findRow r xs (n+1))
  | [] -> []

let rec recMul r b elem_Idx column_Idx sum get add mul = 
  match r with 
  | x::xs -> recMul xs b (elem_Idx+1) column_Idx (add sum (mul x (get elem_Idx column_Idx b))) get add mul
  | [] -> sum

let perRow r b zero get add mul = List.mapi (fun column_Idx x -> recMul r b 0 column_Idx zero get add mul) r

let swap r c v nM set = (set c r v nM)

let myTranspose m create set=
  let rows = List.length m
  in 
  let columns = List.length (List.hd m)
  in
  let nMatrix = create columns rows
  in
  List.iteri (fun i x -> List.iteri (fun j y -> swap j i y nMatrix set) x) m;nMatrix

(*let isRow r c v m f = (List.mapi (fun i x -> if (i = r) then(f c v x) else(x)) m)
*)
(*
module DenseMatrix (F : Ring) : Matrix with type t = (F.t list list) and type elem = F.t = struct
  type t = (F.t list list)
  type elem = F.t
  let to_string m = (List.iter (fun x -> (printRow x F.to_string);debugString "\n") m);"test"
  let create n m = let res = buildMatrix n m [] F.zero in List.rev res
  let identity n = buildIDMatrix n n [] F.one F.zero
  let from_rows l = l
  let set r c v m = (List.mapi (fun i x -> if(i=r)then(replace c v x)else(x)) m)
  let get r c m = let res = List.nth (List.nth m r) c in F.to_string res;res
  let transpose m = let r = List.length m
                    in 
                    let c = List.length (List.hd m) 
                    in
                    let res = (buildTransposedMatrix c r [] get m) in to_string res;res
  let add a b = let res =  List.mapi (fun i x -> (List.mapi (fun j y -> (F.add y (get i j b) )) x)) a in to_string res; res
  let mul a b = let res = List.mapi (fun i x -> perRow x b F.zero get F.add F.mul) a in to_string res; res
end
*)


let rec findSparse rIdx cIdx row res = match row with
  | (r,c,v)::xs ->if((r=rIdx) && (c=cIdx))
                    then (findSparse rIdx cIdx [] true)
                    else (findSparse rIdx cIdx row res)
  | [] -> res

let setRow rIdx cIdx row value= if(findSparse rIdx cIdx row false)
                           then (rIdx,cIdx,value)::(List.filter (fun (x,y,v) -> if(rIdx=x&&cIdx=y)then(false)else(true)) row)
                           else (rIdx,cIdx,value)::row


let rec addRow rIdx m r a b g add= 
  if(m>0)
  then let sum = (add (g rIdx m a) (g rIdx m b)) in if(sum>0) then (addRow rIdx (m-1) ((rIdx,m,sum)::r) a b g add)else(addRow rIdx (m-1) r a b g add)
  else r


module SparseMatrix (F:Ring) : Matrix with type t = (int*int*((int*int*F.t) list list)) and type elem = (int*int*F.t) = struct
  type t = (int*int*((int*int*F.t) list list))
  type elem = F.t
  let create n m = (n,m, (List.init n (fun i -> [])))
  let id_row n m = List.init m (fun i -> if(n=i)then(n,m,F.one)else())
  let identity = (n,m, (List.init n (fun i -> id_row i m)))
  let from_row m = (List.length m,List.length (List.hd m), List.mapi (fun i r -> List.filter (fun (x,y,v) -> if(v<>F.zero)
then(true)else(false)) (List.mapi (fun j e -> (i,j,e)) r)) m)
  let set n m v (r,c,mat) = (r,c, List.mapi (fun i x-> if(n=i)then setRow n m x v else x) mat)
  let get n m mat = match (List.filter (fun (x,y,v) -> ((n=x) && (m=y))) (List.nth mat n)) with
    | (a,b,value)::xs -> value
    | [] -> 0
  let transpose m = List.map (fun r -> List.mapi (fun (x,y,v) -> (y,x,v)) r) m
  let add a b = let n = List.length a in let m = (List.length List.hd a) in  List.mapi (fun i r ->  (addRow i m r a b get F.add) ) (create n m)
  let mul a b = a
end


(*****************************************************************************)
(**************************** END OF HOMEWORK ********************************)
(*****************************************************************************)

(*****************************************************************************)
(* TESTS [do not change] *)
let (|=) a b =
  List.sort compare a = List.sort compare b

let check_string_representation s elems =
  if String.length s < 2 then false else
  if String.get s 0 <> '{' then false else
  if String.get s (String.length s - 1) <> '}' then false else
  String.sub s 1 (String.length s - 2)
  |> String.split_on_char ','
  |> List.map String.trim
  |> (|=) elems

let tests =
  (****************************
   * tests for 10.2 (IntRing) :
   * NOTE: Comment tests until you have completed your implementation of IntRing
   *)
  
  let implementsRingSignature (module M : Ring) = true in
  [
  __LINE_OF__ (fun () -> implementsRingSignature (module IntRing));
  __LINE_OF__ (fun () -> IntRing.compare 9 10 < 0 && IntRing.compare 10 9 > 0 && IntRing.compare 10 10 = 0);
  __LINE_OF__ (fun () -> IntRing.add 10 IntRing.zero = 10);
  __LINE_OF__ (fun () -> IntRing.mul 10 IntRing.one = 10);
  __LINE_OF__ (fun () -> IntRing.to_string 10 = "10");
  ] @

  (******************************
   * tests for 10.2 (FloatRing) :
   * NOTE: Comment tests until you have completed your implementation of FloatRing
   *)
  
  let implementsRingSignature (module M : Ring) = true in
  [
  __LINE_OF__ (fun () -> implementsRingSignature (module FloatRing));
  __LINE_OF__ (fun () -> FloatRing.compare 9.5 10.0 < 0 && FloatRing.compare 10.0 9.5 > 0 && FloatRing.compare 10.0 10.0 = 0);
  __LINE_OF__ (fun () -> FloatRing.add 10.0 FloatRing.zero = 10.0);
  __LINE_OF__ (fun () -> FloatRing.mul 10.0 FloatRing.one = 10.0);
  __LINE_OF__ (fun () -> FloatRing.to_string 10.0 = "10.");
  ] @

  (*****************************
   * tests for 10.2 (BoolRing) :
   * NOTE: Comment tests until you have completed your implementation of BoolRing
   *)
  
  let implementsFiniteRingSignature (module M : FiniteRing) = implementsRingSignature (module M) in
  [
  __LINE_OF__ (fun () -> implementsFiniteRingSignature (module BoolRing));
  __LINE_OF__ (fun () -> BoolRing.compare BoolRing.zero BoolRing.one < 0 && BoolRing.compare BoolRing.one BoolRing.zero > 0 && BoolRing.compare BoolRing.zero BoolRing.zero = 0);
  __LINE_OF__ (fun () -> BoolRing.add true BoolRing.zero = true && BoolRing.add false BoolRing.zero = false);
  __LINE_OF__ (fun () -> BoolRing.mul true BoolRing.one = true && BoolRing.mul false BoolRing.one = false);
  __LINE_OF__ (fun () -> BoolRing.to_string true = "true");
  __LINE_OF__ (fun () -> BoolRing.elems |= [true;false]);
  ] @ 

  (****************************
   * tests for 10.2 (SetRing) :
   * NOTE: Comment tests until you have completed your implementation of SetRing
   *)
  
  let module TestRing : FiniteRing with type t = char = struct
    let cfrom x = (int_of_char x) - (int_of_char 'a')
    let cto x = char_of_int (x mod 4 + int_of_char 'a')

    type t = char
    let zero = 'a'
    let one = 'd'
    let compare = Pervasives.compare
    let to_string c = Printf.sprintf "'%c'" c
    let add a b = (cfrom a) + (cfrom b) |> cto
    let mul a b = (cfrom a) * (cfrom b) |> cto
    let elems = ['a'; 'b'; 'c'; 'd']
  end in
  let module SR = SetRing (TestRing) in
  [
  __LINE_OF__ (fun () -> SR.zero = [] && SR.one |= ['a'; 'b'; 'c'; 'd']);
  __LINE_OF__ (fun () -> SR.compare ['b';'d'] ['a'] > 0);
  __LINE_OF__ (fun () -> SR.compare ['c';'b'] ['c';'d'] < 0);
  __LINE_OF__ (fun () -> SR.compare ['a';'d'] ['d';'a'] = 0);
  __LINE_OF__ (fun () -> SR.add ['a';'b'] ['c';'b'] |= ['a';'b';'c']);
  __LINE_OF__ (fun () -> SR.add ['b';'d'] SR.zero |= ['b';'d']);
  __LINE_OF__ (fun () -> SR.mul ['a';'b'] ['c';'b'] |= ['b']);
  __LINE_OF__ (fun () -> SR.mul ['a';'b'] SR.one |= ['a';'b']);
  __LINE_OF__ (fun () -> check_string_representation (SR.to_string SR.one) ["'a'";"'b'";"'c'";"'d'"]);
  ] @ 

  (********************************
   * tests for 10.2 (DenseMatrix) :
   * NOTE: Comment tests until you have completed your implementation of DenseMatrix
   * NOTE: from_rows and get have to be correct in order for these tests to work correctly!
   *)
  
  let module DM = DenseMatrix (IntRing) in
  let dm0 = DM.from_rows [[4;-2;1];[0;3;-1]] in
  let dm1 = DM.from_rows [[1;2];[-3;4];[3;-1]] in
  let check_dense m l =
    List.mapi (fun r row -> List.mapi (fun c col -> col = DM.get r c m) row) l |> List.flatten |> List.for_all (fun x -> x)
  in
  [
    __LINE_OF__ (fun () -> check_dense (DM.create 2 3) [[0;0;0];[0;0;0]]);
    __LINE_OF__ (fun () -> check_dense (DM.identity 3) [[1;0;0];[0;1;0];[0;0;1]]);
    __LINE_OF__ (fun () -> check_dense (DM.set 1 0 7 (DM.identity 2)) [[1;0];[7;1]]);
    __LINE_OF__ (fun () -> check_dense (DM.transpose dm0) [[4;0];[-2;3];[1;-1]]);
    __LINE_OF__ (fun () -> check_dense (DM.add dm0 dm0) [[8;-4;2];[0;6;-2]]);
    __LINE_OF__ (fun () -> check_dense (DM.mul dm0 dm1) [[13;-1];[-12;13]]);
    __LINE_OF__ (fun () -> (DM.to_string dm0) = "4 -2 1\n0 3 -1");
  ] @ 

  (*********************************
   * tests for 10.2 (SparseMatrix) :
   * NOTE: Comment tests until you have completed your implementation of SparseMatrix
   * NOTE: from_rows and get have to be correct in order for these tests to work correctly!
   *)
  (*
  let module SM = SparseMatrix (IntRing) in
  let sm0 = SM.from_rows [[4;-2;1];[0;3;-1]] in
  let sm1 = SM.from_rows [[1;2];[-3;4];[3;-1]] in
  let check_sparse m l =
    List.mapi (fun r row -> List.mapi (fun c col -> col = SM.get r c m) row) l |> List.flatten |> List.for_all (fun x -> x)
  in
  [
    __LINE_OF__ (fun () -> check_sparse (SM.create 2 3) [[0;0;0];[0;0;0]]);
    __LINE_OF__ (fun () -> check_sparse (SM.identity 3) [[1;0;0];[0;1;0];[0;0;1]]);
    __LINE_OF__ (fun () -> check_sparse (SM.set 1 0 7 (SM.identity 2)) [[1;0];[7;1]]);
    __LINE_OF__ (fun () -> check_sparse (SM.transpose sm0) [[4;0];[-2;3];[1;-1]]);
    __LINE_OF__ (fun () -> check_sparse (SM.add sm0 sm0) [[8;-4;2];[0;6;-2]]);
    __LINE_OF__ (fun () -> check_sparse (SM.mul sm0 sm1) [[13;-1];[-12;13]]);
    __LINE_OF__ (fun () -> (SM.to_string sm0) = "4 -2 1\n0 3 -1");
  ] @ *)
  []


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


