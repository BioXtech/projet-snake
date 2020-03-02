(* Module for testing functions *)

(* utilisation de #mod_use "CPutil_sn.ml";; OBLIGATOIRE *)
                                                     
open CPutil_sn

(* fonctions de manipulation de listes pour les tests *)

let rec is_list_contains(l, e : 'a list * 'a) : bool =
  match l with
  [] -> false
  | a::sl -> if a = e then true else is_list_contains(sl,e)
;; 


let rec is_list_included(l1,l2 : 'a list * 'a list) : bool =
  let rec rem(l2, a) = 
    match l2 with 
      [] -> failwith ""
      | x::sl2 -> if x = a then sl2 else x::rem(sl2,a)
  in
  match l1 with
  [] -> true
  | a::sl1 -> is_list_contains(l2,a) && is_list_included(sl1,rem(l2,a))
;;


let is_list_similar(l1,l2 : 'a list * 'a list) : bool =
  len(l1) = len(l2) && (is_list_included(l1,l2)) && (is_list_included(l2,l1))
;;
   
(*
exception CP_Test_Failure

let test_nb_current_test   : int ref  = ref 0;;
let test_nb_non_successful : int ref  = ref 0;;
let test_nb_successful     : int ref  = ref 0;;
*)

type t_test_status =
  {
    nb_ok : int ref;
    nb_ko : int ref;
    fname : string;
  }
;;

let create_test_status(func_name : string) : t_test_status =
  { nb_ok = ref 0; nb_ko = ref 0; fname = func_name }
;;

let incr_test_ok(test_status : t_test_status) : unit =
  test_status.nb_ok := !(test_status.nb_ok) + 1
;;

let incr_test_ko(test_status : t_test_status) : unit =
  test_status.nb_ko := !(test_status.nb_ko) + 1
;;

let print_test_status(test_status : t_test_status) : unit =
  let tot_test = !(test_status.nb_ok) + !(test_status.nb_ko) in
  let perc_ok = (!(test_status.nb_ok) * 100) / tot_test in
  let perc_ko = (!(test_status.nb_ko) * 100) / tot_test in
    (
    print_endline("Tests on "^test_status.fname); 
    Printf.printf "\ttests OK: %d/%d (%d%%)" !(test_status.nb_ok) (tot_test) (perc_ok);
    print_newline();
    Printf.printf "\ttests KO: %d/%d (%d%%)" !(test_status.nb_ko) (tot_test) (perc_ko);
    print_newline()
    )
;;

(* ------------------------------------------ *)
(* General testing expected value of function *)
(* ------------------------------------------ *)

let test_func_equals_value (test_status, action, func, arg, expected_value : t_test_status * string * ('a -> 'b) * 'a * 'b) : unit =
  try
    (
      if (func arg) = expected_value
      then incr_test_ok(test_status)
      else
        (
          incr_test_ko(test_status);
          print_endline(test_status.fname ^": "^ action ^ ": KO! Not expected value.")
        )
    ) 
  with e -> let msg = Printexc.to_string e in
            (
              print_endline(test_status.fname ^": "^ action ^ ": KO! An error occur: " ^ msg);
              incr_test_ko(test_status)
            )
;;

let test_func_notequals_value (test_status, action, func, arg, expected_value : t_test_status * string * ('a -> 'b) * 'a * 'b) : unit =
  try
    (
      if (func arg) <> expected_value
      then incr_test_ok(test_status)
      else
        (
          incr_test_ko(test_status);
          print_endline(test_status.fname ^": "^ action ^ ": KO! Not expected value.")
        )
    ) 
  with e -> let msg = Printexc.to_string e in
            (
              print_endline(test_status.fname ^": "^ action ^ ": KO! An error occur: " ^ msg);
              incr_test_ko(test_status)
            )
;;



let test_func_in_list (test_status, action, func, arg, reference_list : t_test_status * string * ('a -> 'b) * 'a * 'b list) : unit =
  try
    (
      if is_list_contains(reference_list,(func arg))
      then incr_test_ok(test_status)
      else
        (
          incr_test_ko(test_status);
          print_endline(test_status.fname ^": "^ action ^ ": KO! Not expected value.")
        )
    ) 
  with e -> let msg = Printexc.to_string e in
            (
              print_endline(test_status.fname ^": "^ action ^ ": KO! An error occur: " ^ msg);
              incr_test_ko(test_status)
            )
;;

let test_func_notin_list (test_status, action, func, arg, reference_list : t_test_status * string * ('a -> 'b) * 'a * 'b list) : unit =
  try
    (
      if not(is_list_contains(reference_list,(func arg)))
      then incr_test_ok(test_status)
      else
        (
          incr_test_ko(test_status);
          print_endline(test_status.fname ^": "^ action ^ ": KO! Not expected value.")
        )
    ) 
  with e -> let msg = Printexc.to_string e in
            (
              print_endline(test_status.fname ^": "^ action ^ ": KO! An error occur: " ^ msg);
              incr_test_ko(test_status)
            )
;;


let test_value_in_func (test_status, action, func, arg, reference_value : t_test_status * string * ('a -> 'b list) * 'a * 'b) : unit =
  try
    (
      if is_list_contains((func arg), reference_value)
      then incr_test_ok(test_status)
      else
        (
          incr_test_ko(test_status);
          print_endline(test_status.fname ^": "^ action ^ ": KO! Not expected value.")
        )
    ) 
  with e -> let msg = Printexc.to_string e in
            (
              print_endline(test_status.fname ^": "^ action ^ ": KO! An error occur: " ^ msg);
              incr_test_ko(test_status)
            )
;;


let test_value_notin_func (test_status, action, func, arg, reference_value : t_test_status * string * ('a -> 'b list) * 'a * 'b) : unit =
  try
    (
      if not(is_list_contains((func arg), reference_value))
      then incr_test_ok(test_status)
      else
        (
          incr_test_ko(test_status);
          print_endline(test_status.fname ^": "^ action ^ ": KO! Not expected value.")
        )
    ) 
  with e -> let msg = Printexc.to_string e in
            (
              print_endline(test_status.fname ^": "^ action ^ ": KO! An error occur: " ^ msg);
              incr_test_ko(test_status)
            )
;;


let test_func_similar_list(test_status,action, func, arg, expected_list : t_test_status * string * ('a -> 'b list) * 'a * 'b list ) : unit =
  try
    (
      if is_list_similar((func arg), expected_list)
      then incr_test_ok(test_status)
      else
        (
          incr_test_ko(test_status);
          print_endline(test_status.fname ^": "^ action ^ ": KO! Not expected list.")
        )
    ) 
  with e -> let msg = Printexc.to_string e in
            (
              print_endline(test_status.fname ^": "^ action ^ ": KO! An error occur: " ^ msg);
              incr_test_ko(test_status)
            )
;;

let test_func_notsimilar_list(test_status,action, func, arg, expected_list : t_test_status * string * ('a -> 'b list) * 'a * 'b list ) : unit =
  try
    (
      if not(is_list_similar((func arg), expected_list))
      then incr_test_ok(test_status)
      else
        (
          incr_test_ko(test_status);
          print_endline(test_status.fname ^": "^ action ^ ": KO! Not expected list.")
        )
    ) 
  with e -> let msg = Printexc.to_string e in
            (
              print_endline(test_status.fname ^": "^ action ^ ": KO! An error occur: " ^ msg);
              incr_test_ko(test_status)
            )
;;

let test_func_included_list(test_status, action, func, arg, reference_list : t_test_status * string * ('a -> 'b list) * 'a * 'b list) : unit =
  try
    (
      if is_list_included((func arg), reference_list)
      then incr_test_ok(test_status)
      else
        (
          incr_test_ko(test_status);
          print_endline(test_status.fname ^": "^ action ^ ": KO! Not expected list.")
        )
    )
  with e -> let msg = Printexc.to_string e in
            (
              print_endline(test_status.fname ^": "^ action ^ ": KO! An error occur: " ^ msg);
              incr_test_ko(test_status)
            )
;;

let test_func_notincluded_list(test_status, action, func, arg, reference_list : t_test_status * string * ('a -> 'b list) * 'a * 'b list) : unit =
  try
    (
      if not(is_list_included((func arg), reference_list))
      then incr_test_ok(test_status)
      else
        (
          incr_test_ko(test_status);
          print_endline(test_status.fname ^": "^ action ^ ": KO! Not expected list.")
        )
    )
  with e -> let msg = Printexc.to_string e in
            (
              print_endline(test_status.fname ^": "^ action ^ ": KO! An error occur: " ^ msg);
              incr_test_ko(test_status)
            )
;;


let test_list_included_func(test_status, action, func, arg, reference_list : t_test_status * string * ('a -> 'b list) * 'a * 'b list) : unit =
  try
    (
      if is_list_included(reference_list, (func arg))
      then incr_test_ok(test_status)
      else
        (
          incr_test_ko(test_status);
          print_endline(test_status.fname ^": "^ action ^ ": KO! Not expected value.")
        )
    )
  with e -> let msg = Printexc.to_string e in
            (
              print_endline(test_status.fname ^": "^ action ^ ": KO! An error occur: " ^ msg);
              incr_test_ko(test_status)
            )
;;

let test_not_list_included_func(test_status, action, func, arg, reference_list : t_test_status * string * ('a -> 'b list) * 'a * 'b list) : unit =
  try
    (
      if not(is_list_included(reference_list, (func arg)))
      then incr_test_ok(test_status)
      else
        (
          incr_test_ko(test_status);
          print_endline(test_status.fname ^": "^ action ^ ": KO! Not expected value.")
        )
    )
  with e -> let msg = Printexc.to_string e in
            (
              print_endline(test_status.fname ^": "^ action ^ ": KO! An error occur: " ^ msg);
              incr_test_ko(test_status)
            )
;;




(* ----------------------------------------------- *)
(* General testing expected failure prefix message *)
(* ----------------------------------------------- *)

let test_failwith(test_status, action, func, arg, expected_msg : t_test_status * string * ('a -> 'b) * 'a * string) : unit =
  try 
    (
      (func arg);
      print_endline(test_status.fname ^": "^ action ^ ": KO! no failure detected.");
      incr_test_ko(test_status)
    ) 
  with 
      Failure(msg) -> 
       (
         if msg = expected_msg
         then 
            (
              incr_test_ok(test_status);
            )
          else 
            (
              print_endline(test_status.fname ^": "^ action ^ ": KO! not expected message (expected: '" ^ msg ^ "')");
              incr_test_ko(test_status);              
            )
        )
    | e -> 
        (
          print_endline(test_status.fname ^": "^ action ^ ": KO: not expected error ("^(Printexc.to_string e)^")");
          incr_test_ko(test_status);
        )
;;

let test_failwith_any (test_status, action, func, arg : t_test_status * string * ('a -> 'b)* 'a ) : unit =
  try 
    (
      (func arg);
      print_endline(test_status.fname ^": "^ action ^ ": KO! no failure detected.");
      incr_test_ko(test_status)
    ) 
  with 
      Failure(_) -> 
       (
         incr_test_ok(test_status);
        )
    | e -> 
        (
          print_endline(test_status.fname ^": "^ action ^ ": KO: not expected error ("^(Printexc.to_string e)^")");
          incr_test_ko(test_status);
        )
;;


(* General test function with predicat *)

let test_func_pred(test_status, action, func, arg, pred : t_test_status * string * ('a -> 'b) * 'a * ('a * 'b -> bool)) : unit =
  try
    (
      if pred(arg, func arg)
      then incr_test_ok(test_status)
      else
        (
          incr_test_ko(test_status);
          print_endline(test_status.fname ^": "^ action ^ ": KO! predicate failure.")
        )
    ) 
  with e -> let msg = Printexc.to_string e in
            (
              print_endline(test_status.fname ^": "^ action ^ ": KO! An error occur: " ^ msg);
              incr_test_ko(test_status)
            )
;;



(* --------------------- *)
(* display tests summary *)
(* --------------------- *)

(*
let test_summary() : unit =
  print_endline("Total number of TESTS : " ^ string_of_int(!test_nb_current_test));
  print_endline("Total number of SUCCESSFUL TESTS : " ^ string_of_int(!test_nb_successful));
  print_endline("Total number of FAILED TESTS : " ^ string_of_int(!test_nb_non_successful))
;;
*)


(* ============================================================================================== *)


(* Partie sur les assertions *)


let var_enable_assert : bool ref = ref true;;

let set_assertion_evaluation(b : bool) : unit =
  var_enable_assert := b
;;


exception CP_Assert_Failure of string

let assert_equals(message, a, b : string * 'a * 'a) : unit =
  if(!var_enable_assert &&  a <> b) then
    raise (CP_Assert_Failure(message))
;;

let assert_not_equals(message, a, b : string * 'a * 'a) : unit =
  if(!var_enable_assert &&  a = b) then
    raise (CP_Assert_Failure message)
;;

let assert_true(message, a : string * bool) : unit =
  if (!var_enable_assert && not a) then
    raise (CP_Assert_Failure(message))
;;

let assert_false(message, a : string * bool) : unit =
  if(!var_enable_assert && a) then
    raise (CP_Assert_Failure(message))
;;


let assert_equals_float(message,a,b,e : string * float * float * float) : unit =
  if(!var_enable_assert && not (abs_float(a -. b) <= abs_float(e))) then
    raise (CP_Assert_Failure(message))
;;

let assert_not_equals_float(message, a,b,e : string * float * float * float) : unit =
  if(!var_enable_assert && (abs_float(a -. b) <= abs_float(e))) then
    raise (CP_Assert_Failure(message))
;;

let assert_similar_list(message, l1, l2 : string * 'a list * 'a list) : unit =
  if(!var_enable_assert && is_list_similar(l1,l2)) then
    raise (CP_Assert_Failure(message))
;;

(*
let assert_equals_list( message, l1, l2 : string * 'a list * 'a list) : unit =
  if(!var_enable_assert && is_list_equal(l1,l2)) then
    raise (CP_Assert_Failure(message))  
;;
*)
