#use "CPtest_sn.ml";;
#use "snake2.ml";;

(** Test fonctionnel de increase_score() 
    @author Duc.
    @since 2.0
 *))

let test_increase_score() : unit =
  let test_status : t_test_status = create_test_status("increase_score") in
  (
    test_func_equals_value(test_status,"La valeur est passé de 0 à 10",increase_score,
                           ({dt = ref (mydt_init());sn = ref (init_snake()); mat = init_matrix() ; score = ref 0 })
                           ,(10));
    print_test_status(test_status);  
  )
;;
 
