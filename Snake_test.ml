#use "CPtest_sn.ml";;
#use "Snake.ml";;

(* Nous verifions ici si la valeur de sortie correspond bien a la valeur calculee *)
let test_mygraphic_x_functionnal() : unit =
  let test_status : t_test_status  = create_test_status("mygraphic_x") in
  (
    test_func_equals_value(test_status,"La valeur corresponds bien",mygraphic_x,(5),105);
    test_func_equals_value(test_status,"Les valeurs corresponds bien",mygraphic_x,(0),100);
    test_func_equals_value(test_status,"Les valeurs corresponds bien",mygraphic_x,(99),199);
    print_test_status(test_status);
  )
;;

let test_mygraphic_y_functionnal() : unit =
  let test_status : t_test_status  = create_test_status("mygraphic_x") in
  (
    test_func_equals_value(test_status,"La valeur corresponds bien",mygraphic_y,(5),105);
    test_func_equals_value(test_status,"Les valeurs corresponds bien",mygraphic_y,(0),100);
    test_func_equals_value(test_status,"Les valeurs corresponds bien",mygraphic_y,(99),199);
    print_test_status(test_status);
  )
;;
