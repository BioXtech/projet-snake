#use "CPtest_sn.ml";;
#use "Snake.ml";;

(** Nous verifions ici si la valeur de sortie correspond bien a la valeur calculee *)
let test_mygraphic_x_functionnal() : unit =
  let test_status : t_test_status  = create_test_status("mygraphic_x") in
  (
    test_func_equals_value(test_status,"La valeur corresponds bien",mygraphic_x,(5),125);
    test_func_equals_value(test_status,"Les valeurs corresponds bien",mygraphic_x,(0),100);
    test_func_equals_value(test_status,"Les valeurs corresponds bien",mygraphic_x,(99),195);
    print_test_status(test_status);
  )
;;
mygraphic_x();;
let test_mygraphic_y_functionnal() : unit =
  let test_status : t_test_status  = create_test_status("mygraphic_x") in
  (
    test_func_equals_value(test_status,"La valeur corresponds bien",mygraphic_y,(5),125);
    test_func_equals_value(test_status,"Les valeurs corresponds bien",mygraphic_y,(0),100);
    test_func_equals_value(test_status,"Les valeurs corresponds bien",mygraphic_y,(99),195);
    print_test_status(test_status);
  )
;;

mygraphic_y();;
(** Selon la valeur du type value, on vérifie qu'elle corresponde bien à la couleur demandée *)

let test_color_of_value_structurel() : unit =
  let test_status : t_test_status = create_test_status("color_of_value") in
  (
    test_func_equals_value(test_status,"La couleur est blanche",color_of_value,(EMPTY),Graphics.white);
    test_func_equals_value(test_status,"La couleur est noir",color_of_value,(PROBLEM),Graphics.black);
    test_func_equals_value(test_status,"La couleur est rouge",color_of_value,(FRAME),Graphics.red);
    test_func_equals_value(test_status,"La couleur est vert",color_of_value,(SNAKE),Graphics.green);
    print_test_status(test_status);
  )
;;



let test_compute_new_position_functionnal() : unit =
  let test_status : t_test_status = create_test_status("ompute_new_position") in
  (
    test_func_equals_value(test_status,"La direction est au nord",compute_new_position,({pt = {x = 0; y = 0};dir = UP},{pt = {x = 0; y = 1}, dir = UP}));
    print_test_status(test_status); 
  ) 
;;

  

