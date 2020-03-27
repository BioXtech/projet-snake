#use "CPtest_sn.ml";;
#use "Snake.ml";;

(** Test structurel de mygraphic_x
    @author Duc.
    @since 1.0
 *)
let test_mygraphic_x_functionnal() : unit =
  let test_status : t_test_status  = create_test_status("mygraphic_x") in
  (
    test_func_equals_value(test_status,"La valeur 0 corresponds bien",mygraphic_x,(5),125);
    test_func_equals_value(test_status,"La valeurs 0 corresponds bien",mygraphic_x,(0),100);
    test_func_equals_value(test_status,"La valeurs 0 corresponds bien",mygraphic_x,(99),195);
    print_test_status(test_status);
  )
;;
test_mygraphic_x_functionnal();;
let test_mygraphic_y_functionnal() : unit =
  let test_status : t_test_status  = create_test_status("mygraphic_x") in
  (
    test_func_equals_value(test_status,"La valeur 5 corresponds bien",mygraphic_y,(5),125);
    test_func_equals_value(test_status,"La valeurs 0 corresponds bien",mygraphic_y,(0),100);
    test_func_equals_value(test_status,"La valeurs 99 corresponds bien",mygraphic_y,(99),195);
    print_test_status(test_status);
  )
;;

test_mygraphic_y_functionnal();;
(** Selon la valeur du type value, on vérifie qu'elle corresponde bien à la couleur demandée *)

let test_color_of_value_structural() : unit =
  let test_status : t_test_status = create_test_status("color_of_value") in
  (
    test_func_equals_value(test_status,"La couleur est blanche",color_of_value,(EMPTY),Graphics.white);
    test_func_equals_value(test_status,"La couleur est noir",color_of_value,(PROBLEM),Graphics.black);
    test_func_equals_value(test_status,"La couleur est rouge",color_of_value,(FRAME),Graphics.red);
    test_func_equals_value(test_status,"La couleur est vert",color_of_value,(SNAKE),Graphics.green);
    print_test_status(test_status);
  )
;;

test_color_of_value_structurel();;

let test_compute_new_position_functionnal() : unit =
  let test_status : t_test_status = create_test_status("ompute_new_position") in
  (
    test_func_equals_value(test_status,"La direction est au nord donc le serpent monte",compute_new_position,({pt = {x = 0; y = 0};dir = UP},UP),({pt = {x = 0; y = 1}; dir = UP}));
    test_func_equals_value(test_status,"La direction est au sud donc le serpent descend",compute_new_position,({pt = {x = 5; y = 5};dir = DOWN},DOWN),({pt = {x = 5; y = 4}; dir = DOWN}));
    test_func_equals_value(test_status,"La direction est à l'est donc le serpent tourne à droite",compute_new_position,({pt = {x = 5; y = 5};dir = RIGHT},RIGHT),({pt = {x = 6; y = 5}; dir = RIGHT}));
    test_func_equals_value(test_status,"La direction est à l'ouest donc le serpent tourne à gauche",compute_new_position,({pt = {x = 5; y = 5};dir = LEFT},LEFT),({pt = {x = 4 ; y = 5}; dir = LEFT})); 
    print_test_status(test_status); 
  ) 
;;

test_compute_new_position_functionnal();;


let test_compute_move_functionnal() : unit =
  let test_status : t_test_status = create_test_status("compute_new_position") in
  (
    test_func_equals_value(test_status,"",compute_move,({pt = { x = 0 ; y = 0};dir = LEFT},LEFT,init_matrix()),({pt = { x = -1 ; y = 0};dir = LEFT},FRAME));
    test_func_equals_value(test_status,"",compute_move,({pt = { x = 100 ; y = 100};dir = UP},UP,init_matrix()),({pt = { x = 100 ; y = 101};dir = UP},FRAME));
    print_test_status(test_status);
  )
;;

test_compute_move_functionnal();;

                                                      
