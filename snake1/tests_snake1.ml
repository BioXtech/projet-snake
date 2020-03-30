(*#use "CPtest_sn.ml";;
#use "Snake.ml";;*)

open CPtest_sn
open CPtest_sn
open Snake1
(** Ceci est le fichier de test de la première version du jeu snake
    @version 1.0
 *)
   
(* Nous verifions ici si la valeur de sortie correspond bien a la valeur calculee *)
(** Test fonctionnel de mygraphic_x
    @author Duc.
    @since 1.0
 *)
let test_mygraphic_x_functionnal() : unit =
  let test_status : t_test_status  = create_test_status("mygraphic_x") in
  (
    test_func_equals_value(test_status,"La valeur corresponds bien",mygraphic_x,(5),125);
    test_func_equals_value(test_status,"Les valeurs corresponds bien",mygraphic_x,(0),100);
    test_func_equals_value(test_status,"Les valeurs corresponds bien",mygraphic_x,(99),595);
    print_test_status(test_status);
  )
;;
test_mygraphic_x_functionnal();;

(** Test fonctionnel de mygraphic_y 
    @author Duc.
    @since 1.0
 *)
let test_mygraphic_y_functionnal() : unit =
  let test_status : t_test_status  = create_test_status("mygraphic_x") in
  (
    test_func_equals_value(test_status,"La valeur corresponds bien",mygraphic_y,(5),125);
    test_func_equals_value(test_status,"Les valeurs corresponds bien",mygraphic_y,(0),100);
    test_func_equals_value(test_status,"Les valeurs corresponds bien",mygraphic_y,(99),595);
    print_test_status(test_status);
  )
;;
test_mygraphic_y_functionnal();;

(** Test strucurel de color_of_value
Selon la valeur du type value, on vérifie qu'elle corresponde bien à la couleur demandÃ©e 
      @author Duc.
      @since 1.0
*)
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
test_color_of_value_structural();;

(** Test fonctionnel de init_snake_aux
    @author Guillaume
    @since 1.0
 *)
let test_init_snake_aux_functionnal() : unit =
  let test_status : t_test_status = create_test_status("init_snake_aux") in
  (
    test_func_equals_value(test_status,"Crée un serpent de taille initiale",init_snake_aux,(mysnake_length_init()),[
	{pt = {x = 20; y = 20}; dir = LEFT};
	{pt = {x = 21; y = 20}; dir = LEFT};
	{pt = {x = 22; y = 20}; dir = LEFT}

]);
    test_func_equals_value(test_status,"Crée un serpent de taille 4",init_snake_aux,(4),[
	{pt = {x = 20; y = 20}; dir = LEFT};
	{pt = {x = 21; y = 20}; dir = LEFT};
	{pt = {x = 22; y = 20}; dir = LEFT};
	{pt = {x = 23; y = 20}; dir = LEFT}

]);
    test_func_equals_value(test_status,"Crée un serpent de taille 0",init_snake_aux,(0),[
	{pt = {x = 20; y = 20}; dir = LEFT}
]);
    print_test_status(test_status);
  )
;;
test_init_snake_aux_functionnal();;

(** Test fonctionnel de init_snake
    @author Guillaume
    @since 1.0
 *)
let test_init_snake_functionnal() : unit =
  let test_status : t_test_status = create_test_status("init_snake") in
  (
    test_func_similar_list(test_status,"Crée un serpent de taille initiale", init_snake,(),[
	{pt = {x = 20; y = 20}; dir = LEFT};
	{pt = {x = 21; y = 20}; dir = LEFT};
	{pt = {x = 22; y = 20}; dir = LEFT}

]);
    print_test_status(test_status);
  )
;;
test_init_snake_functionnal();;

(** Test fonctionnel de init_snake_matrix
    @author Guillaume
    @since 1.0
 *)
let test_init_snake_matrix_functionnal() : unit =
  let test_status : t_test_status = create_test_status("init_snake_matrix") and test_matrix : t_matrix = init_matrix() in
  (
    test_matrix.(20).(20) <- SNAKE;
    test_matrix.(21).(20) <- SNAKE;
    test_matrix.(22).(20) <- SNAKE;
    test_func_equals_value(test_status,"Verifie que les positions du serpent sont bien au bonne endroit dans la matrice", init_snake_matrix, (), ([                                                                                           {pt = {x = 20; y = 20}; dir = LEFT};
	{pt = {x = 21; y = 20}; dir = LEFT};
	{pt = {x = 22; y = 20}; dir = LEFT}],test_matrix));
    print_test_status(test_status);
  )
;;
test_init_snake_matrix_functionnal();;

(** Test fonctionnel de init_play
    @author Guillaume
    @since 1.0
 *)
let test_init_play_functionnal() : unit =
  let test_status : t_test_status = create_test_status("init_play") and (test_snake,test_matrix) : t_snake * t_matrix = init_snake_matrix() in
  (
    test_func_equals_value(test_status, "Verifie que le plateau de jeu s'initialise bien", init_play, (), (
                             {dt = {contents = mydt_init()};
                              sn = {contents = [{pt = {x = 20; y = 20}; dir = LEFT};
                                                {pt = {x = 21; y = 20}; dir = LEFT};
                                                {pt = {x = 22; y = 20}; dir = LEFT}]};
                              mat = test_matrix})
      );
    
    print_test_status(test_status);
  )
;;
test_init_play_functionnal();;

(** Test fonctionnel de compute_new_position 
    On vérifie bien que la case attendue corresponde bien à celle calculée
    @author Duc.
    @since 1.0
 *)
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

(** Test fonctionnel de compute_move
    @author Duc.
    @since 1.0
*)
let test_compute_move_functionnal() : unit =
  let test_status : t_test_status = create_test_status("compute_new_position") in
  (
    test_func_equals_value(test_status,"Fait avancer le serpent à gauche pour le faire sortir du plateau",compute_move,({pt = { x = 0 ; y = 0};dir = LEFT},LEFT,init_matrix()),({pt = { x = -1 ; y = 0};dir = LEFT},FRAME));
    test_func_equals_value(test_status,"Fait monter le serpent pour le faire sortir du plateau",compute_move,({pt = { x = 100 ; y = 100};dir = UP},UP,init_matrix()),({pt = { x = 100 ; y = 101};dir = UP},FRAME));
    test_func_equals_value(test_status,"Fait avancer le serpent à droite",compute_move,({pt = { x = 20 ; y = 20};dir = RIGHT},RIGHT,init_matrix()),({pt = { x = 21 ; y = 20};dir = RIGHT},EMPTY));
    print_test_status(test_status);
  )
;;

test_compute_move_functionnal();;
