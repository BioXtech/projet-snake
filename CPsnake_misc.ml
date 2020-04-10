
(* direction donnee par la difference *)
(* entre la position pos et la souris *)

let analyze(pos : t_position) : t_direction =
  let (i, j) : int * int = mouse_pos() in
  let (k, l) : int * int = (mygraphic_x(pos.pt.x), mygraphic_y(pos.pt.y)) in
  let v : t_direction =
    if abs(i - k) < abs(j - l)
    then 
      if j > l 
      then UP
      else DOWN
    else
      if i < k
      then LEFT
      else RIGHT
  in
    if (pos.dir = UP && v = DOWN) || (pos.dir = DOWN && v = UP)
    then if i < k then LEFT else RIGHT
    else 
      if (pos.dir = LEFT && v = RIGHT) || (pos.dir = RIGHT && v = LEFT)
      then if j > l then UP else DOWN
      else v
;;




(* gestion de l'acceleration *)

let handle_t_acc(t, t_acc, play : float ref * float ref * t_play) : unit = 
  if (!t -. !t_acc) > mydt_acc()
  then 
    (
    play.dt := !(play.dt) *. mydt_ratio() ; 
    t_acc := !t ;
    ) 
;;

(* fonction de simulation *)
    
let simulation() : unit =
  let pl : t_play = init_play() in
  let t : float ref = ref (Sys.time()) and newt : float ref = ref (Sys.time())  in
  let tacc : float ref = ref (Sys.time()) in 
  let thend : bool ref = ref false in
    (
    while not(!thend)
    do
      newt := Sys.time() ;
      while not((!newt -. !t) > !(pl.dt))
      do newt := Sys.time() ;
      done ; 
      t := !newt ; 
      handle_t_acc(t, tacc, pl) ;
      thend := new_step(pl) ;
    done ;
    )
;;




