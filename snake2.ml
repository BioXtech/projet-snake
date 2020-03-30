#use "CPsnakeV1.ml";;

let snake_grow(pl : t_play) : unit =
  let snake : t_snake = !(pl.sn) in
  let snake_tail : t_position = lst(snake) in
  let x : int ref = ref snake_tail.pt.x and y : int ref = ref snake_tail.pt.y in
  (
    if snake_tail.dir = UP
    then y := !y - 1
    else
      if snake_tail.dir = LEFT
      then x := !x + 1
      else
        if snake_tail.dir = RIGHT
        then x := !x - 1
        else y := !y + 1;
    pl.sn := add_lst(snake,{pt = {x = !x; y = !y}; dir = snake_tail.dir});
  )
;;
