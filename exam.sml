val t = [          0,   1,   2,   3,   4,   5,   6,   7,
                   8,   9,  10,  11,  12,  13,  14,  15,
                  16,  17,  18,  19,  20,  21,  22,  23,
                  24,  25,  26,(*27,  28,*)29,  30,  31,
                  32,  33,  34,(*35,  36,*)37,  38,  39,
                  40,  41,  42,  43,  44,  45,  46,  47,
                  48,  49,  50,  51,  52,  53,  54,  55,
                  56,  57,  58,  59,  60,  61,  62,  63   ];
val b_board = [   0,  0,  0,  0,  0,  0,  0,  0,
                  0,  0,  0,  0,  0,  0,  0,  0,
                  0,  0,  0,  0,  0,  0,  0,  0,
                  0,  0,  0,  1, ~1,  0,  0,  0,
                  0,  0,  0, ~1,  1,  0,  0,  0,
                  0,  0,  0,  0,  0,  0,  0,  0,
                  0,  0,  0,  0,  0,  0,  0,  0,
                  0,  0,  0,  0,  0,  0,  0,  0   ];          

signature Reversi =
sig
    datatype player = Black | White
    datatype move = Pass | Move of int (* 0..63 *)
    type T
    val author: string
    val nickname: string
    val init: player -> T
    (* val think: T * move * Time.time -> move * T *)
end;

structure Reversi_AI 
(* :> Reversi *)
=
struct
datatype player = Black | White
fun opponent Black = White
  | opponent White = Black;

datatype move = Pass | Move of int (* 0..63 *)
type field = player option
datatype board = Board of field list
type T = player * board
val author = "Nhat Minh Pham"
val nickname = "BeXuanMike"
val init_board = 
  let
    fun init_loop 63 = []
      | init_loop 27 = (SOME Black)::(init_loop 28)
      | init_loop 28 = (SOME White)::(init_loop 29)
      | init_loop 35 = (SOME White)::(init_loop 36)
      | init_loop 36 = (SOME Black)::(init_loop 37)
      | init_loop a  = NONE::(init_loop (a+1))    
  in
    Board (init_loop 0)
  end

fun init Black = (Black, init_board)
  | init White = (Black, init_board)   
    
fun get_all_NONE [] _ = []
  | get_all_NONE (bf::bs) c = if bf = NONE then c::(get_all_NONE bs (c+1)) else get_all_NONE bs (c+1)
fun get_all_p [] _ _ = []
  | get_all_p (bf::bs) p c = if bf = (SOME p) then c::(get_all_p bs p (c+1)) else get_all_p bs p (c+1)
fun get_all_op_p [] _ _ = []
  | get_all_op_p (bf::bs) p c = if bf = (SOME (opponent p)) then c::(get_all_op_p bs p (c+1)) else get_all_op_p bs p (c+1)    

fun member v [] = false
  | member v (x::xs) = (v=x) orelse member v xs

fun valid_int_moves (p,b) = 
  let
    val Board b1 = b
    val NONE_ps = get_all_NONE b1 0
    val p_ps = get_all_p b1 p 0
    val op_p_ps = get_all_op_p b1 p 0
    fun is_valid pn = 
      let
        val col = pn mod 8
        val row = pn div 8
        fun to_direction p1 c r c1 r1 confirmed = 
          let
            val p_next = (r+r1)*8 + (c+c1)
            val confirmed = if (member p1 op_p_ps) then true else confirmed
            val is_p = if (member p1 p_ps) then true else false
            val is_op_p = if (member p1 op_p_ps) then true else false
          in
            if (c < 0 orelse c > 7 orelse r < 0 orelse r > 7) then false else
            if (member p_next NONE_ps) then false else 
            if (member p_next p_ps andalso confirmed = true) then true else 
            if (member p_next p_ps andalso confirmed = false) then false else
            if (member p_next op_p_ps andalso is_p) then false else
            if (member p_next op_p_ps) then to_direction p_next (c+c1) (r+r1) c1 r1 confirmed else false
          end
        val direction_list = [(~1,~1),(~1,0),(~1,1),(0,~1),(0,0),(0,1),(1,~1),(1,0),(1,1)]  
        fun to_all_directions pn1 [] = false
          | to_all_directions pn1 ((c1,r1)::crs) = (to_direction pn1 col row c1 r1 false) orelse (to_all_directions pn1 crs)  
      in
        to_all_directions pn direction_list
      end
    fun to_move_int_list [] = []
      | to_move_int_list (x::xs) = if is_valid x then x::(to_move_int_list xs) else to_move_int_list xs
  in
    to_move_int_list NONE_ps
  end

fun length lst = let
    fun recur [] acc = acc
      | recur (_::rest) acc = recur rest (1 + acc)
  in recur lst 0
end

fun valid_moves (p,b) =
  let
    val ml = valid_int_moves (p,b)
    fun make_list [] = []
      | make_list (x::xs) = (Move x)::(make_list xs)
  in
    if length ml = 0 then [Pass] else make_list ml
  end

fun make_move (p,b) m =
  let
    val Move v = m
    val Board b1 = b
    val NONE_ps = get_all_NONE b1 0
    val p_ps = get_all_p b1 p 0
    val op_p_ps = get_all_op_p b1 p 0

    fun check_directions b2 pn =
      let
        val col = pn mod 8
        val row = pn div 8
        fun to_direction b3 p1 c r c1 r1  = 
          let
            fun mark_position [] p2 c = []
              | mark_position (bb::bbs) p2 c = if (c = p2) then (SOME p)::(mark_position bbs p2 (c+1)) else bb::(mark_position bbs p2 (c+1)) 
            val p_next = (r+r1)*8 + (c+c1)
            val b3' = mark_position b3 p1 0
          in
            if (c < 0 orelse c > 7 orelse r < 0 orelse r > 7) then b3 else
            if (member p_next NONE_ps) then b3 else 
            if (member p_next p_ps) then b3' else to_direction b3' p_next (c+c1) (r+r1) c1 r1
          end
        val direction_list = [(~1,~1),(~1,0),(~1,1),(0,~1),(0,0),(0,1),(1,~1),(1,0),(1,1)]  
        fun to_all_directions b3 pn1 [] = b3
          | to_all_directions b3 pn1 ((c1,r1)::crs) = to_all_directions (to_direction b3 pn1 col row c1 r1) pn1 crs
      in
        to_all_directions b2 pn direction_list
      end      
  in
    if m = Pass then (opponent p,b) else (opponent p, Board (check_directions b1 v))
  end

end;


val (x::xs) = Reversi_AI.valid_moves (Reversi_AI.init Reversi_AI.Black);
val t_status = Reversi_AI.make_move (Reversi_AI.init Reversi_AI.Black) x;

Reversi_AI.valid_moves t_status;

val (p, Reversi_AI.Board b) = t_status;

Reversi_AI.get_all_p b (Reversi_AI.Black) 0;
Reversi_AI.get_all_p b (Reversi_AI.White) 0;

val t_status = Reversi_AI.make_move t_status (Reversi_AI.Move 19);
val (p, Reversi_AI.Board b) = t_status;

Reversi_AI.valid_moves t_status;

Reversi_AI.get_all_p b (Reversi_AI.Black) 0;
Reversi_AI.get_all_p b (Reversi_AI.White) 0;

val t_status = Reversi_AI.make_move t_status (Reversi_AI.Move 26);
val (p, Reversi_AI.Board b) = t_status;

(* Reversi_AI.valid_moves t_status; *)

Reversi_AI.get_all_p b (Reversi_AI.Black) 0;
Reversi_AI.get_all_p b (Reversi_AI.White) 0;