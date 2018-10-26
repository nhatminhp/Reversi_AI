(* I take the idea mainly from the game Tic-Tac-Toe, from how to define game status variable T,
field, board of field list. 
I also find some heuristic grading tables from the internet. 
I tried different tables and I find that the table I use in this file is the most stable one.  

First I find all positions in game table that have not been marked by any player.
Check all directions (I have a direction list that can be used in recursion) of all the positions in that list.
Afterthat we have a list of valid moves.
I also have function to receive game status, move, and return the game status after that move.
I also use the function called minimax that I take the idea from Tic-Tac-Toe and the idea of its structure is my friend's.
The main idea of minimax is a decision rule in order to
to maximize the minimum gain. To be more detailed, the "maximin" value of a player is the highest value 
that the player can be sure to get without knowing the actions of the other players, 
equivalently, it is the lowest value the other players can force the player to receive when they know the player's action.
Then I have function next_move that take the decided move from function minimax, and function think that receive move from
opponent, get the game status after that opponent's move, and return my next move and game status after my move.

REFERENCES: http://www.samsoft.org.uk/reversi/strategy.htm
            http://en.wikipedia.org/wiki/Computer_Othello *)

structure Reversi_AI =
struct
    (* datatype player = Black | White
    datatype move = Pass | Move of int *)

    (*  average p
    TYPE: player -> player
    PRE: TRUE
    POST: opponent of player p
    EXAMPLES: opponent Black = White
    *)
    fun opponent Black = White
      | opponent White = Black;

    (* heuristic list that is used in minimax *) 
    val heuristic_table = [25, ~5, 14, 10, 10, 14, ~5, 25,
                  ~5, ~7, ~4, 1, 1, ~4, ~7, ~5,
                  14, ~4, 3, 2, 2, 3, ~4, 14,
                  10, 1, 2, ~6, ~6, 2, 1, 10,
                  10, 1, 2, ~6, ~6, 2, 1, 10,
                  14, ~4, 3, 2, 2, 3, ~4, 14,
                  ~5, ~7, ~4, 1, 1, ~4, ~7, ~5,
                  25, ~5, 14, 10, 10, 14, ~5, 25]
                  
    (* depth of game tree that used in minimax *)
    val depth = 5
    (* each position in the board, can be SOME Black, SOME White or NONE *)
    type field = player option
    (* Board that contains list of fields *)
    datatype board = Board of field list
    (* game status, including tuple of player and board *)
    type T = player * board
    val author = "Nhat Minh Pham"
    val nickname = "MinhPham"
    (* board including field list that initiated from start *)
    val init_board = 
      let
        fun init_loop 63 = [NONE]
          | init_loop 27 = (SOME White)::(init_loop 28)
          | init_loop 28 = (SOME Black)::(init_loop 29)
          | init_loop 35 = (SOME Black)::(init_loop 36)
          | init_loop 36 = (SOME White)::(init_loop 37)
          | init_loop a  = NONE::(init_loop (a+1))    
      in
        Board (init_loop 0)
      end

    (*  init p
    TYPE: player -> player * board
    PRE: TRUE
    POST: initial state of game
    EXAMPLES: init Black = (Black, init_board)
    *)
    fun init Black = (Black, init_board)
      | init White = (White, init_board)

    (*  grading (p,b)
    TYPE: player * board -> int
    PRE: TRUE
    POST: return mark of current game state (integer)
    *)
    fun grading (p,b) = 
        let
            val Board b' = b
            fun grading' i = if i > 63 then
                                0
                            else case (List.nth (b', i)) of
                                SOME(pl) => if pl = p then (List.nth (heuristic_table, i)) + (grading' (i+1))
                                            else ~(List.nth (heuristic_table, i)) + (grading' (i+1))
                              | NONE => grading' (i+1)
        in
            grading' 0
        end 

    (*  get_all_NONE b count
    TYPE: player option list -> int -> int list
    PRE: TRUE
    POST: position list of positions in table b that have not been set by any player
    *)
    fun get_all_NONE [] _ = []
      | get_all_NONE (bf::bs) c = if bf = NONE then c::(get_all_NONE bs (c+1)) else get_all_NONE bs (c+1)

    (*  get_all_p b p count
    TYPE: player option list -> player -> int -> int list
    PRE: TRUE
    POST: position list of positions in table b that have been set by player p
    EXAMPLE: get_all_p init_board Black 0 = [28,35]
    *)
    fun get_all_p [] _ _ = []
      | get_all_p (bf::bs) p c = if bf = (SOME p) then c::(get_all_p bs p (c+1)) else get_all_p bs p (c+1)

    (*  get_all_op_p b p count
    TYPE: player option list -> player -> int -> int list
    PRE: TRUE
    POST: position list of positions in table b that have been set by opponent of player p
    EXAMPLE: get_all_op_p init_board Black 0 = [27,36]
    *)  
    fun get_all_op_p [] _ _ = []
      | get_all_op_p (bf::bs) p c = if bf = (SOME (opponent p)) then c::(get_all_op_p bs p (c+1)) else get_all_op_p bs p (c+1)    
 
    (*  member v vlist
    TYPE: ''a -> ''a list -> bool
    PRE: v is a'' and vlist is a'' list
    POST: return result whether v is member of vlist of not
    EXAMPLE: member Move1 [Move 1, Move 2] = true 
    *)
    fun member v [] = false
      | member v (x::xs) = (v=x) orelse member v xs
 
    (*  valid_int_moves (p,b)
    TYPE: player * board -> int list
    PRE: TRUE
    POST: return integer list that are valid position that player p can take as a move in table b
    EXAMPLE: valid_int_moves (init Black) = [19, 26, 37, 44]
    *)
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
            (*  to_direction p c r c1 r1 confirmed
            TYPE: player -> int -> int -> int -> int -> bool -> bool
            PRE: TRUE
            POST: return whether that the position p having column c and row r can be valid in one direction defined by column and row difference between each recursion.
            *)
            fun to_direction p1 c r c1 r1 confirmed = 
              let
                val p_next = (r+r1)*8 + (c+c1)
                val is_p = if (member p1 p_ps) then true else false
                val is_op_p = if (member p1 op_p_ps) then true else false
                val is_none = if (member p1 NONE_ps) then true else false
                val confirmed = if (is_op_p) then true else confirmed
              in
                if (c < 0 orelse c > 7 orelse r < 0 orelse r > 7) then false else
                if (c+c1 < 0 orelse c+c1 > 7 orelse r+r1 < 0 orelse r+r1 > 7) then false else
                if (member p_next NONE_ps) then false else
                if (member p_next op_p_ps andalso is_none andalso confirmed = false) then to_direction (p_next) (c+c1) (r+r1) c1 r1 confirmed else 
                if (member p_next op_p_ps andalso is_none andalso confirmed = true) then false else
                if (member p_next op_p_ps andalso is_op_p) then to_direction (p_next) (c+c1) (r+r1) c1 r1 confirmed else 
                if (member p_next op_p_ps andalso is_p andalso confirmed = false) then false else 
                if (member p_next op_p_ps andalso is_p andalso confirmed = true) then false else
                if (member p_next p_ps andalso is_none andalso confirmed = false) then false else
                if (member p_next p_ps andalso is_none andalso confirmed = true) then false else 
                if (member p_next p_ps andalso is_p andalso confirmed = false) then false else 
                if (member p_next p_ps andalso is_p andalso confirmed = true) then false else
                if (member p_next p_ps andalso is_op_p andalso confirmed = false) then false else
                if (member p_next p_ps andalso is_op_p andalso confirmed = true) then true else false
              end
            val direction_list = [(~1,~1),(~1,0),(~1,1),(0,~1),(0,1),(1,~1),(1,0),(1,1)]
            (*  to_all_direction p d_list
            TYPE: int -> int * int list -> bool
            PRE: TRUE
            POST: return whether that the position p can be valid in all direction (defined in list d_list)
            *)  
            fun to_all_directions pn1 [] = false
              | to_all_directions pn1 ((c1,r1)::crs) = (to_direction pn1 col row c1 r1 false) orelse (to_all_directions pn1 crs)  
          in
            to_all_directions pn direction_list
          end
        (*  to_move_int_list p_list
            TYPE: int list -> int list
            PRE: TRUE
            POST: all valid positions of moves from list p_list 
        *)
        fun to_move_int_list [] = []
          | to_move_int_list (x::xs) = if is_valid x then x::(to_move_int_list xs) else to_move_int_list xs
      in
        to_move_int_list NONE_ps
      end

    (*  length p_list
            TYPE: 'a list -> int
            PRE: TRUE
            POST: length of list p_list 
            EXAMPLE: length [1,2,3] = 3
    *)
    fun length lst = 
      let
        fun recur [] acc = acc
          | recur (_::rest) acc = recur rest (1 + acc)
      in recur lst 0
      end
 
    (*  valid_moves (p,b)
    TYPE: player * board -> move list
    PRE: TRUE
    POST: return move list that are valid position that player p can take as a move in table b
    EXAMPLE: valid_moves (init Black) = [Move 19, Move 26, Move 37, Move 44]
    *)
    fun valid_moves (p,b) =
      let
        val ml = valid_int_moves (p,b)
        fun make_list [] = []
          | make_list (x::xs) = (Move x)::(make_list xs)
      in
        if length ml = 0 then [Pass] else make_list ml
      end
    (*  make_moves (p,b) m
    TYPE: player * board -> move -> player * board
    PRE: TRUE
    POST: return game status after take a move m
    *)
    fun make_move (p,b) m =
      let
        val v = case m of (Move x) => x | Pass => ~1
        val Board b1 = b
        val NONE_ps = get_all_NONE b1 0
        val p_ps = get_all_p b1 p 0
        val op_p_ps = get_all_op_p b1 p 0
        (*  check_directiosn b pn
        TYPE: player option list -> int -> player option list
        PRE: TRUE
        POST: take the position number pn (int), take pn as a move and return the board after move pn
        *)
        fun check_directions b2 pn =
          let
            val col = pn mod 8
            val row = pn div 8
            (*  to_direction b p c r c1 r1
            TYPE: player option list -> player -> int -> int -> int -> int -> bool
            PRE: TRUE
            POST: return board status (player option list) after take the move number p and consider one direction defined by c1 r1 from position p.
            *)
            fun to_direction b3 p1 c r c1 r1  = 
              let
                (*  players_to_flip p c r c1 r1
                TYPE: player -> int -> int -> int -> int -> bool
                PRE: TRUE
                POST: return list of positions that will be flipped to current player in one direction defined by c1 r1, 
                list will contain element [-99] if we cannot flipped any positions in that direction
                *)
                fun players_to_flip p_num cl rw cl1 rw1 =
                  let
                    val p_next = (rw+rw1)*8 + (cl+cl1)
                  in
                    if ((cl+cl1) < 0 orelse (cl+cl1) > 7 orelse (rw+rw1) < 0 orelse (rw+rw1) > 7) then [~99] else
                    if (cl < 0 orelse cl > 7 orelse rw < 0 orelse rw > 7) then [~99] else
                    if (member p_next NONE_ps) then [~99] else
                    if (member p_next op_p_ps andalso member p_num p_ps) then [p_num] else
                    if (member p_next op_p_ps andalso member p_num op_p_ps) then p_num::(players_to_flip p_next (cl+cl1) (rw+rw1) cl1 rw1) else
                    if (member p_next op_p_ps andalso member p_num NONE_ps) then (players_to_flip p_next (cl+cl1) (rw+rw1) cl1 rw1) else
                    if (member p_next p_ps andalso member p_num p_ps) then [~99] else
                    if (member p_next p_ps andalso member p_num op_p_ps) then [p_num] else
                    if (member p_next p_ps andalso member p_num NONE_ps) then [~99] else [~99]
 
                  end
                val players_to_flip_list = players_to_flip p1 c r c1 r1
                (*  mark_position b p count
                TYPE: player option list -> int -> int -> player option list
                PRE: TRUE
                POST: return board status (player option list) after take one move number p and mark the position with current player
                *)
                fun mark_position [] p2 c = []
                  | mark_position (bb::bbs) p2 c = if (c = p2) then (SOME p)::(mark_position bbs p2 (c+1)) else bb::(mark_position bbs p2 (c+1))
                (*  change_board b p_list
                TYPE: player option list -> int list -> player option list
                PRE: TRUE
                POST: return board status (player option list) after take all moves from list p_list and mark all the positions with current player
                *)
                fun change_board bo [] = bo
                  | change_board bo (x::xs) = change_board (mark_position bo x 0) xs
 
                val b3 = if (member (~99) players_to_flip_list) then b3 else change_board b3 players_to_flip_list
              in
                b3
              end
            val direction_list = [(~1,~1),(~1,0),(~1,1),(0,~1),(0,1),(1,~1),(1,0),(1,1)] 
            (*  to_all_direction board p_num d_list
            TYPE: player option list -> int -> int * int list -> player option list
            PRE: TRUE
            POST: return list of positions that will be flipped to current player in all direction, 
            *) 
            fun to_all_directions b3 pn1 [] = b3
              | to_all_directions b3 pn1 ((c1,r1)::crs) = to_all_directions (to_direction b3 pn1 col row c1 r1) pn1 crs
            val b2 = to_all_directions b2 pn direction_list
            (*  mark_position b p count
                TYPE: player option list -> int -> int -> player option list
                PRE: TRUE
                POST: return board status (player option list) after take one move number p and mark the position with current player
            *)
            fun mark_position [] p2 c = []
              | mark_position (bb::bbs) p2 c = if (c = p2) then (SOME p)::(mark_position bbs p2 (c+1)) else bb::(mark_position bbs p2 (c+1))
            val b2 = mark_position b2 pn 0 
          in
            b2
          end      
      in
        if v = ~1 then (p,b) else (p, Board (check_directions b1 v))
      end
    (* minimax (p,b)
      TYPE: player * board -> move * int
      PRE:  true
      POST: next move containing the highest mark after recursively traverse the game tree with defined height.
    *)
    fun minimax (p,b) =
        let
            (* minimax_value i (p,b)
              TYPE: int -> player * board -> move * int
              PRE:  true
              POST: next move containing the highest mark after recursively traverse the game tree with defined height.
            *)
            fun minimax_value i (p1,b1) =
                let
                    fun extremum cmp [] = (Pass, grading (p1, b1))
                    | extremum cmp [(m, v)] = (m, v)
                    | extremum cmp ((m1, v1)::(m2, v2)::xs) =
                        if Int.compare (v1, v2) = cmp then
                            extremum cmp ((m1, v1)::xs)
                        else
                            extremum cmp ((m2, v2)::xs)
                    val max = extremum GREATER
                    val min = extremum LESS
                in
                    if i > depth then
                        (Pass, grading (p1,b1))
                    else if i mod 2 = 1 then
                        let
                            val moves = valid_moves (p1,b1)
                            val positions = map (make_move (p1,b1)) moves
                            val moves_and_values = map (minimax_value (i+1)) positions
                            val values = map (fn (_, v) => v) moves_and_values
                        in
                            max (ListPair.zipEq (moves, values))
                        end
                    else
                        let
                            val moves = valid_moves (opponent p1, b1)
                            val positions = map (make_move (opponent p1, b1)) moves
                            val moves_and_values = map (minimax_value (i+1)) positions
                            val values = map (fn (_, v) => v) moves_and_values
                        in
                            min (ListPair.zipEq (moves, values))
                        end
                end
        in
            minimax_value 1 (p,b)
        end

    (* next_move (p,b)
      TYPE: player * board -> move
      PRE:  true
      POST: next move from current player p and current game table b
    *)
    fun next_move (p,b) = 
      let
        val vm = valid_moves (p,b)
        val next_m = minimax (p,b)
        val (mm, a) = next_m 
      in
        mm
      end

    (* next_move (p,b)
      TYPE: (player * board) * move * 'a -> move * (player * board)
      PRE:  true
      POST: next move of current player and the board after making the move.
    *)
    fun think ((p,b), m, t) = 
      let
          val cur_p = make_move (opponent p,b) m
          val (p1,b1) = cur_p
          val nm = next_move (p, b1)
          val next_p = make_move (p, b1) nm
      in
          (nm, next_p)
      end
end;