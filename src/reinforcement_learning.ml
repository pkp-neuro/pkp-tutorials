(** Dynamic programming and memoization applied to a simple
    reinforcement learning scenario (TIC-TAC-TOE) *)

(** Some types to represent marks, boards, etc *)

type mark =
  | O
  | X

type outcome =
  | Unfinished
  | Win
  | Lose
  | Tie

type board = mark option array array

type player =
  { play : board -> board
  ; mark : mark
  }

(** The student's mission *)
module type Mission = sig
  val other_mark : mark -> mark
  val empty_board : board
  val mean : float list -> float
  val transpose : board -> board
  val is_full : board -> bool

  val play_game
    :  display:(board -> unit)
    -> finished:(board -> bool)
    -> player * player
    -> unit
end

module Make (M : Mission) = struct
  open M

  let full_of mark v = Array.fold_left (fun accu s -> accu && s = Some mark) true v

  (* check if [player] wins on any row *)
  let row_win mark b = Array.fold_left (fun accu row -> accu || full_of mark row) false b

  (* check if [mark] wins on any of the two diagonals *)
  let diag_win mark b =
    let n = Array.length b in
    let ids = Array.init n (fun i -> i) in
    let check_for j_of =
      Array.fold_left (fun accu i -> accu && b.(i).(j_of i) = Some mark) false ids
    in
    check_for (fun i -> i) || check_for (fun i -> n - i - 1)


  (* Given a board configuration b, return all possible next configurations given
   that it is [mark]'s turn to play *)
  let choices mark b =
    let n = Array.length b in
    let ids = List.init n (fun i -> List.init n (fun j -> i, j)) |> List.concat in
    ids
    |> List.filter (fun (i, j) -> b.(i).(j) = None)
    |> List.map (fun (i, j) ->
           let b' = Array.map Array.copy b in
           b'.(i).(j) <- Some mark;
           b')


  (** Evaluate a board configuration *)
  let evaluate mark (b : board) =
    let wins mark b = row_win mark b || row_win mark (transpose b) || diag_win mark b in
    if wins mark b
    then Win
    else if wins (other_mark mark) b
    then Lose
    else if is_full b
    then Tie
    else Unfinished


  let finished =
    let finished mark b =
      match evaluate mark b with
      | Unfinished -> false
      | _ -> true
    in
    fun b -> finished X b || finished O b


  (* value function using dynamic programming, assuming the opponent 
     takes random actions *)
  let value value_fun (mark, b) =
    match evaluate mark b with
    | Win -> b, 2.0
    | Tie -> b, 1.0
    | Lose -> b, 0.0
    | Unfinished ->
      let cs = choices mark b in
      (match cs with
      | [ final_board ] -> value_fun (mark, final_board)
      | _ ->
        (* if the opponent can still play...
         ... evaluate the opponent and return resulting boards *)
        let results =
          List.map
            (fun c ->
              let outcomes = choices (other_mark mark) c in
              c, mean (List.map (fun o -> snd (value_fun (mark, o))) outcomes))
            cs
        in
        (* extract the best choice & associated value *)
        List.fold_left
          (fun accu (c, v) -> if v > snd accu then c, v else accu)
          ([||], -1000000.0)
          results)


  let random mark =
    let play b =
      let cs = choices mark b |> Array.of_list in
      cs.(Random.int (Array.length cs))
    in
    { play; mark }


  let optimal mark =
    let play =
      let p = Misc.memoize_rec value in
      fun b -> fst (p (mark, b))
    in
    ignore (play empty_board);
    (* traverse the whole tree once *)
    { play; mark }


  let display_in_notebook ~display_id =
    let to_string = function
      | Some O -> "<td>O</td>"
      | Some X -> "<td>X</td>"
      | None -> "<td>_</td>"
    in
    fun b ->
      let s =
        b
        |> Array.map (fun r ->
               r |> Array.map to_string |> Array.to_list |> String.concat "")
        |> Array.to_list
        |> List.map (fun v -> "<tr>" ^ v ^ "</tr>")
        |> String.concat ""
      in
      Jupyter_notebook.printf "<table style=\"border: 1px solid;\">%s</table>%!" s;
      Jupyter_notebook.display_formatter ~display_id "text/html" |> ignore;
      Unix.sleepf 0.1


  let play (player1, player2) =
    let display_id = Jupyter_notebook.display "text/html" "" in
    let display = display_in_notebook ~display_id in
    play_game ~display ~finished (player1, player2)
end

module Solution : Mission = struct
  let other_mark = function
    | X -> O
    | O -> X


  let empty_board = Array.make 3 (Array.make 3 None)

  let mean x =
    let n = List.length x in
    List.fold_left ( +. ) 0. x /. float n


  let transpose b =
    let n = Array.length b in
    Array.init n (fun i -> Array.init n (fun j -> b.(j).(i)))


  let is_full b =
    Array.fold_left (fun accu row -> accu && not (Array.mem None row)) true b


  let play_game ~display ~finished (player, next_player) =
    let rec iter b (p, np) =
      display b;
      if not (finished b) then iter (p.play b) (np, p)
    in
    iter empty_board (player, next_player)
end
