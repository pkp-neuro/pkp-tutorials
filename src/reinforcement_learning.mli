(** Dynamic programming and memoization applied to a simple
    reinforcement learning scenario: TIC-TAC-TOE. *)

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

(** Student: your MISSION is to implement each element of the following module: *)
module type Mission = sig
  (** A function that, given a mark (X or O), returns the other one *)
  val other_mark : mark -> mark

  (** An empty board -- cf [board] type above. *)
  val empty_board : board

  (** The mean of a list of numbers *)
  val mean : float list -> float

  (** The (matrix) transpose of a board *)
  val transpose : board -> board

  (** Check whether a board is full -- i.e. that here is no single [None] element in it. *)
  val is_full : board -> bool

  (** [play_game ?display (player1, player2)] should implement a loop that
      alternates between player1 (to start) and player2, starting from
      [empty_board]; the function should return the final board configuration.
      If the optional argument [display] is provided, you should use it to
      display the current board at each iteration. You also have access (in the
      arguments) to [finished], which says whether the current board corresponds
      to a terminated game. *)
  val play_game
    :  ?display:(board -> unit)
    -> finished:(board -> bool)
    -> player * player
    -> board
end

module Make (M : Mission) : sig
  val optimal : mark -> player
  val random : mark -> player
  val play : ?display:bool -> player * player -> mark option
end

module Solution : Mission
