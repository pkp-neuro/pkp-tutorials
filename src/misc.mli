open Owl

(** {1 Interact with the notebook} *)

(** Add [let _ = Pkp.Misc.silence ()] at the end of a cell so that
    its output gets cleared right after execution. *)
val silence : unit -> unit

(** [time_indicator dt] returns a function to which you can
    pass the current discrete time index t whenever you want
    to display the current simulation time (t*dt) in the notebook *)
val time_indicator : float -> int -> unit

(** [how_long f] returns the evaluation [f ()] along with the
    time (in s) it took to do this. *)
val how_long : (unit -> 'a) -> 'a * float

(** {1 Signals} *)

(** Generates a unit-variance Ornstein-Uhlenbeck process with characteristic 
    time constant [tau], over a [duration] with sampling resolution [dt]. *)
val ou_process : tau:float -> dt:float -> duration:float -> Mat.mat

val memoize : ('a -> 'b) -> 'a -> 'b
val memoize_rec : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
