open Owl

(** {1 Interact with the notebook} *)

val silence : unit -> unit
(** Add [let _ = Pkp.Misc.silence ()] at the end of a cell so that
    its output gets cleared right after execution. *)

val time_indicator : float -> int -> unit
(** [time_indicator dt] returns a function to which you can
    pass the current discrete time index t whenever you want
    to display the current simulation time (t*dt) in the notebook *)

val how_long : (unit -> 'a) -> 'a * float
(** [how_long f] returns the evaluation [f ()] along with the
    time (in s) it took to do this. *)

(** {1 Signals} *)

val ou_process : tau:float -> dt:float -> duration:float -> Mat.mat
(** Generates a unit-variance Ornstein-Uhlenbeck process with characteristic 
    time constant [tau], over a [duration] with sampling resolution [dt]. *)

