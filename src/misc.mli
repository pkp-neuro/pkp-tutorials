(** Miscellaneous functions *)

open Owl

(** Run this command within your Jupyter notebook to update to the latest
    version of the Pkp library. Do not forget to restart your kernel afterwards,
    and run [#require "pkp"] again. *)
val upgrade : unit -> (unit, [> Rresult.R.msg ]) Bos.OS.result

val quiet_owl: unit -> unit
val hooting_owl: unit -> unit

(** {1 Interact with the notebook} *)

(** Prints a message *)
val print_msg : string -> unit

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

(** {1 Memoization} *)

val memoize : ('a -> 'b) -> 'a -> 'b
val memoize_rec : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
