(** Miscellaneous functions *)

open Owl

(** Run this command within your Jupyter notebook to update to the latest
    version of the Pkp library. Do not forget to restart your kernel afterwards,
    and run [#require "pkp"] again. *)
val upgrade : unit -> (unit, [> Rresult.R.msg ]) Bos.OS.result

val quiet_owl : unit -> unit
val hooting_owl : unit -> unit

(** {1 Interact with the notebook} *)

(** Prints a message *)
val print_msg : string -> unit

(** [info_printer ()] returns a function which will display any [string] message
    (passed in as argument) in the same area of the output cell. *)
val info_printer : unit -> string -> unit

(** Take an iteri / mapi function, and make a new one that indicates
    progress in the notebook *)
val with_indicator : ((int -> 'a -> 'b) -> 'c -> 'd) -> (int -> 'a -> 'b) -> 'c -> 'd

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
