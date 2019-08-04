(** Miscellaneous functions *)

open Owl

(** {1 Library upgrade} *)

(** Run this command within your Jupyter notebook to update to the latest
    version of the Pkp library. Do not forget to restart your kernel afterwards,
    and run [#require "pkp"] again. *)
val upgrade : unit -> (unit, [> Rresult.R.msg ]) Bos.OS.result

(** {1 Interacting with the notebook} *)

(** Stop printing the content of Owl matrices in the notebook. *)
val quiet_owl : unit -> unit

(** Resume printing the content of Owl matrices in the notebook. *)
val hooting_owl : unit -> unit

type placeholder

(** Returns a placeholder for output messages to be displayed in the notebook.
    Many functions below accept such placeholders as optional arguments. *)
val placeholder : unit -> placeholder

(** Prints a message (optionally in a placeholder you specify, otherwise in a
    new placeholder) *)
val print_msg : ?ph:placeholder -> string -> unit

(** Take an iter / map function (such as [Array.{map/iter}, List.{map/iter}, Mat.{map/iter}, ...]),
    and make a new one that does the same thing but also indicates progress
    in the notebook. For example:
    {[
      let a = Mat.linspace 0. 1. 10
      let ph = placeholder ()
      let _ = (with_indicator Mat.iter)
        (fun x -> print_msg ~ph (string_of_float x); Unix.sleepf 0.2)
        a
    ]} *)
val with_indicator
  :  ?ph:placeholder
  -> ?description:string
  -> (('a -> 'b) -> 'c -> 'd)
  -> ('a -> 'b)
  -> 'c
  -> 'd

(** [average_over n f] is the average of [f 0, f 1, f 2, ..., f (n-1)].
    If [f] takes a long time to evaluate (e.g. because it involves a lot of
    computational work), you might want to use the optional [display] argument
    to monitor progress in the notebook. This optional argument should be a
    pair of the form [(placeholder, string_description)] (try it!). For example:
    {[
      let ph = placeholder ()
      let result = average_over ~display:(ph, "i") 100 (fun i -> Unix.sleepf 0.2; float i)
    ]}
    will return the average of all integers between 0 and 99 (which is 49.5),
    and display progress along the way (I intentionally slow down the
    computation by using [Unix.sleepf 0.2] to force OCaml to wait 200ms each
    time). *)
val average_over : ?display:placeholder * string -> int -> (int -> float) -> float

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

(** {1 Histograms} *)

(** Computes a histogram given a matrix of numbers. Returns a matrix with two columns,
    where the first column are the bins (x-axis) and the second column are the counts
    (number of elements in the input matrix that fall within the corresponding bin).
    You may plot the result using:
    {[
      P.plot (A your_histogram) ~style:"boxes fs solid 0.5 lc 8" default_props
    ]}
*)
val hist : n_bins:int -> Mat.mat -> Mat.mat
