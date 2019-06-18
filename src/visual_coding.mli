(** Visual coding: dense vs sparse coding *)

open Owl

val minibatch : string -> int -> int -> Arr.arr

module Sparse_coding : sig
  type prms =
    { batch_size : int
    ; n_bases : int
    ; bfgs_max_iter : int
    ; lambda : float
    ; sigma : float
    ; learning_rate : float
    }

  val default_prms : prms

  val optimize
    :  ?callback:(int -> Mat.mat -> Mat.mat -> float -> unit)
    -> prms
    -> (int -> Arr.arr)
    -> unit
end

val plot_bases : ?display_id:Jupyter_notebook.display_id -> int -> Mat.mat -> unit
val default_callback : unit -> int -> Mat.mat -> Mat.mat -> float -> unit
