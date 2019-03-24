open Owl
module P = Pervasives

(** Main module of parameters *)
module type PT = sig
  val w_rec: Mat.mat
  val dt: float
  val sampling_dt: float
  val tau: float (* time constant of cortical dynamics *)
  val noise_prms: (float * Mat.mat) option (* optional input noise parameters *)
  val rate_function: Mat.mat -> Mat.mat
end

module Dynamics (Prms: PT) : sig

  (** Network size *)
  val n: int

  (** simulate the dynamics of the network *)
  val simulate: ?verbose:bool -> duration:float -> Mat.mat

end


