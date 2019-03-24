open Owl

module Poisson_population (P: sig
    val n: int
    val rate: float
    val dt: float
  end): sig

  val step: unit -> unit
  val spikes: unit -> int array

end

module LIF_population (P: sig
    val free_vm: bool
    val n: int
    val dt: float
    val tau: float
    val threshold: float
  end): sig

  val step: (float * (int list array) * (unit -> int array)) list -> unit
  val spikes: unit -> int array
  val us: Mat.mat

end

(** Given a set of spikes x, builds a spike matrix that can easily be plotted.
  
    @param x [x.(t)] is an [int array] containing the indices of all the neurons
    that have spiked in time bin [t] *)
val plottable_raster: dt:float -> int array array -> Mat.mat

val population_rate: n:int -> dt:float -> int array array -> Mat.mat

val random_synapses: int -> int -> int -> int list array


