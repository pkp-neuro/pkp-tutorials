open Owl

(**

  This module provides key functions for simulating the classical «balanced ⚖
  network». You can create your neural populations using e.g. the {!lif} or
  {!poisson} funtions, connect them using {!random_connections}, run a network
  simulation using {!simulate}, and retrieve spikes / voltages for plotting /
  further analysis.

  For example:
  {[
    let input = Array.init 1000 (fun _ -> poisson 10.)
    let output = Array.init 1 (fun _ -> lif ())

    let connections = [ all_to_all_connections ~from:input ~onto:output ~w:0.01 ]

    let net = { neurons = [ input; output ]; connections }
    let duration = 1.0 (* in seconds *)
    let _ = simulate ~duration net

    (* simplest possible analysis: what's the output firing rate? *)
    let output_rate =
      output.(0)
      |> spikes
      |> List.length
      |> fun x -> float x /. duration
  ]}

  @see <https://www.jstor.org/stable/pdf/2890956.pdf> for the mathematical details
    in the original Science paper (1996).

*)

(** {1 Neurons} *)

type neuron

(** Creates a leaky integrate-and-fire (LIF) neuron; note that
    the voltage units have been rescaled so that the resting potential is 0 and the
    spike threshold is 1.
    @param tau membrane time constant (default: 20E-3 s)
    @param tau_refr absolute refractory time constant (default: 5E-3 s)
    @param threshold firing threshold (default: 1.0)
    @param init_voltage initial value of the membrane potential at t=0 (default: 0.0)
    @param axon_delay (default: uniformly drawn at random between 0 and 2E-3 s)
    @param log_voltage whether the membrane potential should be loged (e.g. for plotting later; default: [true]) *)
val lif
  :  ?tau:float
  -> ?tau_refr:float
  -> ?threshold:float
  -> ?resting_potential:float
  -> ?init_voltage:float
  -> ?axon_delay:float
  -> ?log_voltage:bool
  -> unit
  -> neuron

(** [poisson rate] creates a "Poisson neuron", i.e. a fake neuron that simply
    emits action potentials like a Poisson process with rate [rate] (in Hz) *)
val poisson : float -> neuron

(** Retrieve all the spike times of a neuron after simulation *)
val spikes : neuron -> float list

(** Retrieve the voltage trace of a neuron after simulation;
    will fail for a Poisson neuron which has no voltage *)
val voltage_trace : ?dt:float -> duration:float -> neuron -> Mat.mat

(** {1 Connections} *)

type connections

(** [all_to_all_connections from onto k w] creates a set of connections
    such that any neuron in the [from] population connects to all neurons in
    the [onto] population; all synaptic weights are set to [w] *)
val all_to_all_connections
  :  from:neuron array
  -> onto:neuron array
  -> w:float
  -> connections

(** [random_connections from onto k w] creates a set of random connections
    such that any neuron in the [from] population connects to exactly [k]
    neurons randomly chosen (without replacement) from the [onto] population;
    all synaptic weights are set to [w] *)
val random_connections
  :  from:neuron array
  -> onto:neuron array
  -> k:int
  -> w:float
  -> connections

(** {1 Event-based simulation of network dynamics} *)

type network =
  { neurons : neuron array list
  ; connections : connections list
  }

(** [simulate net duration] simulates the dynamics of [net] for [duration] seconds;
    optional parameter [display] (default: [false]) can be set to true to
    display simulation progress. *)
val simulate : ?display:bool -> duration:float -> network -> unit

(** {1 Dealing with spike trains} *)

(** Converts an array of spike list (each list corresponding to a particular neuron)
    into a matrix with two columns: (spike time, neuron index).
    Such a matrix can then easily be plotted as dots -- this is what neuroscientists
    call a "spike raster" *)
val raster : float list array -> Mat.mat

(** Constructs a readily-plottable [Gp.item] voltage trace for a given neuron. *)
val plottable_voltage : ?style:string -> duration:float -> neuron -> Gp.item

(** Constructs a readily-plottable [Gp.item] spike train for a given neuron.
    Spikes are displayed as bars going from 0 to some tunable height. *)
val plottable_spikes : ?bar_height:float -> ?style:string -> neuron -> Gp.item

(** Discard the first [first] seconds of a spike train *)
val discard : first:float -> float list -> float list

(** Estimates the Fano factor from a long spike train; spikes are counted
    in a window of duration [window] seconds. *)
val fano_factor : window:float -> float list -> float
