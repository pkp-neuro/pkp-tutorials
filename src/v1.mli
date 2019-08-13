open Owl

(** {1 Neurons on a “ring”} *)

(** Neurons are "positioned" around a ring of orientation preference;
    [angles] is a vector whose ith element contains the preferred orientation
    of the ith neuron. *)
val angles : Mat.mat

type radians = float

(** Converts degrees into radians. *)
val rad_of_deg : float -> radians

(** {1 LGN input} *)

(** [input_from_LGN ~epsilon contrast angle] models LGN input as a vector
    of size [n], when an oriented stimulus ([angle]) is presented at some
    [contrast] (a number typically between 0 and 1). Try it, plot it! *)
val input_from_LGN : float -> radians -> Mat.mat

(** {1 V1 model} *)

(** Describes the input-to-rate transformation (at steady state) in each V1 neuron.
    In this model, the maximum firing rate is "1" by convention ─ biologically, "1" would
    actually mean ~80 Hz or so. *)
val firing_rate_function : float -> float

(** Network parameters;
    [a] and [b] describe recurrent interactions, such that
    the connection from neuron [j] to neuron [i] has strength
    [-a + b cos(2*(angle(j) - angle(i)))]. *)
type prms =
  { a : float
  ; b : float
  }

(** Parameters of the feedforward model ([a=0, b=0], ie. no recurrent connectivity!). *)
val feedforward_prms : prms

(** Default parameters of the recurrent model. *)
val recurrent_prms : prms

(** [simulate ~duration prms input] simulates the network dynamics for
    [duration] seconds. Here, [input] is a function of time, which for any time t
    returns the LGN input to the network. Returns [(time, rates)], where [time]
    is a column vector of [T] time points, and [rates] is a matrix of size
    [T x n]. *)
val simulate : duration:float -> prms -> (float -> Mat.mat) -> Mat.mat * Mat.mat

(** {1 Plotting functions} *)

(** Set of plot properties you can use whenever you want to plot
    something as a function of angle. *)
val angular_axis : Gp.property list

(** [plot_dynamics ?movie_duration (time, rate)] shows a movie of network
    activity as it unfolds over time, for a pair [(time, rate)]
    obtained from the [simulate] function. *)
val plot_dynamics : ?movie_duration:float -> Mat.mat * Mat.mat -> unit

(** [plot_steady_state list] plots multiple firing rate responses in the same
    graph. The argument is a list of [(time, rate)] pairs (e.g. each returned by
    the [simulate] function), and for each, only the final (steady-state)
    response is shown on the graph (instead of a movie). *)
val plot_steady_states : (Mat.mat * Mat.mat) list -> unit

(** {1 Miscellaneous} *)

(** Rescales a vector of firing rates so that the maximum value becomes one. *)
val normalise : Mat.mat -> Mat.mat

