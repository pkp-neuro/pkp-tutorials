open Owl

(** Hodgkin-Huxley model *)
module HH : sig
  (** type of voltage-dependent gate *)
  type gate =
    { alpha : float -> float (** opening rate (1/s) *)
    ; beta : float -> float (** closing rate (1/s)*)
    }

  (** Steady state activation of a gate at a given Vm *)
  val steady_state : gate -> float -> float

  (** Time constant at which relaxation to steady-state occurs, for a given Vm *)
  val time_constant : gate -> float -> float

  (** Parameters for the A-current of the Connor-Stevens model *)
  type a_current_prms =
    { g_a_max : float
    ; e_a : float
    ; a_gate : gate
    ; b_gate : gate
    }

  type prms =
    { cm : float (** membrane capacitance (in F) *)
    ; e_leak : float (** leak reversal potential (in V) *)
    ; g_leak : float (** leak conductance (in S) *)
    ; g_na_max : float (** peak conductance (in S) for sodium channels *)
    ; g_k_max : float (** peak conductance (in S) for potassium channels *)
    ; e_na : float (** reversal potential (in V) for sodium channels *)
    ; e_k : float (** reversal potential (in V) for potassium channels *)
    ; m_gate : gate (** rate constants for the sodium "m" gate *)
    ; h_gate : gate (** rate constants for the sodium "h" gate *)
    ; n_gate : gate (** rate constants for the potassium "n" gate *)
    ; a_current : a_current_prms option (** optional A-current parameters *)
    }

  (** A good set of default parameters ─ those found in the original HH paper *)
  val default_prms : prms

  (** A better model for cortical cells: the Connor-Stevens model *)
  val connor_stevens : prms

  (** Simulates the model for a certain duration, with a certain input current
      (provided as a function of time in seconds).
      @return [(time, state)] where [time] is a column vector of times
      and [state] is a matrix containing the four state variables of the
      model at those same times (thus [time] and [state] have the same number of rows).
      The first column is the membrane potential (in mV), the other columns are
      the gate variables [m, h, n] in this order, optionally followed by the [a] and [b]
      gates in the case of the Connor-Stevens model. *)
  val simulate : prms:prms -> duration:float -> (float -> float) -> Mat.mat * Mat.mat
end

(** Leaky integrate-and-fire (LIF) model *)
module LIF : sig
  type prms =
    { cm : float (** membrane capacitance (in F) *)
    ; g_leak : float (** leak conductance (in S) *)
    ; vm_rest : float (** resting potential (in V) *)
    ; vm_thresh : float (** spiking threshold (in V) *)
    ; vm_reset : float (** reset potential after spike (in V) *)
    ; dt : float (** integration time step *)
    }

  val default_prms : prms
  val simulate : prms:prms -> duration:float -> (float -> float) -> Mat.mat * Mat.mat
end

(** [count_spikes ~after:2.0 (t, vm)] counts then number of action potentials that
    occur in the membrane potential time series (t, vm) after 2 seconds and
    until the end of the time series. [t] must be a column vector, with the same
    number of rows as [vm]; if [vm] has multiple columns, only the first one is considered. *)
val count_spikes : after:float -> Mat.mat * Mat.mat -> int

(** [spikes ~after:2.0 (t, vm)] returns a list of all spike times that occurred
    after 2 seconds, given a membrane potential time series (t, vm).
    [t] must be a column vector, with the same number of rows as [vm]; if [vm]
    has multiple columns, only the first one is considered. *)
val spikes : after:float -> Mat.mat * Mat.mat -> float list
