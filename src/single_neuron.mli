open Owl

(** Hodgkin-Huxley model *)
module Hodgkin_Huxley : sig
  (** type of voltage-dependent gate *)
  type gate =
    { alpha : float -> float
    ; beta : float -> float
    }

  val steady_state : gate -> float -> float

  type prms =
    { cm : float (** membrane capacitance (in F) *)
    ; vt : float (** intrinsic spiking threshold (in mV) *)
    ; e_leak : float (** leak reversal potential (in mV) *)
    ; g_leak_max : float (** peak leak conductance (in S) *)
    ; g_na_max : float (** peak conductance (in S) for sodium channels *)
    ; g_k_max : float (** peak conductance (in S) for potassium channels *)
    ; e_na : float (** reversal potential (in mV) for sodium channels *)
    ; e_k : float (** reversal potential (in mV) for potassium channels *)
    ; m_gate : gate (** rate constants for the sodium "m" gate *)
    ; h_gate : gate (** rate constants for the sodium "h" gate *)
    ; n_gate : gate (** rate constants for the potassium "n" gate *)
    }

  (** A good set of default parameters *)
  val default_prms : prms

  val simulate : prms:prms -> duration:float -> (float -> float) -> Mat.mat * Mat.mat
end

(** Leaky integrate-and-fire (LIF) model *)
module LIF : sig
  type prms =
    { tau : float (** membrane time constant *)
    ; v_rest : float (** resting potential *)
    ; v_thresh : float (** spiking threshold *)
    ; v_reset : float (** reset potential after spike *)
    ; dt : float (** integration time step *)
    }

  val default_prms : prms
  val simulate : prms:prms -> duration:float -> (float -> float) -> Mat.mat * Mat.mat
end
