open Owl

module type Neuron = sig
  val simulate : duration:float -> (float -> float) -> Mat.mat * Mat.mat
  (** [simulate ~duration input] simulates the model for a specified [duration]
      and input current (in A) as a function of time (in s) *)
end

(** {1 Hodgkin-Huxley model} *)

(** type of voltage-dependent gate *)
type gate = { alpha : float -> float; beta : float -> float }

let steady_state gate vm =
  let a = gate.alpha vm and b = gate.beta vm in
  a /. (a +. b)

module type HH_prms = sig
  val cm : float
  (** membrane capacitance (in F) *)

  val g_leak_max : float
  (** peak leak conductance (in S) *)

  val e_leak : float
  (** leak reversal potential (in V) *)

  val g_na_max : float
  (** peak conductance (in S) for sodium channels *)

  val g_k_max : float
  (** peak conductance (in S) for potassium channels *)

  val e_na : float
  (** reversal potential (in V) for sodium channels *)

  val e_k : float
  (** reversal potential (in V) for potassium channels *)

  val m_gate : gate
  (** rate constants for the sodium "m" gate *)

  val h_gate : gate
  (** rate constants for the sodium "h" gate *)

  val n_gate : gate
  (** rate constants for the potassium "n" gate *)
end

module HH_default_prms : HH_prms = struct
  let cm = 100E-12

  let g_leak_max = 30E-9

  let e_leak = -60E-3

  let g_na_max = 12E-6

  let g_k_max = 3.6E-6

  let e_na = 45E-3

  let e_k = -82E-3

  let m_gate =
    let alpha vm =
      let dvm = -.vm -. 0.045 in
      1E5 *. dvm /. (exp (100. *. dvm) -. 1.)
    in
    let beta vm = 4E3 *. exp ((-.vm -. 0.070) /. 0.018) in
    { alpha; beta }

  let h_gate =
    let alpha vm = 70. *. exp (50. *. (-.vm -. 0.070)) in
    let beta vm = 1E3 /. (1. +. exp (100. *. (-.vm -. 0.040))) in
    { alpha; beta }

  let n_gate =
    let alpha vm =
      let dvm = -.vm -. 0.060 in
      1E4 *. dvm /. (exp (100. *. dvm) -. 1.)
    in
    let beta vm = 125. *. exp ((-.vm -. 0.070) /. 0.08) in
    { alpha; beta }
end

module HH (P : HH_prms) : Neuron = struct
  open P

  let simulate ~duration input =
    let open Owl_ode in
    let dxdt x t =
      let vm = Mat.get x 0 0 in
      let m = Mat.get x 0 1 in
      let h = Mat.get x 0 2 in
      let n = Mat.get x 0 3 in
      let am = m_gate.alpha vm in
      let ah = h_gate.alpha vm in
      let an = n_gate.alpha vm in
      let bm = m_gate.beta vm in
      let bh = h_gate.beta vm in
      let bn = n_gate.beta vm in
      Mat.of_array
        [| ( (g_leak_max *. (e_leak -. vm))
           +. (g_na_max *. m *. m *. m *. h *. (e_na -. vm))
           +. (g_k_max *. n *. n *. n *. n *. (e_k -. vm))
           +. input t )
           /. cm;
           (am *. (1. -. m)) -. (bm *. m);
           (ah *. (1. -. h)) -. (bh *. h);
           (an *. (1. -. n)) -. (bn *. n)
        |]
        1 4
    in
    let t_spec = Owl_ode.Types.(T1 { t0 = 0.; duration; dt = 1E-5 }) in
    let x0 =
      let vm = -70E-3 in
      Mat.of_array
        [| vm;
           steady_state m_gate vm;
           steady_state h_gate vm;
           steady_state n_gate vm
        |]
        1 4
    in
    Ode.odeint (module Owl_ode_sundials.Owl_Cvode) dxdt x0 t_spec ()
end

(** {1 Leaky Integrate-and-Fire model} *)

module type LIF_prms = sig
  val tau : float

  val v_rest : float

  val v_thresh : float

  val v_reset : float

  val dt : float
end

module LIF_default_prms : LIF_prms = struct
  let tau = 20E-3

  let v_rest = -70E-3

  let v_thresh = -60E-3

  let v_reset = -70E-3

  let dt = 1E-4
end

module LIF (P : LIF_prms) : Neuron = struct
  open P

  let simulate ~duration input =
    let n = int_of_float (duration /. dt) in
    let t = Array.init n (fun t -> dt *. float t) in
    let u = ref v_rest in
    let spike = ref false in
    let us =
      Array.map
        (fun t ->
          if !spike then (
            spike := false;
            u := v_reset );
          u := !u +. (dt /. tau *. (v_rest -. !u +. input t));
          if !u > v_thresh then (
            spike := true;
            u := 0. );
          !u )
        t
    in
    (Mat.of_array t (-1) 1, Mat.of_array us (-1) 1)
end
