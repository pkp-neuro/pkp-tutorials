open Owl

module type Neuron = sig
  val simulate : duration:float -> (float -> float) -> Mat.mat * Mat.mat
  (** [simulate ~duration input] simulates the model for a specified [duration]
      and input current (in A) as a function of time (in s) *)
end

(** {1 Hodgkin-Huxley model} *)

(** type of voltage-dependent gate *)
type gate = {alpha: float -> float; beta: float -> float}

let steady_state gate vm =
  let a = gate.alpha vm and b = gate.beta vm in
  a /. (a +. b)

module type HH_prms = sig
  val cm : float
  (** membrane capacitance (in F) *)

  val vt : float
  (** ~intrinsic spiking threshold *)

  val e_leak : float
  (** leak reversal potential (in V) *)

  val g_leak_max : float
  (** peak leak conductance (in S) *)

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

(** A good set of default parameters *)
module HH_default_prms : HH_prms = struct
  let cm = 1E-6
  let vt = -60.
  let e_leak = -70.0
  let g_leak_max = 1E-4
  let g_na_max = 50E-3
  let g_k_max = 5E-3
  let e_na = 50.0
  let e_k = -90.0

  let m_gate =
    let alpha vm =
      let dv = vm -. vt -. 13. in
      -320.0 *. dv /. (exp (-.dv /. 4.) -. 1.)
    and beta vm =
      let dv = vm -. vt -. 40. in
      280. *. dv /. (exp (dv /. 5.) -. 1.)
    in
    {alpha; beta}

  let h_gate =
    let alpha vm = 128.0 *. exp (-.(vm -. vt -. 17.) /. 18.)
    and beta vm = 4000. /. (1. +. exp (-.(vm -. vt -. 40.) /. 5.)) in
    {alpha; beta}

  let n_gate =
    let alpha vm =
      let dv = vm -. vt -. 15. in
      -32.0 *. dv /. (exp (-.dv /. 5.) -. 1.)
    and beta vm = 500. *. exp (-.(vm -. vt -. 10.) /. 40.) in
    {alpha; beta}
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
           /. cm
         ; (am *. (1. -. m)) -. (bm *. m)
         ; (ah *. (1. -. h)) -. (bh *. h)
         ; (an *. (1. -. n)) -. (bn *. n) |]
        1 4
    in
    let t_spec = Owl_ode.Types.(T1 {t0= 0.; duration; dt= 1E-5}) in
    let x0 =
      let vm = -70. in
      Mat.of_array
        [| vm; steady_state m_gate vm; steady_state h_gate vm
         ; steady_state n_gate vm |]
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
  let v_rest = -70.0
  let v_thresh = -60.0
  let v_reset = -70.0
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
            spike := false ;
            u := v_reset ) ;
          u := !u +. (dt /. tau *. (v_rest -. !u +. input t)) ;
          if !u > v_thresh then (
            spike := true ;
            u := 0. ) ;
          !u )
        t
    in
    (Mat.of_array t (-1) 1, Mat.of_array us (-1) 1)
end
