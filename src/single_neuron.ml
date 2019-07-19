open Owl

module Hodgkin_Huxley = struct
  type gate =
    { alpha : float -> float
    ; beta : float -> float
    }

  let steady_state gate vm =
    let a = gate.alpha vm
    and b = gate.beta vm in
    a /. (a +. b)


  type prms =
    { cm : float
    ; vt : float
    ; e_leak : float
    ; g_leak_max : float
    ; g_na_max : float
    ; g_k_max : float
    ; e_na : float
    ; e_k : float
    ; m_gate : gate
    ; h_gate : gate
    ; n_gate : gate
    }

  let default_prms =
    let cm = 1E-6 in
    let vt = -60. in
    let e_leak = -70.0 in
    let g_leak_max = 1E-4 in
    let g_na_max = 50E-3 in
    let g_k_max = 5E-3 in
    let e_na = 50.0 in
    let e_k = -90.0 in
    let m_gate =
      let alpha vm =
        let dv = vm -. vt -. 13. in
        -320.0 *. dv /. (exp (-.dv /. 4.) -. 1.)
      and beta vm =
        let dv = vm -. vt -. 40. in
        280. *. dv /. (exp (dv /. 5.) -. 1.)
      in
      { alpha; beta }
    in
    let h_gate =
      let alpha vm = 128.0 *. exp (-.(vm -. vt -. 17.) /. 18.)
      and beta vm = 4000. /. (1. +. exp (-.(vm -. vt -. 40.) /. 5.)) in
      { alpha; beta }
    in
    let n_gate =
      let alpha vm =
        let dv = vm -. vt -. 15. in
        -32.0 *. dv /. (exp (-.dv /. 5.) -. 1.)
      and beta vm = 500. *. exp (-.(vm -. vt -. 10.) /. 40.) in
      { alpha; beta }
    in
    { cm; vt; e_leak; g_leak_max; g_na_max; g_k_max; e_na; e_k; m_gate; n_gate; h_gate }


  let simulate ~prms ~duration input =
    let open Owl_ode in
    let dxdt x t =
      let vm = Mat.get x 0 0 in
      let m = Mat.get x 0 1 in
      let h = Mat.get x 0 2 in
      let n = Mat.get x 0 3 in
      let am = prms.m_gate.alpha vm in
      let ah = prms.h_gate.alpha vm in
      let an = prms.n_gate.alpha vm in
      let bm = prms.m_gate.beta vm in
      let bh = prms.h_gate.beta vm in
      let bn = prms.n_gate.beta vm in
      Mat.of_array
        [| ((prms.g_leak_max *. (prms.e_leak -. vm))
           +. (prms.g_na_max *. m *. m *. m *. h *. (prms.e_na -. vm))
           +. (prms.g_k_max *. n *. n *. n *. n *. (prms.e_k -. vm))
           +. input t)
           /. prms.cm
         ; (am *. (1. -. m)) -. (bm *. m)
         ; (ah *. (1. -. h)) -. (bh *. h)
         ; (an *. (1. -. n)) -. (bn *. n)
        |]
        1
        (-1)
    in
    let t_spec = Owl_ode.Types.(T1 { t0 = 0.; duration; dt = 1E-5 }) in
    let x0 =
      let vm = -70. in
      Mat.of_array
        [| vm
         ; steady_state prms.m_gate vm
         ; steady_state prms.h_gate vm
         ; steady_state prms.n_gate vm
        |]
        1
        (-1)
    in
    Ode.odeint (module Owl_ode_sundials.Owl_Cvode) dxdt x0 t_spec ()
end

module LIF = struct
  type prms =
    { tau : float (** membrane time constant *)
    ; v_rest : float (** resting potential *)
    ; v_thresh : float (** spiking threshold *)
    ; v_reset : float (** reset potential after spike *)
    ; dt : float (** integration time step *)
    }

  let default_prms =
    { tau = 20E-3; v_rest = -70.0; v_thresh = -60.0; v_reset = -70.0; dt = 1E-4 }


  let simulate ~prms ~duration input =
    let n = int_of_float (duration /. prms.dt) in
    let t = Array.init n (fun t -> prms.dt *. float t) in
    let u = ref prms.v_rest in
    let spike = ref false in
    let us =
      Array.map
        (fun t ->
          if !spike
          then (
            spike := false;
            u := prms.v_reset);
          u := !u +. (prms.dt /. prms.tau *. (prms.v_rest -. !u +. input t));
          if !u > prms.v_thresh
          then (
            spike := true;
            u := 0.);
          !u)
        t
    in
    Mat.of_array t (-1) 1, Mat.of_array us (-1) 1
end
