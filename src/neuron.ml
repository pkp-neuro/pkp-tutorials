open Owl

module HH = struct
  type gate =
    { alpha : float -> float
    ; beta : float -> float
    }

  let steady_state gate vm =
    let a = gate.alpha vm
    and b = gate.beta vm in
    a /. (a +. b)


  let time_constant gate vm =
    let a = gate.alpha vm
    and b = gate.beta vm in
    1.0 /. (a +. b)


  type prms =
    { cm : float
    ; e_leak : float
    ; g_leak : float
    ; g_na_max : float
    ; g_k_max : float
    ; e_na : float
    ; e_k : float
    ; m_gate : gate
    ; h_gate : gate
    ; n_gate : gate
    }

  (* all in standard units (F, S, V) *)
  let default_prms =
    let cm = 100E-12 in
    let g_leak = 30E-9 in
    let g_na_max = 12E-6 in
    let g_k_max = 3.6E-6 in
    let e_leak = -60E-3 in
    let e_na = 45E-3 in
    let e_k = -82E-3 in
    let m_gate =
      let alpha vm =
        let dv = -.vm -. 45E-3 in
        1E5 *. dv /. (exp (100. *. dv) -. 1.)
      and beta vm =
        let dv = -.vm -. 70E-3 in
        4E3 *. exp (dv /. 0.018)
      in
      { alpha; beta }
    in
    let h_gate =
      let alpha vm = 70.0 *. exp (50. *. (-.vm -. 70E-3))
      and beta vm = 1E3 /. (1. +. exp (100. *. (-.vm -. 40E-3))) in
      { alpha; beta }
    in
    let n_gate =
      let alpha vm =
        let dv = -.vm -. 60E-3 in
        1E4 *. dv /. (exp (100. *. dv) -. 1.)
      and beta vm = 125. *. exp ((-.vm -. 70E-3) /. 80E-3) in
      { alpha; beta }
    in
    { cm; e_leak; g_leak; g_na_max; g_k_max; e_na; e_k; m_gate; n_gate; h_gate }


  let simulate ~prms ~duration input =
    let open Owl_ode in
    let dxdt x t =
      Gc.minor ();
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
        [| ((prms.g_leak *. (prms.e_leak -. vm))
           +. (prms.g_na_max *. m *. m *. m *. h *. (prms.e_na -. vm))
           +. (prms.g_k_max *. n *. n *. n *. n *. (prms.e_k -. vm))
           +. if t > 0. then input t else 0.)
           /. prms.cm
         ; (am *. (1. -. m)) -. (bm *. m)
         ; (ah *. (1. -. h)) -. (bh *. h)
         ; (an *. (1. -. n)) -. (bn *. n)
        |]
        1
        (-1)
    in
    (* start at -0.2s to make sure we reach steady state before t=0.0 *)
    let t_spec =
      let burn_in = 0.2 in
      let duration = duration +. burn_in in
      Owl_ode.Types.(T1 { t0 = -.burn_in; duration; dt = 1E-5 })
    in
    (* a sensible initial condition *)
    let x0 =
      let vm = -70E-3 in
      Mat.of_array
        [| vm
         ; steady_state prms.m_gate vm
         ; steady_state prms.h_gate vm
         ; steady_state prms.n_gate vm
        |]
        1
        (-1)
    in
    let solver = Owl_ode.Native.D.rk45 ~tol:1E-6 ~dtmax:1E-3 in
    let t, state = Ode.odeint solver dxdt x0 t_spec () in
    let ids = Mat.filter (fun t -> t >= 0.) t |> Array.to_list in
    let t = Mat.get_fancy [ L ids; R [] ] t in
    let state = Mat.get_fancy [ L ids; R [] ] state in
    let state = Mat.((1E3 $* col state 0) @|| get_slice [ []; [ 1; -1 ] ] state) in
    t, state
end

module LIF = struct
  type prms =
    { cm : float
    ; g_leak : float
    ; vm_rest : float
    ; vm_thresh : float
    ; vm_reset : float
    ; dt : float
    }

  let default_prms =
    let cm = 100E-12 in
    let g_leak = 30E-9 in
    let vm_rest = -70E-3 in
    let vm_thresh = -60E-3 in
    let vm_reset = -80E-3 in
    { cm; g_leak; vm_rest; vm_thresh; vm_reset; dt = 1E-4 }


  let simulate ~prms ~duration input =
    let n = int_of_float (duration /. prms.dt) in
    let t = Array.init n (fun t -> prms.dt *. float t) in
    let u = ref prms.vm_rest in
    let spike = ref false in
    let us =
      Array.map
        (fun t ->
          if !spike
          then (
            spike := false;
            u := prms.vm_reset);
          let total_current = (prms.g_leak *. (prms.vm_rest -. !u)) +. input t in
          u := !u +. (prms.dt /. prms.cm *. total_current);
          if !u > prms.vm_thresh
          then (
            spike := true;
            u := 0.);
          1E3 *. !u)
        t
    in
    Mat.of_array t (-1) 1, Mat.of_array us (-1) 1
end

let count_spikes ~after (t, vm) =
  if Mat.col_num t > 1 || Mat.row_num t <> Mat.row_num vm
  then
    failwith
      "[count_spikes ~after (t, vm)]: t must be a column vector, with the same number \
       of rows as vm";
  let n = Mat.row_num t in
  let rec count i c state =
    if i = n
    then c
    else if Mat.get t i 0 < after
    then count (i + 1) c state
    else (
      let new_state = Mat.get vm i 0 > -20.0 in
      let c = if (not state) && new_state then c + 1 else c in
      count (i + 1) c new_state)
  in
  count 0 0 false
