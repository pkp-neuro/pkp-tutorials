open Owl

let n = 200
let tau = 20E-3
let angles = Mat.init 1 n (fun i -> (Const.pi *. float i /. float n) -. (Const.pi /. 2.))

type radians = float

let rad_of_deg x = Const.pi *. x /. 180.

let input_from_LGN contrast angle =
  let epsilon = 0.1 in
  Mat.(1. $+ (contrast $* (1. -. epsilon $+ (epsilon $* cos (2. $* angles -$ angle)))))


let firing_rate_function x = max 0. (min 1. (x -. 1.))
let normalise m = Mat.(m /$ max' m)

type prms =
  { a : float
  ; b : float
  }

let feedforward_prms = { a = 0.0; b = 0. }
let recurrent_prms = { a = 7.3; b = 11. }

let simulate ~duration prms input =
  let w =
    Mat.init_2d n n (fun i j ->
        let ai = Mat.get angles 0 i in
        let aj = Mat.get angles 0 j in
        -.prms.a +. (prms.b *. cos (2. *. (ai -. aj))))
  in
  let w = Mat.(w /$ float n) in
  let drdt r t =
    let h = Mat.((r *@ w) + input t) in
    Mat.((neg r + Mat.map firing_rate_function h) /$ tau)
  in
  let open Owl_ode in
  let t_spec = Types.(T1 { t0 = 0.; duration; dt = 1E-4 }) in
  (* a sensible initial condition *)
  let r0 = Mat.zeros 1 n in
  let solver = Native.D.rk45 ~tol:1E-3 ~dtmax:1E-3 in
  let t, state = Ode.odeint solver drdt r0 t_spec () in
  t, state


let angular_axis =
  let open Gp in
  [ barebone
  ; borders [ `bottom; `left ]
  ; xlabel "neurons (pref. ori.)"
  ; xtics (`manual [ -.Const.pi /. 2., "-90"; 0.0, "0"; Const.pi /. 2., "+90" ])
  ]


let plot_dynamics ?(movie_duration = 2.) (time, rate) =
  let open Gp in
  let figure tit x (module P : Plot) =
    P.plot
      (L [ angles; x ])
      ~style:"l lc 8 lw 2"
      (angular_axis
      @ [ ylabel "activity"; title tit; yrange (-0.1, 1.1); ytics (`regular [ 0.; 0.5 ]) ]
      )
  in
  let display_id = Jupyter_notebook.display "text/html" "" in
  let resolution = 0.02 in
  Mat.fold_rows
    (fun (i, tprev) r ->
      let t = Mat.get time i 0 in
      if t < tprev +. resolution
      then i + 1, tprev
      else (
        Unix.sleepf
          (movie_duration *. resolution /. Mat.get time (pred Mat.(row_num time)) 0);
        let tit = Printf.sprintf "time: %.1f s" t in
        Juplot.draw ~size:(300, 200) ~display_id (figure tit r);
        i + 1, tprev +. resolution))
    (0, -.resolution)
    rate
  |> ignore


let plot_steady_states ss =
  assert (ss <> []);
  let open Gp in
  let fig (module P : Plot) =
    P.plots
      (List.mapi
         (fun i (_, state) ->
           item
             (L [ angles; Mat.row state (-1) ])
             ~style:Printf.(sprintf "l lc %i lw 2" i))
         ss)
      (angular_axis
      @ [ ylabel "activity"; yrange (-0.1, 1.1); ytics (`regular [ 0.; 0.5 ]) ])
  in
  Juplot.draw ~size:(300, 200) fig
