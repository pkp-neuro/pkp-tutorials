open Owl

let insert_in_ordered_list ol x =
  let x_time, _ = x in
  let rec process left right =
    match right with
    | ((hd_time, _) as hd) :: tl ->
      if x_time < hd_time
      then List.rev_append left (x :: right)
      else process (hd :: left) tl
    | [] -> List.rev (x :: left)
  in
  process [] ol


let next_update_time ~t_max t =
  let t = t +. Owl_stats.exponential_rvs ~lambda:1. in
  if t > t_max then None else Some t


type pattern = Mat.mat

let random_pattern n =
  let x = Mat.zeros 1 n in
  Stats.choose (Array.init n (fun i -> i)) (n / 2)
  |> Array.iter (fun k -> Mat.set x 0 k 1.);
  x


let are_equal x y = Mat.(abs (x - y) <$ 0.01)

let corrupt ~fraction x =
  if fraction < 0. || fraction > 1. then failwith "fraction must be between 0 and 1";
  let x = Mat.copy x in
  let n = Mat.numel x in
  let n_flip = int_of_float (fraction *. float n) in
  let ids = Stats.choose (Array.init n (fun i -> i)) n_flip in
  Array.iter (fun i -> Mat.set x 0 i (1. -. Mat.get x 0 i)) ids;
  x


type connections = Mat.mat

let connectivity patterns =
  let p = Mat.concatenate ~axis:0 patterns in
  let p = Mat.((2. $* p) -$ 1.0) in
  let w = Mat.(transpose p *@ p) in
  (* zero-out the diagonal *)
  Mat.(w - diagm (diag w))


let plot_pattern ?display_id x =
  let open Gp in
  let display_id =
    match display_id with
    | None -> Jupyter_notebook.display "text/html" ""
    | Some id -> id
  in
  let figure (module P : Plot) =
    P.plot
      (A x)
      ~using:"0:(1):1"
      ~style:"boxes fs solid 1.0 noborder lc palette"
      [ barebone
      ; margins [ `left 0.1; `right 0.9; `top 0.9; `bottom 0.57 ]
      ; yrange (0., 1.1)
      ; xlabel "neurons"
      ; set "palette model RGB define (0 'white', 1 'black')"
      ; cbrange (0.0, 1.0)
      ]
  in
  Juplot.draw ~display_id ~size:(300, 75) figure


let compare_patterns ?display_id (x1, label1) (x2, label2) =
  let open Gp in
  let display_id =
    match display_id with
    | None -> Jupyter_notebook.display "text/html" ""
    | Some id -> id
  in
  let figure (module P : Plot) =
    P.plot
      (A x1)
      ~using:"0:(1):1"
      ~style:"boxes fs solid 1.0 noborder lc palette"
      [ barebone
      ; margins [ `left 0.1; `right 0.9; `top 0.9; `bottom 0.57 ]
      ; yrange (0., 1.1)
      ; ylabel label1
      ; set "palette model RGB define (0 'white', 1 'black')"
      ; cbrange (0.0, 1.0)
      ];
    P.plot
      (A x2)
      ~using:"0:(1):1"
      ~style:"boxes fs solid 1.0 noborder lc palette"
      [ barebone
      ; margins [ `left 0.1; `right 0.9; `top 0.53; `bottom 0.2 ]
      ; yrange (0., 1.1)
      ; ylabel label2
      ; xlabel "neurons"
      ; set "palette model RGB define (0 'white', 1 'black')"
      ; cbrange (0.0, 1.0)
      ]
  in
  Juplot.draw ~display_id ~size:(600, 150) figure


let recall ?compare_with ~t_max w cue =
  let m, n = Mat.shape cue in
  assert (m = 1);
  let display_info =
    match compare_with with
    | None -> None
    | Some x_true -> Some (Jupyter_notebook.display "text/html" "", x_true)
  in
  let rec update x queue =
    match queue with
    | (t, i) :: rest ->
      let wi = Mat.row w i in
      let hi = Mat.(sum' (wi * x)) in
      let new_xi = if hi > 0. then 1. else 0. in
      let has_changed = abs_float (new_xi -. Mat.get x 0 i) > 0.01 in
      if has_changed
      then (
        Mat.set x 0 i (if hi > 0. then 1. else 0.);
        match display_info with
        | Some (display_id, x_true) ->
          compare_patterns ~display_id (x, "recalled") (x_true, "true")
        | None -> ());
      let queue =
        match next_update_time ~t_max t with
        | Some t -> insert_in_ordered_list rest (t, i)
        | None -> rest
      in
      update x queue
    | [] -> x
  in
  let queue =
    List.init n (fun i -> Owl_stats.exponential_rvs ~lambda:1., i) |> List.sort compare
  in
  update Mat.(copy cue) queue
