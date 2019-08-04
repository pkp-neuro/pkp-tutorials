open Owl
open Misc

(* simple ordered queue module for logging event times *)
module Queue = struct
  let empty = []

  let add queue x =
    let x_time, _ = x in
    let rec process left right =
      match right with
      | ((hd_time, _) as hd) :: tl ->
        if x_time < hd_time
        then List.rev_append left (x :: right)
        else process (hd :: left) tl
      | [] -> List.rev (x :: left)
    in
    process [] queue
end

module LIF = struct
  type 'a t =
    { tau_refr : float
    ; tau : float
    ; threshold : float
    ; axon_delay : float
    ; mutable spikes : float list
    ; mutable last_known_voltage : float * float
    ; mutable voltages : (float * float) list option
    ; mutable targets : (float * 'a) list
    }
end

module Poisson = struct
  type 'a t =
    { lambda : float
    ; mutable spikes : float list
    ; mutable targets : (float * 'a) list
    }
end

type neuron =
  [ `lif of neuron LIF.t
  | `poisson of neuron Poisson.t
  ]

let to_lif = function
  | `lif x -> `lif x
  | `poisson _ -> assert false


let reset = function
  | `lif x ->
    let open LIF in
    x.targets <- [];
    x.spikes <- [];
    x.last_known_voltage <- 0., 0.;
    x.voltages
      <- (match x.voltages with
         | Some _ -> Some []
         | None -> None)
  | `poisson x ->
    let open Poisson in
    x.targets <- [];
    x.spikes <- []


let spikes = function
  | `lif x -> x.LIF.spikes
  | `poisson x -> x.Poisson.spikes


let add_to_targets pre c =
  match pre with
  | `lif x -> x.LIF.targets <- c @ x.LIF.targets
  | `poisson x -> x.Poisson.targets <- c @ x.Poisson.targets


let lif
    ?(tau = 20E-3)
    ?(tau_refr = 5E-3)
    ?(threshold = 1.0)
    ?axon_delay
    ?(log_voltage = true)
    ()
  =
  let open LIF in
  let axon_delay =
    match axon_delay with
    | Some x -> x
    | None -> Owl_stats.uniform_rvs ~a:0. ~b:2E-3
  in
  `lif
    { tau
    ; tau_refr
    ; threshold
    ; axon_delay
    ; spikes = []
    ; last_known_voltage = 0., 0.
    ; voltages = (if log_voltage then Some [] else None)
    ; targets = []
    }


let log_voltage x v =
  x.LIF.last_known_voltage <- v;
  match x.LIF.voltages with
  | None -> ()
  | Some vs -> x.LIF.voltages <- Some (v :: vs)


let voltage_trace ?(dt = 1E-3) ~duration = function
  | `poisson _ -> failwith "a Poisson neuron has no voltage"
  | `lif x ->
    let open LIF in
    let rec consume events vs t v =
      if t >= duration
      then vs |> List.rev |> Array.of_list |> fun m -> Mat.of_array m (-1) 1
      else (
        match events with
        | [] ->
          let v = v *. exp (-.dt /. x.tau) in
          consume events (v :: vs) (t +. dt) v
        | (t_ev, _) :: _ when t < t_ev ->
          (* we are still waiting for the next jump *)
          let v = v *. exp (-.dt /. x.tau) in
          consume events (v :: vs) (t +. dt) v
        | (t_ev, v_ev) :: rest ->
          (* we have just moved past an event *)
          let v = v_ev *. exp (-.(t -. t_ev) /. x.tau) in
          (* we still need to look ahead to check that if we
             step into t +. dt, we are not going to leave the next event behind,
             in which case we would then miss it *)
          (match rest with
          | [] -> consume rest (v :: vs) (t +. dt) v
          | (t_ev', _) :: _ when t_ev' > t +. dt -> consume rest (v :: vs) (t +. dt) v
          | _ -> consume rest vs t v))
    in
    (match x.LIF.voltages with
    | None -> failwith "no voltage was recorded for this neuron"
    | Some vs ->
      let events = List.sort (fun (t1, _) (t2, _) -> compare t1 t2) vs in
      consume events [] 0. 0.)


let process_input (`lif x) (t, w) queue =
  let open LIF in
  match x.spikes with
  | last :: _ when t < last +. x.tau_refr ->
    (* do nothing if still within refractory period *) queue
  | _ ->
    let t_prev, u_prev = x.last_known_voltage in
    let u = (u_prev *. exp ((t_prev -. t) /. x.tau)) +. w in
    if u < x.threshold
    then (
      log_voltage x (t, u);
      queue)
    else (
      (* spike! *)
      log_voltage x (t, 0.);
      x.spikes <- t :: x.spikes;
      Queue.add queue (t +. x.axon_delay, `lif x))


let poisson rate = `poisson Poisson.{ lambda = rate; spikes = []; targets = [] }
let exp_rv lambda = Owl_stats.exponential_rvs ~lambda

let kickoff queue x =
  match x with
  | `poisson n ->
    let open Poisson in
    let first_spike = exp_rv n.lambda in
    Queue.add queue (first_spike, x)
  | _ -> queue


let poisson_renew (`poisson x) t queue =
  let open Poisson in
  (* log the previous spike *)
  x.spikes <- t :: x.spikes;
  Queue.add queue (t +. exp_rv x.lambda, `poisson x)


type connections = neuron * (float * neuron) list

let all_to_all_connections ~from ~onto ~w =
  Array.map
    (fun pre ->
      ( pre
      , Array.map
          (function
            | `poisson _ -> failwith "can't connect onto a Poisson neuron"
            | `lif post -> w, `lif post)
          onto
        |> Array.to_list ))
    from


let random_connections ~from ~onto ~k ~w =
  Array.map
    (fun pre ->
      (* draw k targets in post, at random, without replacement *)
      ( pre
      , Stats.choose onto k
        |> Array.map (function
               | `poisson _ -> failwith "can't connect onto a Poisson neuron"
               | `lif post -> w, `lif post)
        |> Array.to_list ))
    from


type network =
  { neurons : neuron array list
  ; connections : connections array list
  }

let simulate ?(display = false) ~duration net =
  let ph = if display then Some (placeholder ()) else None in
  let last_t_info = ref 0. in
  (* make sure to reset all neurons first *)
  List.iter (Array.iter reset) net.neurons;
  (* set the connections *)
  List.iter (Array.iter (fun (pre, c) -> add_to_targets pre c)) net.connections;
  (* main iterator -- just go recursively through the event queue *)
  let rec iter queue =
    match queue with
    | [] ->
      (* we are only starting, so let Poisson neurons register their first spike times *)
      let queue = List.fold_left (Array.fold_left kickoff) queue net.neurons in
      iter queue
    | (t, _) :: _ when t > duration ->
      (match ph with
      | Some ph -> print_msg ~ph (string_of_float duration)
      | None -> ());
      () (* finish with the first spike after goes past [duration] *)
    | (t, label) :: rest ->
      (* display time progress if applicable *)
      (match ph with
      | Some ph ->
        if t > !last_t_info +. 0.1
        then (
          print_msg ~ph (string_of_float t);
          last_t_info := t)
      | None -> ());
      (* process this spike *)
      (match label with
      | `poisson x ->
        (* if it's Poisson neuron, log its spike, and register a new one *)
        let queue = poisson_renew (`poisson x) t rest in
        (* propagate to target neurons *)
        let queue =
          List.fold_left
            (fun accu (w, post) -> process_input (to_lif post) (t, w) accu)
            queue
            x.Poisson.targets
        in
        iter queue
      | `lif pre ->
        (* if this spike comes from an E neuron, propagate it and update other neurons *)
        let queue =
          List.fold_left
            (fun accu (w, post) -> process_input (to_lif post) (t, w) accu)
            rest
            pre.LIF.targets
        in
        iter queue)
  in
  iter Queue.empty


let raster spikes =
  Array.fold_left
    (fun (i, accu) spike_list ->
      i + 1, List.fold_left (fun accu' t -> [| t; float i |] :: accu') accu spike_list)
    (0, [])
    spikes
  |> snd
  |> Array.of_list
  |> Mat.of_arrays


let plottable_voltage ?(style = "lc 8") ~duration (x : neuron) =
  let dt = 1E-3 in
  let v = voltage_trace ~dt ~duration x in
  let time = Mat.linspace 0. duration (Mat.row_num v) in
  Gp.item (L [ time; v ]) ~style:("l " ^ style)


let plottable_spikes ?(bar_height = 5.) ?(style = "lc 8") (x : neuron) =
  let data =
    spikes x
    |> (function
         | [] -> [| [| 0.; 0.; 0.; 0. |] |]
         | x -> x |> Array.of_list |> Array.map (fun t -> [| t; 0.; 0.; bar_height |]))
    |> Mat.of_arrays
  in
  Gp.item (A data) ~using:"1:2:3:4" ~style:("vectors nohead " ^ style)


let discard ~first = List.filter (fun t -> t > first)

let fano_factor ~window spikes =
  let slide = window /. 4. in
  let t0 = List.fold_left min max_float spikes
  and t1 = List.fold_left max min_float spikes in
  let rec get_counts accu t =
    if t > t1
    then accu
    else (
      let lb = t
      and ub = t +. window in
      let c = spikes |> List.filter (fun s -> s >= lb && s < ub) |> List.length in
      get_counts (c :: accu) (t +. slide))
  in
  match get_counts [] t0 with
  | [] -> 1.0 (* convention: default to one if empty *)
  | cs ->
    let cs = cs |> List.map float |> Array.of_list in
    let cs = Mat.of_array cs 1 (-1) in
    Mat.(var' cs /. mean' cs)
