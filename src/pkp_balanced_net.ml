open Owl

(** Hello world *)
let maybe_do x f = match x with Some z -> f z | None -> ()

(* --- Population of Poisson neurons ---- *)

module Poisson_population (P: sig
    val n: int
    val rate: float
    val dt: float
  end) = struct

  let n = P.n
  let p = P.dt *. P.rate
  let ids = Array.init n (fun i -> i)
  let _spikes = ref [| |]
  let spikes () = !_spikes
  let step () = _spikes := Stats.(choose ids (binomial_rvs ~p ~n))

end

(* --- Population of leaky integrate-and-fire (LIF) neurons ---- *)

module LIF_population (P: sig
    val free_vm: bool
    val n: int
    val dt: float
    val tau: float
    val threshold: float
  end) = struct

  open P

  let n = P.n
  let us = Mat.zeros 1 n
  let _spikes = ref [| |]
  let spikes () = !_spikes

  let step inputs =
    (* decay *)
    Mat.scalar_mul_ ~out:us (1. -. dt /. tau) us;
    (* synaptic input *)
    List.iter (fun (w, synapses, spikes) ->
        (* synapses.(pre) = list of post 
         * spikes: list of pre *)
        Array.iter (fun pre ->
            let slice = [ I 0; L synapses.(pre) ] in
            Mat.set_fancy slice us Mat.(w $+ get_fancy slice us)
          ) (spikes ())
      ) inputs;
    (* threshold-crossing *)
    _spikes := Mat.filter (fun u -> u > threshold) us;
    (* reset *)
    Mat.set_fancy [ I 0; L (Array.to_list !_spikes) ] us Mat.(zeros 1 (Array.length !_spikes))

end

(* dealing with spike trains *)

let plottable_raster ~dt indices =
  let r = ref [] in
  Array.iteri (fun t s -> 
      Array.iter (fun k -> r := [| dt *. float t; float k |] :: !r) s
    ) indices;
  !r |> Array.of_list |> Mat.of_arrays

let population_rate ~n ~dt spikes =
  Mat.of_array
    Array.(map (fun s -> (float (length s)) /. dt /. float n) spikes)
    1 (-1)

(* spikes: list of spike times *)
let ff ~dt ~tmin ~tmax ~window ~window_shift train =
  let counts = ref [] in
  let t = ref tmin in
  while !t +. window < tmax do
    let _t = !t and _tt = !t +. window in 
    let count = List.fold_left (fun c ts -> 
        if ts >= _t && ts < _tt then c + 1 else c) 0 train in
    counts := count :: !counts;
    t := !t +. window_shift;
  done;
  let counts = !counts |> Array.of_list |> Array.map float in
  Stats.var counts /. Stats.mean counts

let average_ff spike_lists n ~dt ~tmin ~tmax ~window ~window_shift =
  let trains = Array.make n [] in
  Array.iteri (fun t s ->
      List.iter (fun k -> trains.(k) <- (dt *. float t) :: trains.(k)) s
    ) spike_lists;
  trains |> Array.map (ff ~dt ~tmin ~tmax ~window ~window_shift) |> Stats.mean 

let discard_transient dt v =
  let cut = int_of_float (0.1 /. dt)  in
  Array.sub v cut (Array.length v - cut)

(* ------ Build connectivity ------- *)

let random_synapses n_in n_out n_syn =
  let ids = Array.init n_in (fun i -> i) in
  Array.init n_out (fun _ -> Stats.choose ids n_syn |> Array.to_list)


