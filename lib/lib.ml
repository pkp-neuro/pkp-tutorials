open Printf
open Owl
module P = Pervasives

(* -------- network dynamics ------- *)

module type PT = sig
  val w_rec: Mat.mat
  val dt: float
  val sampling_dt: float
  val tau: float (* time constant of cortical dynamics *)
  val noise_prms: (float * Mat.mat) option (* optional input noise parameters *)
  val rate_function: Mat.mat -> Mat.mat
end

module Dynamics (Prms: PT) = struct

  open Prms
  let n, _ = Mat.shape w_rec

  let noise = match noise_prms with 
    | None -> None
    | Some (tau_eta, sigma_eta) -> 
      let ell = Linalg.D.chol ~upper:false sigma_eta in
      let eta = ref Mat.(ell *@ gaussian n 1) in
      let decay = 1. -. dt /. tau_eta in
      let factor = sqrt (2. *. dt /. tau_eta) in
      Some (fun () ->
          eta := Mat.((decay $* !eta) + (factor $* (ell *@ gaussian n 1)));
          !eta)

  let simulate ?(verbose=false) ~duration =
    let n_bins = Maths.round (duration /. dt) |> int_of_float in
    let sample_every = Maths.round (sampling_dt /. dt) |> int_of_float in
    let x_data = Mat.zeros P.(n_bins / sample_every) n in
    let x = ref (Mat.zeros n 1) in
    for t=0 to n_bins-1 do
      let time = dt *. float t in
      if t mod 10 = 0 then begin
        if verbose then printf "\r[dynamics] %.3f / %.3f%!" time duration;
      end;
      if t mod 1000 = 0 then Gc.minor ();
      let r = rate_function !x in
      let input = Mat.(w_rec *@ r) in
      let input = match noise with Some draw -> Mat.(input + draw ()) | None -> input in
      let xdot = Mat.(P.(dt /. tau) $* ((neg !x) + input)) in
      x := Mat.(!x + xdot);
      if t mod sample_every = 0 
      then Mat.copy_ ~out:Mat.(row x_data P.(t/sample_every)) r;
    done;
    if verbose then print_newline ();
    x_data

end


