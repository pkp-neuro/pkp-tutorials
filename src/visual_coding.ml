(** Visual coding: dense vs sparse coding *)

open Owl

module Image_patches (P : sig
  val file : string
  val patch_size : int
end) =
struct
  let images = Arr.load P.file

  let n_img, dim_x, dim_y =
    let s = Arr.shape images in
    s.(0), s.(1), s.(2)


  let minibatch batch_size =
    Array.init batch_size (fun _ ->
        let i = Random.int n_img in
        let x = Random.int (dim_x - P.patch_size) in
        let y = Random.int (dim_y - P.patch_size) in
        Arr.get_slice
          [ [ i ]; [ x; x + P.patch_size - 1 ]; [ y; y + P.patch_size - 1 ] ]
          images)
    |> Arr.concatenate ~axis:0
end

module type Sparse_coding_prms = sig
  val n_bases : int
  val batch_size : int
  val minibatch : int -> Arr.arr
  val bfgs_max_iter : int
  val bnorm : float
  val lambda : float
  val sigma : float
  val eta : float
end

module Sparse_coding (P : Sparse_coding_prms) = struct
  open P

  (* patch size *)
  let m, n_pix =
    let fake = minibatch 1 in
    let s = Arr.shape fake in
    assert (s.(1) = s.(2));
    s.(1), s.(1) * s.(2)


  let cost s =
    let open Algodiff.D in
    let s = pack_arr s in
    let z = float batch_size in
    fun b a ->
      let error = Maths.(l2norm_sqr' (s - (b *@ a))) in
      let sparsity =
        let h = Maths.(F lambda * log (F 1.0 + sqr (a / F sigma))) in
        Maths.(sum' h)
      in
      Maths.((error + sparsity) / F z)


  (*
  (* test if the variance is in the right ballpark *)
  let _ =
    let b =
      Array.init nb (fun _ ->
          let v = Array.init (m * m) (fun _ -> Rand.gaussian_noise 1.) in
          bnorm /. norm2 v *.| v )
    in
    let a = Array.init nb (fun _ -> Rand.exp_noise sigma) in
    printf "std generated = %f\n%!" (std (trans b ||* a)) ;
    let s = samples () >> flatten in
    printf "std data = %f\n%!" (std s) *)

  let a_bins = Mat.linspace (-3.) 3.0 601 |> Mat.to_array

  let frozen_data =
    minibatch batch_size
    |> fun v -> Owl.Arr.reshape v [| batch_size; -1 |] |> Owl.Mat.transpose


  (* main loop *)
  let rec iter ?callback k a b =
    let open Algodiff.D in
    let s =
      minibatch batch_size
      |> fun v -> Owl.Arr.reshape v [| batch_size; -1 |] |> Owl.Mat.transpose
    in
    let cost = cost s in
    (* optimize the activations given fixed basis functions *)
    let a =
      Owl_lbfgs.minimise
        ~callback:(fun st _ -> Lbfgs.iter st > bfgs_max_iter)
        (S { f = cost b; init_prms = a })
      |> snd
      |> Owl_lbfgs.unpack_s
    in
    let db = (grad (fun b -> cost b a)) b in
    let b = Maths.(b - (F eta * db)) |> unpack_arr in
    let b = Owl.Mat.(bnorm $* b / l2norm ~axis:0 b) |> pack_arr in
    (match callback with
    | Some f ->
      let c = cost b a |> unpack_flt in
      f k a b c
    | None -> ());
    iter ?callback (k + 1) a b


  let optimize ?callback () =
    let a = Mat.zeros n_bases batch_size |> Algodiff.D.pack_arr in
    let b =
      let b = Mat.gaussian n_pix n_bases in
      Mat.(bnorm $* b / l2norm ~axis:0 b) |> Algodiff.D.pack_arr
    in
    iter ?callback 1 a b
end

(** round a float to the nearest integer *)
let round x =
  let a = floor x
  and b = ceil x in
  int_of_float (if x -. a > b -. x then b else a)


let plot_bases ?(display_id = Jupyter_notebook.display "text/html" "") (n_x, n_y) b =
  let n_pix, n_bases = Mat.shape b in
  assert (n_x * n_y <= n_bases);
  let size = n_pix |> float |> sqrt |> round in
  assert (size * size = n_pix);
  let open Gp in
  let figure (module P : Plot) =
    P.multiplot
      ~rect:((0.1, 0.1), (0.9, 0.9))
      ~spacing:(0.002, 0.002)
      (n_x, n_y)
      (fun k _ _ ->
        let patch = Mat.(reshape (col b k) [| size; size |]) in
        P.heatmap patch [ barebone; unset "colorbox" ])
  in
  Juplot.draw ~fmt:`svg ~size:(500, 500) ~display_id figure
