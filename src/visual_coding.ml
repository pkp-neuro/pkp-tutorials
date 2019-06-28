(** Visual coding: dense vs sparse coding *)

open Owl

let minibatch file patch_size =
  let images = Arr.load file in
  let n_img, dim_x, dim_y =
    let s = Arr.shape images in
    s.(0), s.(1), s.(2)
  in
  fun batch_size ->
    Array.init batch_size (fun _ ->
        let i = Random.int n_img in
        let x = Random.int (dim_x - patch_size) in
        let y = Random.int (dim_y - patch_size) in
        images
        |> Arr.get_slice [ [ i ]; [ x; x + patch_size - 1 ]; [ y; y + patch_size - 1 ] ]
        |> fun m -> if Random.bool () then Arr.swap 1 2 m else m)
    |> Arr.concatenate ~axis:0


module Sparse_coding = struct
  type prms =
    { batch_size : int
    ; n_bases : int
    ; bfgs_max_iter : int
    ; lambda : float
    ; learning_rate : float
    }

  let default_prms =
    { batch_size = 100
    ; n_bases = 100
    ; bfgs_max_iter = 1000
    ; lambda = 0.3
    ; learning_rate = 1.
    }


  (* infer number of pixels *)
  let n_pix minibatch =
    let fake = minibatch 1 in
    let s = Arr.shape fake in
    assert (s.(1) = s.(2));
    s.(1) * s.(2)


  let cost ~prms ~sigma s =
    let open Algodiff.D in
    let s = pack_arr s in
    let z = float prms.batch_size in
    fun b a ->
      let error = Maths.(l2norm_sqr' (s - (b *@ a))) in
      let sparsity =
        let h = Maths.(F prms.lambda * log (F 1.0 + sqr (a / F sigma))) in
        Maths.(sum' h)
      in
      Maths.((error + sparsity) / F z)


  (* main loop *)
  let rec iter ?callback ~prms ~minibatch k a b =
    Gc.major ();
    let open Algodiff.D in
    (* grab the sigma from the previous iteration *)
    let s =
      minibatch prms.batch_size
      |> fun v -> Owl.Arr.reshape v [| prms.batch_size; -1 |] |> Owl.Mat.transpose
    in
    let cost = cost ~prms ~sigma:0.05 s in
    (* optimize the activations given fixed basis functions *)
    let a =
      Owl_lbfgs.minimise
        ~callback:(fun st _ -> Lbfgs.iter st > prms.bfgs_max_iter)
        (S { f = cost b; init_prms = Mat.zeros Mat.(row_num a) Mat.(col_num a) })
      |> snd
      |> Owl_lbfgs.unpack_s
    in
    (* take a gradient step for the basis functions *)
    let rec descent b kk =
      if kk = 0
      then b
      else (
        let db = (grad (fun b -> cost b a)) b in
        let b = Maths.(b - (F prms.learning_rate * db)) in
        descent b (kk - 1))
    in
    let b = descent b 1 |> unpack_arr in
    let b = Owl.Mat.(b / l2norm ~axis:0 b) |> pack_arr in
    (match callback with
    | Some f ->
      let c = cost b a |> unpack_flt in
      f k (unpack_arr a) (unpack_arr b) c
    | None -> ());
    iter ?callback ~prms ~minibatch (k + 1) a b


  let optimize ?callback prms minibatch =
    let n_pix = n_pix minibatch in
    let a =
      Mat.gaussian ~sigma:0.08 prms.n_bases prms.batch_size |> Algodiff.D.pack_arr
    in
    let b =
      let b = Mat.gaussian n_pix prms.n_bases in
      Mat.(b / l2norm ~axis:0 b) |> Algodiff.D.pack_arr
    in
    iter ?callback ~prms ~minibatch 1 a b
end

(** round a float to the nearest integer *)
let round x =
  let a = floor x
  and b = ceil x in
  int_of_float (if x -. a > b -. x then b else a)


let plot_bases ?(display_id = Jupyter_notebook.display "text/html" "") grid_size b =
  let n_pix, n_bases = Mat.shape b in
  assert (grid_size * grid_size <= n_bases);
  let size = n_pix |> float |> sqrt |> round in
  assert (size * size = n_pix);
  let open Gp in
  let figure (module P : Plot) =
    P.multiplot
      ~rect:((0.1, 0.1), (0.9, 0.9))
      ~spacing:(0.01, 0.01)
      (grid_size, grid_size)
      (fun k _ _ ->
        let patch = Mat.(reshape (col b k) [| size; size |]) in
        P.heatmap patch ~style:"image pixels" [ barebone; unset "colorbox" ])
  in
  Juplot.draw ~fmt:`png ~size:(500, 500) ~display_id figure


let default_callback () =
  let text_id = Jupyter_notebook.display "text/html" "" in
  let img_id = Jupyter_notebook.display "text/html" "" in
  let cost_history = ref [] in
  let log_cost c =
    cost_history := c :: !cost_history;
    if List.length !cost_history > 100
    then cost_history := !cost_history |> List.rev |> List.tl |> List.rev
  in
  let mean x = List.fold_left ( +. ) 0. x /. float (List.length x) in
  fun iter _ b cost ->
    log_cost cost;
    Jupyter_notebook.printf
      "iteration: %05i | cost [avg over last 100 iterations]: %.5f%!"
      iter
      (mean !cost_history);
    Jupyter_notebook.display_formatter ~display_id:text_id "text/html" |> ignore;
    if iter mod 10 = 0 then plot_bases ~display_id:img_id 10 b
