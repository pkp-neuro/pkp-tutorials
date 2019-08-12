open Owl

let load_images ?(file = "/home/opam/pkp/pkp-tutorials/data/natural_images.bin") () =
  Arr.load file


let get_from_arr x i = x |> Arr.get_slice [ [ i ] ] |> Arr.squeeze

let round x =
  let a = floor x
  and b = ceil x in
  int_of_float (if x -. a > b -. x then b else a)


let resize_to_img x =
  if Arr.num_dims x = 3
  then x
  else (
    let n = Arr.(shape x).(0) in
    let n_pix = Arr.(shape x).(1) in
    let size = round (sqrt (float n_pix)) in
    Arr.reshape x [| n; size; size |])


type placeholder = Jupyter_notebook.display_id

let placeholder () = Jupyter_notebook.display "text/html" ""

let plot_image ?ph ?(size = 300, 300) x =
  let open Gp in
  let r = max (abs_float (Mat.min' x)) (Mat.max' x) in
  let figure (module P : Plot) =
    P.heatmap
      Mat.(flip ~axis:0 x)
      [ barebone
      ; set "size square"
      ; margins [ `top 1.0; `bottom 0.; `left 0.; `right 1. ]
      ; set "palette model RGB defined (-1 'black', 1 'white')"
      ; cbrange (-.r, r)
      ]
  in
  Juplot.draw ~fmt:`png ?display_id:ph ~size figure


type stream = int -> Arr.arr

let create_stream images =
  let patch_size = 14 in
  let n_img = Arr.(shape images).(0) in
  let dim_x = Arr.(shape images).(1) in
  let dim_y = Arr.(shape images).(2) in
  fun batch_size ->
    Array.init batch_size (fun _ ->
        let i = Random.int n_img in
        let x = Random.int (dim_x - patch_size) in
        let y = Random.int (dim_y - patch_size) in
        Arr.get_slice
          [ [ i ]; [ x; x + patch_size - 1 ]; [ y; y + patch_size - 1 ] ]
          images
        |> fun m -> if Random.bool () then Arr.swap 1 2 m else m)
    |> Arr.concatenate ~axis:0


let sample_stream stream k = stream k

let visualise_stream stream =
  let display_id = Jupyter_notebook.display "text/html" "" in
  let rec iter k =
    if k > 0
    then (
      let x = get_from_arr (stream 1) 0 in
      let open Gp in
      let figure (module P : Plot) =
        P.heatmap
          x
          [ barebone
          ; cbrange (-3., 3.)
          ; margins [ `bottom 0.; `top 1.0; `left 0.; `right 1.0 ]
          ; set "size square"
          ; set "palette model RGB defined (-1 'black', 1 'white')"
          ]
      in
      Juplot.draw ~fmt:`png ~size:(50, 50) ~display_id figure;
      Unix.sleepf 0.1;
      iter (k - 1))
  in
  iter 100


let grid_size n =
  let rec find k = if k * k >= n then k else find (k + 1) in
  find 1


let plot_patches ?(display_id = Jupyter_notebook.display "text/html" "") p =
  let n = min Arr.(shape p).(0) 100 in
  let p = Arr.get_slice [ [ 0; pred n ] ] p in
  let p = resize_to_img p in
  let gs = grid_size n in
  let r = 1.5 *. Stats.quantile (Arr.to_array (Arr.abs p)) 0.95 in
  let open Gp in
  let figure (module P : Plot) =
    P.multiplot
      ~rect:((0.1, 0.1), (0.9, 0.9))
      ~spacing:(0.01, 0.01)
      (gs, gs)
      (fun k _ _ ->
        if k < n
        then (
          let patch = get_from_arr p k in
          P.heatmap
            patch
            [ barebone
            ; unset "colorbox"
            ; cbrange (-.r, r)
            ; set "palette model RGB defined (-1 'blue', 0 'white', 1 'red')"
            ]))
  in
  Juplot.draw ~fmt:`png ~size:(500, 500) ~display_id figure


let default_callback iterations =
  let text_id = Jupyter_notebook.display "text/html" "" in
  let img_id = Jupyter_notebook.display "text/html" "" in
  fun iter proto ->
    Jupyter_notebook.printf "iteration: %05i/%i%!" iter iterations;
    Jupyter_notebook.display_formatter ~display_id:text_id "text/html" |> ignore;
    if iter mod 10 = 0 then plot_patches ~display_id:img_id proto


(* infer number of pixels *)
let n_pix stream =
  let fake = get_from_arr (stream 1) 0 in
  let a, b = Mat.shape fake in
  a * b


(* s: n_img x size x size *)
let most_likely_intensities cost_fun proto s =
  let batch_size = Arr.(shape s).(0) in
  let n_proto = Arr.(shape proto).(0) in
  let proto = Arr.reshape proto [| n_proto; -1 |] in
  let open Algodiff.D in
  (* function to be minimised *)
  let f = cost_fun s (pack_arr proto) in
  Owl_lbfgs.minimise
    ~callback:(fun st _ -> Lbfgs.iter st > 1000)
    (S { f; init_prms = Mat.gaussian batch_size n_proto })
  |> snd
  |> Owl_lbfgs.unpack_s
  |> unpack_arr


(* main optimization loop *)
let optimise ~n_proto ~batch_size ~iterations ~learning_rate ~cost_fun stream =
  let callback = default_callback iterations in
  let n_pix = n_pix stream in
  let proto =
    let proto = Mat.gaussian n_proto n_pix in
    Mat.(proto / l2norm ~axis:1 proto) |> Algodiff.D.pack_arr
  in
  let rec iter k proto =
    if k > iterations
    then resize_to_img (Algodiff.D.unpack_arr proto)
    else (
      Gc.minor ();
      (* sample image patches from the stream *)
      let s = stream batch_size in
      let open Algodiff.D in
      let n_proto = Arr.(shape proto).(0) in
      let cost_ = cost_fun s in
      (* optimise the activations given fixed prototypical templates *)
      let x =
        Owl_lbfgs.minimise
          ~callback:(fun st _ -> Lbfgs.iter st > 1000)
          (S { f = cost_ proto; init_prms = Mat.zeros batch_size n_proto })
        |> snd
        |> Owl_lbfgs.unpack_s
      in
      let proto =
        let dproto = (grad (fun proto -> cost_ proto x)) proto in
        Maths.(proto - (F learning_rate * dproto)) |> unpack_arr
      in
      (* renormalise and perhaps display *)
      let proto = Owl.Mat.(proto / l2norm ~axis:1 proto) in
      (* compute cost *)
      callback k proto;
      iter (k + 1) (pack_arr proto))
  in
  iter 0 proto


module Dense_model = struct
  let cost_fun s =
    let lambda = 0.3 in
    let batch_size = Arr.(shape s).(0) in
    let s = Arr.reshape s [| batch_size; -1 |] in
    let open Algodiff.D in
    let s = pack_arr s in
    fun proto x ->
      let error = Maths.(l2norm_sqr' (s - (x *@ proto))) in
      let sparsity = Maths.(F lambda * l2norm_sqr' x) in
      Maths.((error + sparsity) / F (float batch_size))


  let learn ?(k = 25) ?(iterations=1000) ?(learning_rate = 0.02) stream =
    optimise ~n_proto:k ~batch_size:100 ~iterations ~learning_rate ~cost_fun stream


  let most_likely_intensities = most_likely_intensities cost_fun
end

module Sparse_model = struct
  (* s : n_img * size * size *)
  let cost_fun ~lambda s =
    let batch_size = Arr.(shape s).(0) in
    let s = Arr.reshape s [| batch_size; -1 |] in
    let open Algodiff.D in
    let sigma = 0.05 in
    let s = pack_arr s in
    (* proto: n_proto x n_pix
     x: batch_size x n_proto
  *)
    fun proto x ->
      let error = Maths.(l2norm_sqr' (s - (x *@ proto))) in
      let sparsity =
        let h = Maths.(F lambda * log (F 1.0 + sqr (x / F sigma))) in
        Maths.(sum' h)
      in
      Maths.((error + sparsity) / F (float batch_size))


  let learn ?(k = 100) ?(iterations=1000) ?(learning_rate = 0.5) stream =
    let cost_fun = cost_fun ~lambda:0.3 in
    optimise ~n_proto:k ~batch_size:100 ~iterations ~learning_rate ~cost_fun stream


  let most_likely_intensities ?(lambda = 0.01) proto s =
    most_likely_intensities (cost_fun ~lambda) proto s
end

let reconstruct proto x =
  let batch_size, n_proto = Mat.shape x in
  assert (Arr.num_dims proto = 3 && Arr.(shape proto).(0) = n_proto);
  let size = Arr.(shape proto).(1) in
  let proto = Arr.reshape proto [| n_proto; -1 |] in
  let s = Mat.(x *@ proto) in
  Mat.reshape s [| batch_size; size; size |]
