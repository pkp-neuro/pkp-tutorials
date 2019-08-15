open Owl

let upgrade () =
  let open Bos in
  let c = Cmd.(v "sh" % "/home/opam/update.sh") in
  OS.Cmd.(run c)


let quiet_owl () =
  let c = "#remove_printer Dense.Ndarray.Generic.pp_dsnda;;" in
  c
  |> Lexing.from_string
  |> !Toploop.parse_toplevel_phrase
  |> Toploop.execute_phrase true Format.err_formatter
  |> ignore


let hooting_owl () =
  let c = "#install_printer Dense.Ndarray.Generic.pp_dsnda;;" in
  c
  |> Lexing.from_string
  |> !Toploop.parse_toplevel_phrase
  |> Toploop.execute_phrase true Format.err_formatter
  |> ignore


type placeholder = Jupyter_notebook.display_id

let placeholder () = Jupyter_notebook.display "text/html" ""

let print_msg ?ph s =
  Jupyter_notebook.printf "%s%!" s;
  Jupyter_notebook.display_formatter ?display_id:ph "text/html" |> ignore



let with_indicator ?ph ?(description="index") iter_fun =
  let display_id =
    match ph with
    | Some d -> d
    | None -> placeholder ()
  in
  fun f ->
    let i = ref 0 in
    iter_fun (fun z ->
        incr i;
        Jupyter_notebook.printf "[%s] %i%!" description !i;
        Jupyter_notebook.display_formatter ~display_id "text/html" |> ignore;
        f z)


let average_over ?display n f =
  let trials = Array.init n (fun k ->
      (match display with
      | Some (display_id, label) ->
        Jupyter_notebook.printf "[%s] %05i / %05i" label k n;
        Jupyter_notebook.display_formatter ~display_id "text/html" |> ignore
      | None -> ());
      f ()) in
  let mu = Stats.mean trials in
  let delta = 1.96 *. Stats.sem trials in
  mu, (mu -. delta, mu +. delta)


let how_long f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let t1 = Unix.gettimeofday () in
  result, t1 -. t0


let ou_process ~tau ~dt ~duration =
  let n = int_of_float (duration /. dt) in
  let x = Mat.gaussian 1 n in
  for t = 1 to pred n do
    Mat.set
      x
      0
      t
      ((Mat.get x 0 (pred t) *. (1. -. (dt /. tau)))
      +. (Mat.get x 0 t *. dt /. tau *. sqrt (2. /. tau)))
  done;
  x


let memoize f =
  let table = Hashtbl.create 100 in
  fun x ->
    try Hashtbl.find table x with
    | Not_found ->
      let y = f x in
      Hashtbl.add table x y;
      y


let memoize_rec f_norec =
  let fref = ref (fun _ -> assert false) in
  let f = memoize (fun x -> f_norec !fref x) in
  fref := f;
  f

let hist ~n_bins x =
  let open Owl_stats in
  let hist = histogram (`N n_bins) Mat.(to_array x) in
  let bins = Mat.of_array hist.bins (-1) 1 |> Mat.get_slice [ [ 0; -2 ] ] in
  let counts = Mat.of_array (Array.map float hist.counts) (-1) 1 in
  Mat.(bins @|| counts)
