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


let print_msg s =
  Jupyter_notebook.printf "%s" s;
  Jupyter_notebook.display_formatter "text/plain" |> ignore


let time_indicator dt =
  Jupyter_notebook.clear_output ();
  let id = Jupyter_notebook.display_formatter "text/html" in
  fun t ->
    Jupyter_notebook.printf
      "<table><tr><td>time</td><td>%5.3f</td></tr></table>%!"
      (dt *. float t);
    Jupyter_notebook.display_formatter ~display_id:id "text/html" |> ignore


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


(** simple function memoization; takes a function f, returns the
    memoized version of it *)
let memoize f =
  let table = Hashtbl.create 100 in
  fun x ->
    try Hashtbl.find table x with
    | Not_found ->
      let y = f x in
      Hashtbl.add table x y;
      y


(** memoization for recursive functions; takes the non-recursive tail call function, 
    and makes it a memoize recursive function;
    e.g. memoize_rec (fun fib i -> if i<=1 then i else fib (i-1) + fib (i-2)) *)
let memoize_rec f_norec =
  let fref = ref (fun _ -> assert false) in
  let f = memoize (fun x -> f_norec !fref x) in
  fref := f;
  f
