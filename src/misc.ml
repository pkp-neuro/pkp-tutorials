open Gp

let silence () = Jupyter_notebook.clear_output ()

let time_indicator dt =
  Jupyter_notebook.clear_output () ;
  let id = Jupyter_notebook.display_formatter "text/html" in
  fun t ->
    Jupyter_notebook.printf
      "<table><tr><td>time</td><td>%5.3f</td></tr></table>%!"
      (dt *. float t) ;
    Jupyter_notebook.display_formatter ~display_id:id "text/html" |> ignore

let how_long f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let t1 = Unix.gettimeofday () in
  (result, t1 -. t0)


