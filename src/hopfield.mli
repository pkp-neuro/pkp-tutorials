(** Abstract type of memory patterns *)
type pattern

(** Create a random memory pattern
    (a vector with random binary entries)*)
val random_pattern : int -> pattern

(** Check if two patterns are equal. *)
val are_equal : pattern -> pattern -> bool

(** Corrupt a memory pattern by randomly flipping a [fraction] of elements
   (i.e. if they were zero, they become one, and vice-versa) *)
val corrupt : fraction:float -> pattern -> pattern

(** Displays a graph showing two patterns lined up on top of each other,
    allowing visual comparison between the two. *)
val compare_patterns
  :  ?display_id:Jupyter_notebook.display_id
  -> pattern
  -> pattern
  -> unit

(** Abstract type of connections *)
type connections

(** Build the appropriate connectivity to "store" (memorise)
    a set of memory patterns *)
val connectivity : pattern array -> connections

(** Simulates network dynamics starting from some cued memory pattern.
    If you pass the optional argument [~display:true_pattern] then an
    animation will be displayed showing the progressive evolution of the
    state of the network, compared with the pattern it ought to be recalling. *)
val recall : ?display:pattern -> t_max:float -> connections -> pattern -> pattern
