(** Abstract type of memory patterns *)
type pattern

(** Create a random memory pattern
    (a vector with random binary entries)*)
val random_pattern : int -> pattern

(** Plot a memory pattern as a bar code *)
val plot_pattern : ?display_id:Jupyter_notebook.display_id -> pattern -> unit

(** Check if two patterns are equal. *)
val are_equal : pattern -> pattern -> bool

(** Corrupt a memory pattern by randomly flipping a [fraction] of elements
   (i.e. if they were zero, they become one, and vice-versa) *)
val corrupt : fraction:float -> pattern -> pattern

(** [compare (x1, label1) (x2, label2)] displays the a graph showing two
    patterns as bar codes, lined up on top of each other, allowing visual
    comparison between the two. You must pass labels that will be displayed
    along side each pattern. *)
val compare_patterns
  :  ?display_id:Jupyter_notebook.display_id
  -> pattern * string
  -> pattern * string
  -> unit

(** Abstract type of connections *)
type connections

(** Build the appropriate connectivity to "store" (memorise)
    a set of memory patterns *)
val connectivity : pattern array -> connections

(** [recall t_max connections cue] simulates network dynamics for some duration [t_max]
    (usually 10. is good) starting from some [cue] (typically, a corrupted
    memory pattern), and returns the final network state (hopefully, if recall
    is successful, this will be the original uncorrupted memory pattern!). If
    you give the optional argument [~compare_with:true_pattern], then an
    animation will be displayed showing the progressive evolution of the state
    of the network, compared with the pattern it ought to be recalling. *)
val recall : ?compare_with:pattern -> t_max:float -> connections -> pattern -> pattern
