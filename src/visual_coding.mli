open Owl

(** {1 Dealing with the data (images)} *)

(** Load the image bank from disk *)
val load_images : ?file:string -> unit -> Arr.arr

(** [get_from_arr x i] extracts the [i]^th matrix from 3D array [x] *)
val get_from_arr : Arr.arr -> int -> Mat.mat

type placeholder

val placeholder : unit -> placeholder

(** Plot an image in the notebook *)
val plot_image : ?ph:placeholder -> ?size:int * int -> Mat.mat -> unit

type stream

(** Creates a stream of small image patches from a bank of large images. *)
val create_stream : Arr.arr -> stream

(** [sample_stream s k] samples [k] image patches from stream [s]. *)
val sample_stream : stream -> int -> Arr.arr

(** Samples a sequence of image patches from a stream and plots them in the notebook. *)
val visualise_stream : stream -> unit

(** Plot a bank of image patches on a grid *)
val plot_patches : ?display_id:Jupyter_notebook.display_id -> Arr.arr -> unit

(** {1 Learning internal models} *)

module Dense_model : sig
  (** Learn a dense model from a stream of data. Returns the
      set of prototypical patches that constitute the model. *)
  val learn : ?k:int -> ?iterations:int -> ?learning_rate:float -> stream -> Arr.arr

  (** [most_likely_intensities proto s] returns the feature intensities that are
      most likely to have given rise to a particular image [s], given a set
      [proto] of prototypical features. If there are [N] images packed in [s],
      and [K] prototypical features, then the result is a [N x K] matrix. *)
  val most_likely_intensities : Arr.arr -> Arr.arr -> Mat.mat
end

module Sparse_model : sig
  (** Learn a dense model from a stream of data. Returns the
      set of [k] prototypical patches that constitute the model. *)
  val learn : ?k:int -> ?iterations:int -> ?learning_rate:float -> stream -> Arr.arr

  (** [most_likely_intensities proto s] returns the feature intensities that are
      most likely to have given rise to a particular image [s], given a set
      [proto] of prototypical features. If there are [N] images packed in [s],
      and [K] prototypical features, then the result is an [N x K] matrix. The
      optional parameter [lambda] determines how sparse the intensities are
      assumed to be, a priori. *)
  val most_likely_intensities : ?lambda:float -> Arr.arr -> Arr.arr -> Mat.mat
end

(** [reconstruct proto x] reconstructs an image patch as a linear combination of
    prototypical features given in [proto], weighted by intensities given in [x].
    If [proto] is a 3D array of dimensions [K x M x M], and [x] is a [N x K]
    matrix, then the result is a 3D array of dimensions [N x M x M], i.e. [N]
    images of size [M x M] each. *)
val reconstruct : Arr.arr -> Mat.mat -> Arr.arr
