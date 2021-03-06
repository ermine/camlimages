
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Franois Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

val check_header : string -> Images.header;;
  (** Checks the file header of a png image. *)

val load : string -> Images.load_option list -> Images.t;;
  (** Loads a png image. *)

val load_as_rgb24 : string -> Images.load_option list -> Images.t;;
  (** Loads a png image, and coerce it to Rgb24. *)

val write_image :
  Unix.file_descr -> Images.save_option list -> Images.t -> unit;;

val save : string -> Images.save_option list -> Images.t -> unit;;
  (** Saves an image into a png file format. *)

val to_string : Images.save_option list -> Images.t  -> string
