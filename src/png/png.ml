
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

open Images;;

(* do not change the ordering, since the tags are used in png*.c *)
type png_read_result =
    | PNG_RGB24 of string array
    | PNG_RGBA32 of string array
    | PNG_INDEX8 of string array * rgb array
    | PNG_INDEX16 of string array * rgb array
    | PNG_INDEX4 of string array * rgb array
;;

external read_as_rgb24 : string -> int * int * string array  =
   "read_png_file_as_rgb24" 
;;

external read : string -> int * int * png_read_result  = "read_png_file"
;;
external write_rgb : Unix.file_descr -> string -> int -> int -> bool -> unit
  = "write_png_file_rgb"
;;
external write_index : Unix.file_descr ->
  string -> rgb array -> int -> int -> unit
  = "write_png_file_index"
;;
external write_rgb_to_buffer : string -> int -> int -> bool -> string
  = "write_png_rgb_to_buffer"
;;
external write_index_to_buffer : string -> rgb array -> int -> int -> string
  = "write_png_index_to_buffer"

let load_as_rgb24 name _opts =
  let w, h, buf = read_as_rgb24 name in
  Rgb24 (Rgb24.create_with_scanlines w h [] buf)
;;

let load name _opts =
  let w, h, res = read name in
  match res with
  | PNG_RGB24 buf -> Rgb24 (Rgb24.create_with_scanlines w h [] buf)
  | PNG_RGBA32 buf -> Rgba32 (Rgba32.create_with_scanlines w h [] buf)
  | PNG_INDEX8 (buf,cmap) ->
      Index8 (Index8.create_with_scanlines w h [] { max = 255; map = cmap; } (-1) buf)
  | PNG_INDEX16 (buf,cmap) ->
      Index16 (Index16.create_with_scanlines w h [] { max = 65535; map = cmap } (-1) buf)
  | PNG_INDEX4 (buf,cmap) ->
      let buf' = Array.init h (fun _ -> String.create w) in
      for y = 0 to h - 1 do
        for x = 0 to w - 1 do
          buf'.(y).[x] <-
            char_of_int
              (let c = int_of_char buf.(y).[x / 2] in
               if x mod 2 = 0 then c lsr 4 else c mod 16)
        done
      done;
      Index8 (Index8.create_with_scanlines w h [] { max = 16; map = cmap } (-1) buf')
;;

let write_image fd _opts image =
  match image with
    | Rgb24 bmp ->
        write_rgb fd
          (Rgb24.dump bmp) bmp.Rgb24.width bmp.Rgb24.height false
    | Rgba32 bmp ->
        write_rgb fd
          (Rgba32.dump bmp) bmp.Rgba32.width bmp.Rgba32.height true
    | Index8 bmp ->
        write_index fd (Index8.dump bmp) bmp.Index8.colormap.map
          bmp.Index8.width bmp.Index8.height
    | Index16 bmp ->
        write_index fd (Index16.dump bmp)
          bmp.Index16.colormap.map
          bmp.Index16.width bmp.Index16.height
    | Cmyk32 _ -> failwith "Saving of CMYK not supported yet"
;;

let save name opts image =
  let fd = Unix.openfile name
    [Unix.O_APPEND; Unix.O_CREAT; Unix.O_TRUNC] 0o666 in
    write_image fd opts image

;;

let to_string _opts image =
  match image with
    | Rgb24 bmp ->
        write_rgb_to_buffer
          (Rgb24.dump bmp) bmp.Rgb24.width bmp.Rgb24.height false
    | Rgba32 bmp ->
        write_rgb_to_buffer
          (Rgba32.dump bmp) bmp.Rgba32.width bmp.Rgba32.height true
    | Index8 bmp ->
        write_index_to_buffer (Index8.dump bmp) bmp.Index8.colormap.map
          bmp.Index8.width bmp.Index8.height
    | Index16 bmp ->
        write_index_to_buffer (Index16.dump bmp)
          bmp.Index16.colormap.map
          bmp.Index16.width bmp.Index16.height
    | Cmyk32 _ -> failwith "Saving of CMYK not supported yet"
;;  

let check_header filename =
  let len = 24 in
  let ic = open_in_bin filename in
  try
    let str = String.create len in
    really_input ic str 0 len;
    close_in ic;
    if String.sub str 1 3 = "PNG" then begin
      if String.sub str 0 8 <> "\137PNG\013\010\026\010" then begin
          { header_width= -1;
            header_height= -1;
            header_infos= [Info_Corrupted]; }
      end else begin
          let belong str =
            int_of_char str.[0] lsl 24 +
            int_of_char str.[1] lsl 16 +
            int_of_char str.[2] lsl 8 +
            int_of_char str.[3] in
          let w = belong (String.sub str 16 4) in
          let h = belong (String.sub str 20 4) in
          let bdepth = Info_Depth (int_of_char str.[12]) in
          let infos =
            try
              let colormodel =
                match int_of_char str.[13] with
                | 0 -> Info_ColorModel Gray
                | 2 -> Info_ColorModel RGB
                | 3 -> Info_ColorModel Index
                | 4 -> Info_ColorModel GrayA
                | 6 -> Info_ColorModel RGBA
                | _ -> raise Not_found in
              [colormodel; bdepth]
            with
            | Not_found -> [bdepth] in
          { header_width = w;
            header_height = h;
            header_infos = infos; }
      end
    end else raise Wrong_file_type
  with
  | _ -> close_in ic; raise Wrong_file_type
;;

let _ =
  add_methods Png {
    check_header = check_header;
    load = Some load;
    save = Some save;
    write_image = Some write_image;
    to_string = Some to_string;
    load_sequence = None;
    save_sequence = None;
  }
;;
  
