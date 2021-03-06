/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Franois Pessaux, projet Cristal, INRIA Rocquencourt     */
/*            Pierre Weis, projet Cristal, INRIA Rocquencourt          */
/*            Jun Furuse, projet Cristal, INRIA Rocquencourt           */
/*                                                                     */
/*  Copyright 1999,2000                                                */
/*  Institut National de Recherche en Informatique et en Automatique.  */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <png.h>

/* structure to store PNG image bytes */
struct mem_buffer {
  char *buffer;
  size_t size;
};

void png_write_data_to_buffer(png_structp png_ptr, png_bytep data, 
                              png_size_t length) {
  struct mem_buffer* p = (struct mem_buffer*) png_get_io_ptr(png_ptr);
  size_t nsize = p->size + length;

  /* allocate or grow buffer */
  if(p->buffer)
    p->buffer = realloc(p->buffer, nsize);
  else
    p->buffer = malloc(nsize);
  if(!p->buffer)
    png_error(png_ptr, "Write Error");

  /* copy new bytes to end of buffer */
  memcpy(p->buffer + p->size, data, length);
  p->size += length;
}

CAMLprim value write_png_rgb_to_buffer(value buffer, value width, value height,
                                       value with_alpha) {
  CAMLparam4(buffer, width, height, with_alpha);
  CAMLlocal1(vres);

  png_structp png_ptr;
  png_infop info_ptr;
  /* static */
  struct mem_buffer state;
  
  int w, h, a;

  /* initialise - put this before png_write_png() call */
  state.buffer = NULL;
  state.size = 0;

  w = Int_val(width);
  h = Int_val(height);
  a = Bool_val(with_alpha);

  if ((png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
                                         NULL, NULL, NULL)) == NULL )
    failwith("png_create_write_struct");

  if((info_ptr = png_create_info_struct(png_ptr)) == NULL ){
    png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
    failwith("png_create_info_struct");
  }

  /* error handling */
  if (setjmp(png_jmpbuf(png_ptr))) {
    /* Free all of the memory associated with the png_ptr and info_ptr */
    png_destroy_write_struct(&png_ptr, &info_ptr);
    failwith("png write error");
  }

  /* the final arg is NULL because we dont need in flush() */
  png_set_write_fn(png_ptr, &state, png_write_data_to_buffer, NULL);

  /* we use system default compression */
  /* png_set_filter(png_ptr, 0, PNG_FILTER_NONE |
     PNG_FILTER_SUB | PNG_FILTER_PAETH ); */
  /* png_set_compression...() */

  png_set_IHDR(png_ptr, info_ptr, w, h,
                8 /* fixed */,
                a ? PNG_COLOR_TYPE_RGB_ALPHA : PNG_COLOR_TYPE_RGB, /* fixed */
                PNG_INTERLACE_ADAM7,
                PNG_COMPRESSION_TYPE_DEFAULT,
                PNG_FILTER_TYPE_DEFAULT );

  /* infos... */

  png_write_info(png_ptr, info_ptr);

  {
    int rowbytes, i;
    png_bytep *row_pointers;
    char *buf = String_val(buffer);

    row_pointers = (png_bytep*)stat_alloc(sizeof(png_bytep) * h);

    rowbytes= png_get_rowbytes(png_ptr, info_ptr);
    for(i=0; i< h; i++){
      row_pointers[i] = (png_bytep)(buf + rowbytes * i);
    }

    png_write_image(png_ptr, row_pointers);
    stat_free((void*)row_pointers);
  }

  png_write_end(png_ptr, info_ptr);
  png_destroy_write_struct(&png_ptr, &info_ptr);

  vres = caml_alloc_string(state.size);
  memcpy(String_val(vres), state.buffer, state.size);
  free(state.buffer);
  CAMLreturn(vres);
}

CAMLprim value write_png_file_rgb(value fd, value buffer, value width, 
                                  value height, value with_alpha) {
  CAMLparam5(fd, buffer, width, height, with_alpha);

  FILE *fp;
  png_structp png_ptr;
  png_infop info_ptr;

  int w, h;
  int a;

  w = Int_val(width);
  h = Int_val(height);
  a = Bool_val(with_alpha);

  if ((fp = fdopen(Int_val(fd), "wb")) == NULL ){
    failwith("png file open failed");
  }

  if ((png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
                                         NULL, NULL, NULL)) == NULL ){
    fclose(fp);
    failwith("png_create_write_struct");
  }

  if((info_ptr = png_create_info_struct(png_ptr)) == NULL ){
    fclose(fp);
    png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
    failwith("png_create_info_struct");
  }

  /* error handling */
  if (setjmp(png_jmpbuf(png_ptr))) {
    /* Free all of the memory associated with the png_ptr and info_ptr */
    png_destroy_write_struct(&png_ptr, &info_ptr);
    fclose(fp);
    /* If we get here, we had a problem writing the file */
    failwith("png write error");
  }

  /* use standard C stream */
  png_init_io(png_ptr, fp);

  /* we use system default compression */
  /* png_set_filter(png_ptr, 0, PNG_FILTER_NONE |
     PNG_FILTER_SUB | PNG_FILTER_PAETH ); */
  /* png_set_compression...() */

  png_set_IHDR(png_ptr, info_ptr, w, h,
                8 /* fixed */,
                a ? PNG_COLOR_TYPE_RGB_ALPHA : PNG_COLOR_TYPE_RGB, /* fixed */
                PNG_INTERLACE_ADAM7,
                PNG_COMPRESSION_TYPE_DEFAULT,
                PNG_FILTER_TYPE_DEFAULT );

  /* infos... */

  png_write_info(png_ptr, info_ptr);

  {
    int rowbytes, i;
    png_bytep *row_pointers;
    char *buf = String_val(buffer);

    row_pointers = (png_bytep*)stat_alloc(sizeof(png_bytep) * h);

    rowbytes= png_get_rowbytes(png_ptr, info_ptr);
#if 0
    printf("rowbytes= %d width=%d\n", rowbytes, w);
#endif
    for(i=0; i< h; i++){
      row_pointers[i] = (png_bytep)(buf + rowbytes * i);
    }

    png_write_image(png_ptr, row_pointers);
    stat_free((void*)row_pointers);
  }

  png_write_end(png_ptr, info_ptr);
  png_destroy_write_struct(&png_ptr, &info_ptr);

  fclose(fp);

  CAMLreturn(Val_unit);
}

void PngPalette_val(value cmap, png_colorp *pltep, int *lenp )
{
  int i;

  if(cmap == Atom(0) ){
    *pltep = NULL;
    *lenp = 0;
    return;
  }
  *lenp = Wosize_val(cmap );
  *pltep = malloc(sizeof(png_color ) * *lenp );

  for(i=0; i< *lenp; i++){
    (*pltep)[i].red = Int_val(Field(Field(cmap,i),0));
    (*pltep)[i].green = Int_val(Field(Field(cmap,i),1));
    (*pltep)[i].blue = Int_val(Field(Field(cmap,i),2));
  }
  return;
}

CAMLprim value write_png_index_to_buffer(value buffer, value cmap, value 
                                         width, value height) {
  CAMLparam4(buffer, cmap, width, height);
  CAMLlocal1(vres);

  png_structp png_ptr;
  png_infop info_ptr;
  /* static */
  struct mem_buffer state;

  int w, h;

  /* initialise - put this before png_write_png() call */
  state.buffer = NULL;
  state.size = 0;

  w = Int_val(width);
  h = Int_val(height);

  if ((png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
                                         NULL, NULL, NULL)) == NULL ){
    failwith("png_create_write_struct");
  }

  if((info_ptr = png_create_info_struct(png_ptr)) == NULL ){
    png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
    failwith("png_create_info_struct");
  }

  /* error handling */
  if (setjmp(png_jmpbuf(png_ptr))) {
    /* Free all of the memory associated with the png_ptr and info_ptr */
    png_destroy_write_struct(&png_ptr, &info_ptr);
    /* If we get here, we had a problem writing the file */
    failwith("png write error");
  }

  /* the final arg is NULL because we dont need in flush() */
  png_set_write_fn(png_ptr, &state, png_write_data_to_buffer, NULL);

  /* we use system default compression */
  /* png_set_filter(png_ptr, 0, PNG_FILTER_NONE |
     PNG_FILTER_SUB | PNG_FILTER_PAETH ); */
  /* png_set_compression...() */

  png_set_IHDR(png_ptr, info_ptr, w, h,
                8 /* fixed */,
                PNG_COLOR_TYPE_PALETTE, /* fixed */
                PNG_INTERLACE_ADAM7,
                PNG_COMPRESSION_TYPE_DEFAULT,
                PNG_FILTER_TYPE_DEFAULT );

  {
    png_colorp palette;
    int num_palette;

    PngPalette_val(cmap, &palette, &num_palette );

    if(num_palette <= 0 ){
      png_destroy_write_struct(&png_ptr, &info_ptr);
      failwith("png write error (null colormap)");
    }
    png_set_PLTE(png_ptr, info_ptr, palette, num_palette );
  }

  /* infos... */

  png_write_info(png_ptr, info_ptr);

  {
    int rowbytes, i;
    png_bytep *row_pointers;
    char *buf = String_val(buffer);

    row_pointers = (png_bytep*)stat_alloc(sizeof(png_bytep) * h);

    rowbytes= png_get_rowbytes(png_ptr, info_ptr);
#if 0
    printf("rowbytes= %d width=%d\n", rowbytes, w);
#endif

    if(rowbytes != w && rowbytes != w * 2){
      png_destroy_write_struct(&png_ptr, &info_ptr);
      failwith("png write error (illegal byte/pixel)");
    }
    for(i=0; i< h; i++){
      row_pointers[i] = (png_bytep)(buf + rowbytes * i);
    }

    png_write_image(png_ptr, row_pointers);
    stat_free((void*)row_pointers);
  }

  png_write_end(png_ptr, info_ptr);
  png_destroy_write_struct(&png_ptr, &info_ptr);

  vres = caml_alloc_string(state.size);
  memcpy(String_val(vres), state.buffer, state.size);
  free(state.buffer);
  CAMLreturn(vres);
}

CAMLprim value write_png_file_index(value fd, value buffer, value cmap, 
                                    value width, value height) {
  CAMLparam5(fd, buffer, cmap, width, height);

  FILE *fp;
  png_structp png_ptr;
  png_infop info_ptr;

  int w, h;

  w = Int_val(width);
  h = Int_val(height);

  if ((fp = fdopen(Int_val(fd), "wb")) == NULL ){
    failwith("png file open failed");
  }

  if ((png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
                                        NULL, NULL, NULL)) == NULL ){
    fclose(fp);
    failwith("png_create_write_struct");
  }

  if((info_ptr = png_create_info_struct(png_ptr)) == NULL ){
    fclose(fp);
    png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
    failwith("png_create_info_struct");
  }

  /* error handling */
  if (setjmp(png_jmpbuf(png_ptr))) {
    /* Free all of the memory associated with the png_ptr and info_ptr */
    png_destroy_write_struct(&png_ptr, &info_ptr);
    fclose(fp);
    /* If we get here, we had a problem writing the file */
    failwith("png write error");
  }

  /* use standard C stream */
  png_init_io(png_ptr, fp);

  /* we use system default compression */
  /* png_set_filter(png_ptr, 0, PNG_FILTER_NONE |
     PNG_FILTER_SUB | PNG_FILTER_PAETH ); */
  /* png_set_compression...() */

  png_set_IHDR(png_ptr, info_ptr, w, h,
                8 /* fixed */,
                PNG_COLOR_TYPE_PALETTE, /* fixed */
                PNG_INTERLACE_ADAM7,
                PNG_COMPRESSION_TYPE_DEFAULT,
                PNG_FILTER_TYPE_DEFAULT );

  {
    png_colorp palette;
    int num_palette;

    PngPalette_val(cmap, &palette, &num_palette );

    if(num_palette <= 0 ){
      png_destroy_write_struct(&png_ptr, &info_ptr);
      fclose(fp);
      /* If we get here, we had a problem writing the file */
      failwith("png write error (null colormap)");
    }
    png_set_PLTE(png_ptr, info_ptr, palette, num_palette );
  }

  /* infos... */

  png_write_info(png_ptr, info_ptr);

  {
    int rowbytes, i;
    png_bytep *row_pointers;
    char *buf = String_val(buffer);

    row_pointers = (png_bytep*)stat_alloc(sizeof(png_bytep) * h);

    rowbytes= png_get_rowbytes(png_ptr, info_ptr);
#if 0
    printf("rowbytes= %d width=%d\n", rowbytes, w);
#endif

    if(rowbytes != w && rowbytes != w * 2){
      png_destroy_write_struct(&png_ptr, &info_ptr);
      fclose(fp);
      /* If we get here, we had a problem writing the file */
      failwith("png write error (illegal byte/pixel)");
    }
    for(i=0; i< h; i++){
      row_pointers[i] = (png_bytep)(buf + rowbytes * i);
    }

    png_write_image(png_ptr, row_pointers);
    stat_free((void*)row_pointers);
  }

  png_write_end(png_ptr, info_ptr);
  png_destroy_write_struct(&png_ptr, &info_ptr);

  fclose(fp);

  CAMLreturn(Val_unit);
}
