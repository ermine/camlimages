#*********************************************************************#
#                                                                     #
#                          Caml Images                                #
#                                                                     #
#            Fran�ois Pessaux, projet Cristal, INRIA Rocquencourt     #
#            Pierre Weis, projet Cristal, INRIA Rocquencourt          #
#            Jun Furuse, projet Cristal, INRIA Rocquencourt           #
#                                                                     #
#  Copyright 1999-2004,                                               #
#  Institut National de Recherche en Informatique et en Automatique.  #
#  Distributed only by permission.                                    #
#                                                                     #
#*********************************************************************#

#(* $Id: Makefile.am,v 1.7 2008-02-17 23:24:16 furuse Exp $ *)

OCAMLSOURCES = resize.ml

noinst_PROGRAMS =
if BUILD_NATIVE
noinst_PROGRAMS += resize.opt
endif
if BUILD_BYTECODE
noinst_PROGRAMS += resize.byt
endif

resize_opt_SOURCES = $(OCAMLSOURCES)
resize_byt_SOURCES = $(OCAMLSOURCES)

include ../../Makefile.variables

resize.byt$(EXEEXT): $(OCAMLBYTOBJS)
	$(OCAMLC) -o resize.byt$(EXEEXT) \
		$(DLLPATHS) $(LINKFLAGS) $(AM_LINKFLAGS) \
		$(OCAMLLIBS_CORE) $(OCAMLBYTOBJS)

resize.opt$(EXEEXT): $(OCAMLOPTOBJS)
	$(OCAMLOPT) -o resize.opt$(EXEEXT) \
		$(LINKFLAGS) $(AM_LINKFLAGS) \
		$(OCAMLLIBS_CORE:.cma=.cmxa) $(OCAMLOPTOBJS)

include ../../Makefile.rules
