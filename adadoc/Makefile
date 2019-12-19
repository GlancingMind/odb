## ----------------------------------------------------------------------------
##                                                                           --
##  Filename        : $Source: /cvsroot/gnade/odb/adadoc/Makefile,v $
##  Description     : Makefile for ASCL Library                              --
##  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
##  Created On      : 1-May-2002                                             --
##  Last Modified By: $Author: merdmann $                                    --
##  Last Modified On: $Date: 2003/06/23 20:03:18 $                           --
##  Version         : $Revision: 1.2 $
##  Status          : $State: Exp $
##                                                                           --
##  Copyright (C) 2002                                                       --
##                                                                           --
##  ASCL is copyrighted by the persons and institutions enumerated in the    --
##  AUTHORS file. This file is located in the root directory of the          --
##  ASCL distribution.                                                       --
##                                                                           --
##  ASCL is free software;  you can redistribute it  and/or modify it under  --
##  terms of the  GNU General Public License as published  by the Free Soft- --
##  ware  Foundation;  either version 2,  or (at your option) any later ver- --
##  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
##  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
##  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
##  for  more details.  You should have  received  a copy of the GNU General --
##  Public License  distributed with GNAT;  see file COPYING.  If not, write --
##  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
##  MA 02111-1307, USA.                                                      --
##                                                                           --
##  As a special exception,  if other files  instantiate  generics from      --
##  ASCL Ada units, or you link ASCL Ada units or libraries with other       --
##  files  to produce an executable, these  units or libraries do not by     --
##  itself cause the resulting  executable  to  be covered  by the  GNU      --
##  General  Public  License.  This exception does not however invalidate    --
##  any other reasons why  the executable file  might be covered by the      --
##  GNU Public License.                                                      --
##                                                                           --
##  This package is  implemented to work with GNAT, the GNU Ada compiler.    --
##                                                                           --
##  Contact                                                                  --
##  =======                                                                  --
##  Error reports shall be handled via http://gnade.sourceforge.net          --
##  Features and ideas via: gnade-develop@lists.sourceforge.net              --
##                                                                           --
##  Author contact:                                                          --
##               purl:/net/michael.erdmann                                   --
##                                                                           --
## ----------------------------------------------------------------------------
##
##  Functional Description
##  ======================
##
##  Restrictions
##  ============
##  None
##
##  References
##  ==========
##  None
##
## 
##  
include ../make.conf

ADADOC=$(bindir)/adadoc

##
## Build all
##
all:	adadoc.adb text.ads text.adb
	gnatmake -g adadoc
	cp adadoc $(toolsdir)

##
## Create the manifest file 
##
manifest:
	$(DOMANIFEST) adadoc MANIFEST.local >> $(MANIFEST)

##
## Create a distribution
##
dist:

##
## Make install actions
##
install:


##
## Test targets
##
test: t.ps

t.sgml: example.ads example_rstyle.ads
	$(ADADOC) -c "Normal Style" -o t1 $(F) example.ads
	$(ADADOC) -r -c "Reverse Style " -o t2 $(F) example_rstyle.ads

test.ps: test.sgml t.sgml 
	jade -t tex -d /usr/lib/sgml/stylesheets/nwalsh-modular/print/docbook.dsl \
	test.sgml
	jadetex  test.tex
	jadetex  test.tex
	jadetex  test.tex
	dvips -o test.ps test.dvi
##
## Clean up the local directory
##
clean distclean:
	rm -f *.o *.ali core b~*.ad? *~ *.bak log adadoc test.ps t.sgml
	rm -f *.dvi *.aux *.log *.tex *.ps t1.sgml t2.sgml
	rm -f $(toolsdir)/adadoc
