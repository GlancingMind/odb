## ----------------------------------------------------------------------------
##                                                                           --
##                      GNADE  : GNu Ada Database Environment                --
##                                                                           --
##  Filename        : $Source: /cvsroot/gnade/odb/doc/reference/Makefile,v $
##  Description     : Top level makefile for the documentation               --
##  Author          : Michael Erdmann <michael.erdmann@snafu.de>             --
##  Created On      : 03-Oct-2002
##  Last Modified By: $Author: merdmann $
##  Last Modified On: $Date: 2003/08/10 19:34:58 $                           --
##  Status          : $State: Exp $                                          --
##                                                                           --
##  Copyright (C) 2002                                                       --
##                                                                           --
##  GNADE is copyrighted by the persons and institutions enumerated in the   --
##  AUTHORS file. This file is located in the root directory of the          --
##  GNADE distribution.                                                      --
##                                                                           --
##  GNADE is free software;  you can redistribute it  and/or modify it under --
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
##  GNADE Ada units, or you link GNADE Ada units or libraries with other     --
##  files  to produce an executable, these  units or libraries do not by     --
##  itself cause the resulting  executable  to  be covered  by the  GNU      --
##  General  Public  License.  This exception does not however invalidate    --
##  any other reasons why  the executable file  might be covered by the      --
##  GNU Public License.                                                      --
##                                                                           --
##  GNADE is implemented to work with GNAT, the GNU Ada compiler.            --
##                                                                           --
## ----------------------------------------------------------------------------
##
##  Functional Description
##  ======================
##  This makefile builds the various documentation formats. The input 
##  is SGML docbook.
##
##  If the aspell utility is installed on the system, a gobal spell check
##  is done which summerizes all missspellings. If the number of 
##  errors is to large consider the execution of a real spellcheck 
##  run.
##
##  Restrictions
##  ============
##  - Spellcheck is based upon the aspell utility.
##
##  References
##  ==========
##  None
##
##
include ../../make.conf
sinclude ./Makefile.local

.SUFFIXES: .png .eps .gif

.png.eps:
	png2eps $*.png > $*.eps

.gif.eps:
	gif2eps -compress -dpi 120 $*.gif $*.eps
##
## Logfiles of the docbuild run  
##     
LOG=docbuild.log

##
## Basename of the document
##
DOCBASE=refguide

##
## Scale for the xfig -> esp/gif conversion
##
SCALE=0.8

##
## Document parts
##
PARTS=$(DOCBASE).sgml gpl.sgml gfdl.sgml faq.sgml \
	development.eps \
	deployment.eps \
	classes.eps \
	packages.eps \
	loading.eps \
	saving.eps \
	storage.eps


##
## Targets to be build
##
TARGETS= $(docdir)/html/$(DOCBASE).html   \
		$(docdir)/$(DOCBASE).ps   \
		$(docdir)/$(DOCBASE).pdf 
##
## Target for Dist. 
##
all ::  api.sgml $(TARGETS)

##
## Targets are the different file formats for the reference guide
##
$(docdir)/$(DOCBASE).pdf :: $(docdir)/$(DOCBASE).ps
#	$(DOCBUILDER) --pdf --log=$(LOG) --target=$@ $(DOCBASE) 
	ps2pdf $(docdir)/$(DOCBASE).ps $(docdir)/$(DOCBASE).pdf

$(docdir)/$(DOCBASE).ps :: $(DOCBASE).tex
	$(DOCBUILDER) --ps --log=$(LOG) --target=$@ $(DOCBASE) 

$(docdir)/html/$(DOCBASE).html :: $(DOCBASE).tex
	$(DOCBUILDER) --html --log=$(LOG) $(DOCBASE)
	rm -rf $(docdir)/html
	-mkdir -p $(docdir)/html
	-mv *.html *.htm $(docdir)/html
	-cp *.gif $(docdir)/html

$(DOCBASE).tex :: $(PARTS)
	$(DOCBUILDER) --tex $(DOCBASE).sgml

##
## generate the API doc into the file api.sgml
##
api.sgml: 
	$(ADADOC) -c "API References"  \
                     -o api $(SRCDIR)/*.ads $(UTILDIR)/*.ads 

##
## Build the directory for distribution. This target will also be 
## executed in the production of the target environment but the 
## result file shall never be mentioned in the MANIFEST.local.
##
dist ::	distclean
	tar cvf template.tar *.sgml
	gzip -f template.tar

##
##  generate the manifest file
##
manifest:
	$(DOMANIFEST) $(UNIT) MANIFEST.local >> $(MANIFEST)

##
## Install the distribution in the local environment.
##
install ::

##
##  clean up the directory
##
clean ::
	rm -f *.aux *.log *.tex *.dvi *.out *.pdf *.htm *.html *~ *.bak *.gz
	rm -f api.sgml
	rm -f *.jpg *.eps
	rm -f $(LOG)	
##
##
##
distclean :: clean
	rm -f *.dvi *.ps *.pdf *.rtf *.eps
