## ----------------------------------------------------------------------------
##                                                                           --
##  Filename        : $Source: /cvsroot/gnade/odb/src/Makefile,v $
##  Description     : Makefile for the src library                           --
##  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
##  Created On      : 1-May-2002                                             --
##  Last Modified By: $Author: merdmann $                                    --
##  Last Modified On: $Date: 2003/07/20 14:42:33 $                           --
##  Version         : $Revision: 1.4 $				     --
##  Status          : $State: Exp $					     --
##                                                                           --
##  Copyright (C) 2003 Michael Erdmann                                       --
##                                                                           --
##  ABE is copyrighted by the persons and institutions enumerated in the     --
##  AUTHORS file. This file is located in the root directory of the          --
##  ABE distribution.                                                        --
##                                                                           --
##  ABE is free software; you can redistribute it and/or modify it under     --
##  terms of the  GNU General Public License as published  by the Free Soft- --
##  ware  Foundation;  either version 2,  or (at your option) any later ver- --
##  sion. ABE is distributed in the hope that it will be useful, but WITH-   --
##  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
##  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
##  for  more details.  You should have  received  a copy of the GNU General --
##  Public License  distributed with GNAT;  see file COPYING.  If not, write --
##  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
##  MA 02111-1307, USA.                                                      --
##                                                                           --
##  As a special exception,  if other files  instantiate  generics from      --
##  ABE Ada units, or you link ABE Ada units or libraries with other         --
##  files  to produce an executable, these  units or libraries do not by     --
##  itself cause the resulting  executable  to  be covered  by the  GNU      --
##  General  Public  License.  This exception does not however invalidate    --
##  any other reasons why  the executable file  might be covered by the      --
##  GNU Public License.                                                      --
##                                                                           --
##  Contact                                                                  --
##  =======                                                                  --
##  Error reports shall be handled via http://ascl.sourceforge.net           --
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
sinclude ./Makefile.local

##
## Build the target library
##
all: $(libdir)/$(LIBNAME_PKG).so.$(VERSION)

##
## Build the actual library containing all components given by the 
## variable COMPONENTS. This variable is expected to be set in 
## Makefile.local.
##
$(libdir)/$(LIBNAME_PKG).so.$(VERSION): $(COMPONENTS)
	$(MAKELIB) --cflags="-gnatf" --lib=$(LIBNAME_PKG) $(COMPONENTS)

##
## Build a manifest
##
manifest:
	$(DOMANIFEST) $(UNIT) MANIFEST.local >> $(MANIFEST)

##
## Create a distribution package
##
dist:: all

##
##
##
install:

##
## Cleanup completly
##	
distclean: clean



clean:
	rm -rf b~*.ad? *~ *~*~ *.bak 
	rm -rf *.lo *.o .libs *.ali *.la
