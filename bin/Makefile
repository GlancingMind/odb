## ----------------------------------------------------------------------------
##                                                                           --
##  Filename        : $Source: /cvsroot/gnade/odb/bin/Makefile,v $
##  Description     : Makefile all local tools                               --
##  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
##  Created On      : 20-Apr-2002                                            --
##  Last Modified By: $Author: merdmann $                                    --
##  Last Modified On: $Date: 2003/10/14 19:16:02 $                           --
##  Version         : $Revision: 1.3 $		                             --
##  Status          : $State: Exp $					     --
##                                                                           --
##  Copyright (C) 2002                                                       --
##                                                                           --
##  ABE    is copyrighted by the persons and institutions enumerated in the    --
##  AUTHORS file. This file is located in the root directory of the          --
##  ABE    distribution.                                                       --
##                                                                           --
##  ABE    is free software;  you can redistribute it  and/or modify it under  --
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
##  ABE    Ada units, or you link ABE    Ada units or libraries with other       --
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

##
## Build all
##
all: 
	chmod +x *.sh odb-config
##
##  Install all components in the local environment
##
install:
	cp odb-config  $(bindir)
##
## Create a distribution package
##
dist:

##
## Clean up the local directory
##
clean :
	rm -rf *.bak *~ *~*~


distclean: clean
	rm -rf adacompiler.sh vars.sh docbuilder.sh makelib.sh domanifest.sh docbuild.sh odb-config
