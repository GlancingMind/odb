## ----------------------------------------------------------------------------
##                                                                           --
##  Filename        : $Source: /cvsroot/gnade/odb/samples/pool/Makefile,v $
##  Description     : Makefile a demonstration sample                        --
##  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
##  Created On      : 20-Jun-2003                                            --
##  Last Modified By: $Author: merdmann $                                    --
##  Last Modified On: $Date: 2003/07/12 19:07:56 $                           --
##  Version         : $Revision: 1.3 $		                     --                       --
##  Status          : $State: Exp $					     --
##                                                                           --
##  Copyright (C) 2003 Michael Erdmann                                       --
##                                                                           --
##  ODB is copyrighted by the persons and institutions enumerated in the     --
##  AUTHORS file. This file is located in the root directory of the          --
##  ODB distribution.                                                        --
##                                                                           --
##  ODB is free software;  you can redistribute it  and/or modify it under   --
##  terms of the  GNU General Public License as published  by the Free Soft- --
##  ware  Foundation;  either version 2,  or (at your option) any later ver- --
##  sion.  ODB is distributed in the hope that it will be useful, but WITH-  --
##  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
##  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
##  for  more details.  You should have  received  a copy of the GNU General --
##  Public License  distributed with ODB;  see file COPYING.  If not, write  --
##  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
##  MA 02111-1307, USA.                                                      --
##                                                                           --
##  As a special exception,  if other files  instantiate  generics from      --
##  ODB Ada units, or you link DBAda Ada units or libraries with other       --
##  files  to produce an executable, these  units or libraries do not by     --
##  itself cause the resulting  executable  to  be covered  by the  GNU      --
##  General  Public  License.  This exception does not however invalidate    --
##  any other reasons why  the executable file  might be covered by the      --
##  GNU Public License.                                                      --
##                                                                           --
##  This package is implemented to work with GNAT, the GNU Ada compiler.     --
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
include ../../make.conf

odl=$(bindir)/odl
##
## Build all
##
all: pool

##
## simulation of a particle grid
##
pool: main.adb person.o
	gnatmake -I../ main.adb -o pool $(CFLAGS) -largs $(LDFLAGS) 
	if test ! -d data ; then mkdir data ; fi

person.o: person.ods person.odb
	$(odl) -gnatnosref person
	$(ADAMAKE) $(CFLAGS) -c person.adb
##
##
##
manifest:
	$(DOMANIFEST) $(UNIT) MANIFEST.local >> $(MANIFEST)
##
##
##
install:

##
##
##
dist:

##
## Clean up the local directory
##
clean :
	rm -rf *.bak *~ *~*~ 
	rm -rf *.o *.ali 
	rm -rf pool person.pool person.adb person.ads data
##
##
##
distclean: clean
	rm -rf .libs

