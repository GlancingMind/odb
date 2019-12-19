## ----------------------------------------------------------------------------
##                                                                           --
##  Filename        : $Source: /cvsroot/gnade/odb/samples/dungon/Makefile,v $
##  Description     : Makefile a demonstration sample                        --
##  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
##  Created On      : 20-Jun-2003                                            --
##  Last Modified By: $Author: merdmann $                                    --
##  Last Modified On: $Date: 2003/10/06 04:27:57 $                           --
##  Version         : $Revision: 1.5 $		                     --                       --
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

LOCALS=-gnatf
odl=$(bindir)/odl
##
## Build all
##
all: dungon


##
## simulation of a particle grid
##
dungon: main.adb game.o room.o player.o
	gnatmake -I../ main.adb -o dungon $(CFLAGS) -largs $(LDFLAGS) $(LDSUPPORT)
	if test ! -d data ; then mkdir data; fi

room.o: room.ods room.odb
	$(odl) room
	$(ADAMAKE) $(LOCALS) $(CFLAGS) -c room.adb

player.o: player.ods player.odb
	$(odl) player
	$(ADAMAKE) $(LOCALS) $(CFLAGS) -c player.adb

game.o: game.ods game.odb player.o room.o
	$(odl) game
	$(ADAMAKE) $(LOCALS) $(CFLAGS) -c game.adb

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
	rm -rf dungon room.adb room.ads \
	   player.ads player.adb game.ads game.adb 
	rm -rf *.pool data
##
##
##
distclean: clean
	rm -rf .lib
