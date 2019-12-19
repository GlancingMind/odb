## ----------------------------------------------------------------------------
##                                                                           --
##  Filename        : $Source: /cvsroot/gnade/odb/apps/odl/Makefile,v $      --
##  Description     : Makefile for the ODL Translator			     --
##  Author          : Michael Erdmann                                        --
##  Created On      : 27-06-2003					     --
##  Last Modified By: $Author: merdmann $				     --
##  Last Modified On: $Date: 2003/10/12 14:05:05 $			     --
##  Status          : $State: Exp $					     --
##									     --
##  Copyright (C) 2003 Michael Erdmann					     --
##									     --
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
##  ODB Ada units, or you link GNADE Ada units or libraries with other       --
##  files  to produce an executable, these  units or libraries do not by     --
##  itself cause the resulting  executable  to  be covered  by the  GNU      --
##  General  Public  License.  This exception does not however invalidate    --
##  any other reasons why  the executable file  might be covered by the      --
##  GNU Public License.                                                      --
##                                                                           --
##  ODB is implemented to work with GNAT, the GNU Ada compiler.              --
##                                                                           --
## ----------------------------------------------------------------------------
##
##  Functional Description
##  ======================
##  This makefile builds the esql translator and exports the support components
##  for the esql translator.
##
##  make       - build all and copy the binary into the $(bindir) after 
##		 the tests have been compiled correctly.
##
##  test_target- Compile the test targets
##
##  Restrictions
##  ============
##  None
##
##  References
##  ==========
##  None
##
include ../../make.conf

odl=$(bindir)/odl
## ------------------------------------------------------------------------- #
##                        E X E C U T A B L E S		                     #
## ------------------------------------------------------------------------- #
all :: odl test_targets
	cp odl $(odl)

##
## Build the ODL translator.
##
main.adb: main.gpb ../../VERSION 
	$(ADAPREP) -DVersion=\"$(VERSION)\" main.gpb main.adb

odl: 	main.adb	 				\
	odl-scanner.adb odl-scanner.ads 		\
	odl-driver.adb  odl-driver.ads 			\
	odl-parser.ads  odl-parser.adb 			\
	odl-options.ads	
	$(ADAMAKE) main.adb -o $@ $(CFLAGS) -largs $(LDFLAGS) 

##
## Test targets
##
test_targets:	test.o

test.o: test.ods test.odb
	./odl test
	$(ADAMAKE) $(CFLAGS) -c test.adb

##
##
##
install ::

##
## 
##
manifest ::
	$(DOMANIFEST) $(UNIT) MANIFEST.local >> $(MANIFEST)

##
## create a distribution
##
dist: all

##
## clean up
##
clean:
	rm -rf  b~*.* *.o *.ali core *~ *~*~
	rm -f main.adb odl 
	rm -f test.adb test.ads test.o

##
## clean up completly
##
distclean :: clean
	rm -f $(bindir)/odl
