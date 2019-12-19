## ----------------------------------------------------------------------------
##                                                                           --
##  Filename        : $Source: /cvsroot/gnade/odb/rpm/Makefile,v $
##  Description     : Build RPMS from the distribution                       --
##  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
##  Created On      : 14-Oct-2003                                            --
##  Last Modified By: $Author: merdmann $                                    --
##  Last Modified On: $Date: 2003/10/16 19:24:35 $                           --
##  Version         : $Revision: 1.2 $
##  Status          : $State: Exp $
##                                                                           --
##  Copyright (C) 2003 M.Erdmann                                             --
##                                                                           --
##  ODB is copyrighted by the persons and institutions enumerated in the     --
##  AUTHORS file. This file is located in the root directory of the          --
##  GNADE distribution.                                                      --
##                                                                           --
##  ODB is free software;  you can redistribute it  and/or modify it under   --
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
##  ODB Ada units, or you link ODB Ada units or libraries with other         --
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

VERS=$(MAJOR).$(MINOR).$(PATCHLEVEL)
DIST=../odb-src-$(VERS).tar.gz
STAGE=a
##
## Where the RPM is loacted on your system. You may have to adopt this 
## to your system.
##
RPM=/usr/src/packages

RPMSRC=$(RPM)/SOURCES/odb.tar.gz
RPMS=$(RPM)/RPMS/$(CPU)
SRPMS=$(RPM)/SRPMS

M4FLAGS= --define=VERSION=$(VERS) --define=BUILD=`cat .version` 
##
## Per default do nothing
##
all:

##
## In case of a distribution we build the list of files to be included.
##
dist: 

##
## Build the rpms
##
rpm:	$(RPMSRC)
	m4 $(M4FLAGS) odb.m4 > odb.spec
	( export PATH=/usr/local/bin:/usr/gnat/bin:$$PATH && \
		 rpm -b$(STAGE) --target=$(CPU)-$(OS) odb.spec ) >build.log 2>>build.error
	mv $(RPMS)/odb-$(VERS)*.rpm .
	mv $(SRPMS)/odb-$(VERS)*.rpm .

$(RPMSRC): $(DIST) version
	cp $(DIST) $(RPMSRC)

$(RPMFILES):
	(cd .. &&  $(RPMLIST) )

##
## count the build number up each time we make an rpm
##
version:
	@if [ ! -f .version ] ; \
	   then echo 1 > .version ; \
	   else  expr 0`cat .version` + 1 > .version ; \
        fi
	@echo "RPM Build : " `cat .version` 
##
##
##
dep:

##
## Cleanup
##
clean:
	rm -rf *~ *~*~ odb.spec *.log	 build.error

distclean: clean
	rm -rf $(RPMFILES)
	rm -rf *.rpm
	rm .version

publish:
	ncftpput upload.sourceforge.net /incoming odb-$(VERS)-*.src.rpm
	ncftpput upload.sourceforge.net /incoming odb-$(VERS)-*.i686.rpm
