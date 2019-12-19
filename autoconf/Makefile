## ----------------------------------------------------------------------------
##                                                                           --
##                                                                           --
##  Filename        : $Source: /cvsroot/gnade/odb/autoconf/Makefile,v $
##  Description     : Makefile to generate the config script 
##  Author          : Michael Erdmann <Michael.Erdmann@snafu.de> 
##  Created On      : 03-Jan-2001
##  Last Modified By: $Author: merdmann $
##  Last Modified On: $Date: 2003/06/23 07:36:53 $
##  Status          : $State: Exp $
##                                                                           --
##  Copyright (C) 2003                                                       --
##                                                                           --
##  ABE    is copyrighted by the persons and institutions enumerated in the   --
##  AUTHORS file. This file is located in the root directory of the          --
##  ABE    distribution.                                                      --
##                                                                           --
##  ABE    is free software;  you can redistribute it  and/or modify it under --
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
##  ABE    Ada units, or you link ABE    Ada units or libraries with other     --
##  files  to produce an executable, these  units or libraries do not by     --
##  itself cause the resulting  executable  to  be covered  by the  GNU      --
##  General  Public  License.  This exception does not however invalidate    --
##  any other reasons why  the executable file  might be covered by the      --
##  GNU Public License.                                                      --
##                                                                           --
##  ABE    is implemented to work with GNAT, the GNU Ada compiler.            --
##                                                                           --
## ----------------------------------------------------------------------------
##
##  Functional Description
##  ======================
##  This makefile rebuild all configure script used to configure the 
##  ABE    package.
##
##  Restrictions
##  ============
##  None
##
##  References
##  ==========
##  None
##
BASEDIR=../
all: configure 

configure: 
	autoconf -f $(BASEDIR)/base/configure.in > configure
	chmod +x configure
	rm -rf config.cache config.log  config.status 

clean distclean :
	rm -rf config.cache config.log  config.status make.conf *~ configure configure.in
	rm -rf autom*.cache


