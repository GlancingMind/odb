-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/apps/odl/odl-options.ads,v $
--  Description     : Main driver for the embedded SQL translator
--  Author          : Michael Erdmann                                        --
--  Created         : 22.12.2000                                             --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/06/26 19:37:31 $
--  Status          : $State: Exp $
--                                                                           --
--  Copyright (C) 2002 Michael Erdmann                                       --
--                                                                           --
--  GNADE is free software;  you can redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with GNAT;  see file COPYING.  If not, write --
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
--  MA 02111-1307, USA.                                                      --
--                                                                           --
--  As a special exception,  if other files  instantiate  generics from this --
--  unit, or you link  this unit with other files  to produce an executable, --
--  this  unit  does not  by itself cause  the resulting  executable  to  be --
--  covered  by the  GNU  General  Public  License.  This exception does not --
--  however invalidate  any other reasons why  the executable file  might be --
--  covered by the  GNU Public License.                                      --
--                                                                           --
--  This software is implemented to work with GNAT, the GNU Ada compiler.    --
--                                                                           --
--  Functional Description                                                   --
--  ======================                                                   --
--  This package contains the Option flags for the esql translator           --
--                                                                           --
--  Contact                                                                  --
--  =======                                                                  --
--  Error reports shall be handled via http://gnade.sourceforge.net          --
--  Features and ideas via: gnade-develop@lists.sourceforge.net              --
--                                                                           --
--  Author contact:                                                          --
--               purl:/net/michael.erdmann                                   --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;                       use Ada.Text_IO;

package ODL.Options is

   -- print out the activities
   Option_Verbose         : Boolean := False;

   -- if true, no copyright notices etc are printed out
   Option_SILENT          : Boolean := False;

   -- this forces the checks to be pedantic, which means deviations from
   -- ISO are indicated a warnings.
   Option_Pedantic        : Boolean := False;

   -- This will generate additional in line debug code
   Option_Debug_Code      : Boolean := False;

   -- this is the max. number of error before the processing stops.
   Option_Error_Limit     : Natural := 200;

   -- Debugging output
   Option_DEBUG           : Boolean := False;

   -- schema file
   Option_SCHEMA          : Boolean := False;
   Schema_File            : File_Type;

   -- GNAT specific swithches
   Option_GNAT_Sref       : Boolean := True;

end ODL.Options;


