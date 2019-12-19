-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/apps/odl/odl-parser.ads,v $
--| Description     : Scanner for the embedded SQL translator                --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 22-Dec-2000                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/07/20 12:18:49 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2000-2002 Michael Erdmann                                  --
--                                                                           --
--  ODB is free software;  you can redistribute it  and/or modify it under   --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  ODB is distributed in the hope that it will be useful, but WITH-  --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with ODB;  see file COPYING.  If not, write  --
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
--  GNADE is implemented to work with GNAT, the GNU Ada compiler.            --
--                                                                           --
--  Functional Description                                                   --
--  ======================                                                   --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--                                                                           --
-------------------------------------------------------------------------------
with ODL.Scanner;
use  ODL;

package ODL.Parser is

   type ODL_Reader is private;

   Syntax_Exception      : exception ;
   End_Of_File_Exception : exception ;

   function New_ODL_Parser(
      F : in Scanner.File_Reader ) return ODL_Reader;

   procedure Parser_Input( 
      E : in out ODL_Reader;
      F : in Scanner.File_Reader );

   procedure ODL_Statement(
      e : in out ODL_Reader );

   procedure ODL_Prelude( 
      E : in out ODL_Reader );

   function Number_Of_Errors(
      E : ODL_Reader ) return Natural;

   function Number_Of_Warnings(
      E : ODL_Reader ) return Natural;

   -------------------------------------------------------------------------
private
   type ODL_Reader_Type;
   type ODL_Reader is access ODL_Reader_Type;

end ODL.Parser;
