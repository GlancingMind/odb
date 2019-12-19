-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/util/util.ads,v $
--  Version         : $Revision: 1.2 $                                       --
--  Description     : Map Strings to values of any type                      --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 21-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/06/27 15:09:41 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2003 Michael Erdmann                                       --
--                                                                           --
--  ODB is free software;  you can redistribute it  and/or modify it under   --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  ODB is distributed in the hope that it will be useful, but WITH-  --
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
--  Functional Description                                                   --
--  ======================                                                   --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Util.Hash_Table;

generic 

   type Value_Type is private;

package Util.String_Map is

   type Object( Size : Natural ) is private;

   ---------------------------------------------------------------------------
   -- Description:
   --    Add an Key/Value pair to the string map
   -- Preconditions:
   --    C.1 - The key is not already in the table
   -- Postconditions:
   --    The Value is stored in the map and may be retrieved by means of the
   --    Get method.
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Put(
      This  : in out Object;
      Key   : in String;
      Value : in Value_Type ); 


   function Get(
      This : in Object;
      Key  : in String ) return Value_Type;


   function Is_Empty(
      This : in Object ) return Boolean;

   function Key(
      This : in Object;
      H    : in Natural ) return String;

private

   package String_Hash is new Util.Hash_Table(  Key_Type => Unbounded_String );
   use String_Hash;

   type Value_Array is array( Natural Range <> ) of Value_Type;

   type Object( Size : Natural ) is record
         Hash  : String_Hash.Table_Type( Size );
	 Value : Value_Array( 1..Size );
      end record;

end Util.String_Map;
