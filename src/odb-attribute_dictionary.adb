-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-attribute_dictionary.adb,v $
--  Description     : Attribute Table                                        --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 19-Aug-2002                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/10/08 04:43:55 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2000-2003 Michael Erdmann                                  --
--                                                                           --
--  XMLS  is free software;  you can redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion. XMLS is distributed in the hope that it will be useful, but WITH-  --
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
with Util.Hash_Table;
use  Util;

package body ODB.Attribute_Dictionary is

   Version : String := 
      "$Id: odb-attribute_dictionary.adb,v 1.1 2003/10/08 04:43:55 merdmann Exp $";


   package Attrib_Hash is new Hash_Table( Key_Type => Unbounded_String );
    
   type Value_Array is array( Natural Range <> ) of Unbounded_String;

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type( Size : Natural ) is record
      	 Hash  : Attrib_Hash.Table_Type( Size );
	 Value : Value_Array( 1..Size ) := (others => Null_Unbounded_String);
      end record;

   ----------------
   -- Initialize --
   ----------------
   function Initialize(
      Size   : in Natural ) return Object_Data_Access is 
      Result : Object_Data_Access := new Object_Data_Type(Size) ;
   begin
      return Result;
   end Initialize;

   ---------
   -- Add --
   ---------
   procedure Add( 
      This  : in out Object;
      Name  : in String;      
      Value : in String ) is
      use Attrib_Hash;

      Data  : Object_Data_Access renames This.Data;
      Hash  : Natural;
   begin
      Put( Data.Hash, To_Unbounded_String(Name), Hash );
      Data.Value( Hash ) := To_Unbounded_String(Value);
   end Add;

   ---------
   -- Get --
   ---------
   function Get(
      This  : in Object;
      Name  : in String ) return String is
      use Attrib_Hash;

      Data  : Object_Data_Access renames This.Data;
      Hash  : Natural := Get( Data.Hash, To_Unbounded_String(Name) );
   begin
      return To_String( Data.Value( Hash ) );
   end Get;

   -----------
   -- Clear --
   -----------
   procedure Clear(
      This  : in out Object ) is
      Data  : Object_Data_Access renames This.Data;
      use Attrib_Hash;
   begin
      Clear( Data.Hash );
   end Clear;

end ODB.Attribute_Dictionary;
