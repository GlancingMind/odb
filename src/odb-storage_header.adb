-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-storage_header.adb,v $
--  Version         : $Revision: 1.5 $                                       --
--  Description     : A collection of persistent objects                     --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 30-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/10/08 04:43:55 $                           --
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
--  This package contains methods to handle the so called storage header.    --
--                                                                           --
--  header     ::= <header> attribute* </header>                             --
--  attribute  ::= <attribite name= offset= />				     --
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
with Ada.Streams.Stream_IO;                     use Ada.Streams.Stream_IO;
with Ada.Streams;                               use Ada.Streams;
with Ada.Characters.Latin_1;			use Ada.Characters.Latin_1;
with Ada.Strings;				use Ada.Strings;
with Ada.Strings.Fixed;				use Ada.Strings.Fixed;
with Ada.Text_IO;
use  Ada;

with ODB.Classes;				use ODB.Classes;
use  ODB;

with Util.String_Array;				use Util.String_Array;
with Util.Hash_Table;
use  Util;

package body ODB.Storage_Header is

   Version : constant String := "$Id: odb-storage_header.adb,v 1.5 2003/10/08 04:43:55 merdmann Exp $";

   package Attrib_Hash is new Hash_Table( Key_Type => Unbounded_String );

   type Offset_Array is array( Natural Range <> ) of Natural;

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type is record
           Class_Name : Unbounded_String := Null_Unbounded_String;
           Hash       : Attrib_Hash.Table_Type( 200 );
	   Offset     : Offset_Array( 1..200 );
   	end record;

   ----------------
   -- Initialize --
   ----------------
   function Initialize return Object_Data_Access is 
      Result : Object_Data_Access := new Object_Data_Type;
   begin
      return Result;
   end Initialize;

   ------------------------
   -- Register_Attribute --
   ------------------------
   procedure Register_Attribute(
      This   : in out Object;
      Name   : in Unbounded_String;
      Offset : in Natural ) is
      -- register an attribute
      Data   : Object_Data_Access renames This.Data;
      H      : Natural := 0;

      use Attrib_Hash;
   begin
      Put( Data.Hash, Name, H );
      Data.Offset(H) := Offset;
   end Register_Attribute;

   ------------------------
   -- Register_Attribute --
   ------------------------
   procedure Register_Attribute(
      This   : in out Object;
      Id     : in Natural;
      Offset : in Natural;
      Cls    : in Tag ) is
   begin
      Register_Attribute( This, 
         To_Unbounded_String(Classes.Attribute( Cls, Id ) ),
	 Offset );
   end Register_Attribute;

   ----------------------
   -- Lookup_Attribute --
   ----------------------
   function Lookup_Attribute(
      This   : in Object;
      Name   : in String ) return Natural is
      Data   : Object_Data_Access renames This.Data;
      use Attrib_Hash;

      H      : Natural := Get( Data.Hash, To_Unbounded_String(Name) );
   begin
      return Data.Offset(H);
   end Lookup_Attribute;

   ----------------
   -- Attributes --
   ----------------
   function Attributes(
      This   : in Object ) return String_Array.Handle is
      -- return an array with the attribute names in the storage header.
      Data   : Object_Data_Access renames This.Data;
      use Attrib_Hash;

      Keys   : Dictionary_Table( 1..200 );
      Length : Natural             := 0;
      Result : String_Array.Handle := null;
   begin
      Dictionary(  Data.Hash, Keys, Length );

      Result := new String_Array.Object( 1..Length );

      for i in 1..Length loop
         Result(i) := Keys(i);
      end loop;

      return Result;
   end Attributes;   

   -------------
   -- Destroy --
   -------------
   procedure Clear(
      This   : in out Object ) is
      Data   : Object_Data_Access renames This.Data;

      use Attrib_Hash;
   begin
      Clear( Data.Hash );
   end Clear;

   ----------------
   -- Class_Name --
   ----------------
   function Class_Name(
      This   : in Object ) return String is 
      Data   : Object_Data_Access renames This.Data;
   begin

      return To_String( Data.Class_Name );
   end Class_Name;

   ----------------
   -- Class_Name --
   ----------------
   procedure Class_Name(
      This   : in out Object;
      Value  : in String ) is 
      Data   : Object_Data_Access renames This.Data;
   begin
      Data.Class_Name := To_Unbounded_String( Value );
   end Class_Name;

end ODB.Storage_Header;
