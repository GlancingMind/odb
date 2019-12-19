-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-classes.adb,v $
--  Version         : $Revision: 1.4 $                                       --
--  Description     : Definition of a persitent object                       --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 21-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/10/11 19:26:32 $                           --
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
--  R.1 -                                                                    --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Text_IO;				use Ada.Text_IO;
with Ada.Tags;					use Ada.Tags;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Exceptions;           			use Ada.Exceptions;
use  Ada;

with Util.List;
with Util.Hash_Table;
with Util.String_Map;
use  Util;

with ODB;					use ODB;

package body ODB.Classes is

   Version : constant String := 
      "$Id: odb-classes.adb,v 1.4 2003/10/11 19:26:32 merdmann Exp $";

   type Attribute_Array is array( Positive Range <> ) of Unbounded_String;

   --------------------------
   -- Factory_Mapping_Type --
   --------------------------
   type Factory_Mapping_Type is record
         Name       : Tag;
	 Creator    : Factory_Access := null;
	 Attributes : Attribute_Array(1..100) := (others => Null_Unbounded_String);
      end record;

   type Factory_Mapping_Access is access Factory_Mapping_Type;

   package Factory_Hash is new Hash_Table( Key_Type => Tag );

   Factory_Table_Hash : Factory_Hash.Table_Type( 1000 );
   Factory_Table : array( 1..1000 ) of Factory_Mapping_Access ;

   --------------
   -- Class_ID --
   --------------
   function Class_ID(
      Name : in Tag ) return Natural is 

      use Factory_Hash;
   begin
      return Get( Factory_Table_Hash, Name ); 
      
   exception
      when FACTORY_HASH.KEY_NOT_EXISTING =>
         return 0;
   end Class_ID;

   ----------------------
   -- Register_Factory --
   ----------------------
   function Register_Factory(
      Name    : in Tag;
      Creator : in Factory_Access ) return Natural is 
      -- register a factory function together with a class name
      Mapping : Factory_Mapping_Access := new Factory_Mapping_Type;
      Result  : Natural := 0;     

      use Factory_Hash;

   begin  	    
      if Class_ID( Name ) /= 0 then 
         Raise_Exception(Factory_Not_Found'Identity, External_Tag(Name) );
      end if;

      Mapping.Name    := Name;
      Mapping.Creator := Creator;

      Put( Factory_Table_Hash, Name, Result );
      Factory_Table( Result ) := Mapping;

      return Result;
   end Register_Factory;

   -------------
   -- Factory --
   -------------
   function Factory(
      Name : in Tag ) return Factory_Access is 
      -- create an object of the given class name
      Creator : Factory_Access := null;

      use Factory_Hash;
   begin
      Creator := Factory_Table( Get( Factory_Table_Hash, Name) ).Creator;

      if Creator = null then
	 Raise_Exception(Factory_Not_Found'Identity, External_Tag(Name) );
      end if;

      return Creator;
   end Factory;

   ---------------
   -- Attribute --
   ---------------
   procedure Attribute(
      This : in Natural;
      Name : in String;
      Id   : in Natural ) is 
      -- Add field name and id for the given class identifier

      QName : String := External_Tag(Factory_Table(This).Name) & "::" & Name;
   begin
     Factory_Table(This).Attributes(Id) := To_Unbounded_String(QName);
   end Attribute;

   ---------------
   -- Attribute --
   ---------------
   function Attribute(
      This   : in Tag;
      Name   : in String ) return Natural is 
      -- make the attribute name into an id

      Cls        : Natural := Class_ID( This );
      Attributes : Attribute_Array renames Factory_Table(Cls).Attributes; 
      QName      : constant String := External_Tag(This) & "::" & Name ;
   begin
      for i in Attributes'Range loop
         if Attributes(i) = Name then
	    return i;
	 end if; 
      end loop;

      return 0;
   end Attribute;

   ---------------
   -- Attribute --
   ---------------
   function Attribute(
      This   : in Tag;
      Id     : in Natural ) return String is 
      -- get the the field name from the field number
      Cls    : Natural := Class_ID( This );
   begin
      return To_String( Factory_Table(Cls).Attributes(Id) );
   end Attribute;

end ODB.Classes;
