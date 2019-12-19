-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-object_writer.adb,v $
--  Version         : $Revision: 1.1 $                                       --
--  Description     : ODB Utility root package                               --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 21-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/10/06 04:27:57 $                           --
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
with Ada.Characters.Latin_1;			use Ada.Characters.Latin_1;
with Ada.Strings;				use Ada.Strings;
with Ada.Strings.Fixed;				use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;

with Util.String_Array;				use Util.String_Array;
use  Util;

with ODB.XML;					use ODB.XML;
use  ODB;


package body ODB.Object_Writer is

   Version : constant String 
       := "$Id: odb-object_writer.adb,v 1.1 2003/10/06 04:27:57 merdmann Exp $";

   -----------------
   -- Object_Data --
   -----------------
   type Object_Data is record
         Path : Unbounded_String := To_Unbounded_String(".");
      end record;

   ----------------
   -- Initialize --
   ----------------
   function Initialize return Object_Data_Access is 
      Data : Object_Data_Access := new Object_Data;
   begin
      return Data;
   end Initialize;

   ----------
   -- Path --
   ----------
   procedure Path(
      This   : in out Object;
      Value  : in String ) is 
      Data   : Object_Data_Access renames This.Data;
   begin
      Data.Path := To_Unbounded_String( Value );
   end Path;

   ----------
   -- QTag --
   ----------
   function QTag( 
      Tag : in String ) return String is
   begin
      return "cls" & ":" & Tag;
   end QTag;

   -------------------
   -- XML_Attribute --
   -------------------
   function XML_Attribute(
      Name  : in String;
      Value : in String ) return String is 
   begin
      return Name & "=" & '"' & Trim(Value,Both) & '"' ;
   end XML_Attribute;


   -----------
   -- Write --
   -----------
   procedure Write(
      This   : in out Object;
      Name   : in String;
      Header : in Storage_Header.Object;
      OData  : in Stream_Element_Array ) is 
      -- Write out the named object base on the given data and header
      Data   : Object_Data_Access renames This.Data;

      File    : Stream_IO.File_Type ;
      S       : Stream_Access ;
      Attribs : String_Array.Handle := Attributes( Header );
      Cnt     : Natural := 0;

      ------------
      -- Encode --
      ------------
      function Encode(
         Value  : in Stream_Element ) return String is
         Hi     : Natural := ( Natural(Value) / 16 ) mod 16;
         Lo     : Natural :=   Natural(Value)  mod 16;
	 HexVal : constant String := "0123456789ABCDEF";
      begin
         return HexVal( Hi+1 ) & HexVal( Lo+1 );
      end Encode;

   begin
      Create( File => File, 
         Mode => Out_File, 
	 Name => To_String( Data.Path ) & "/" & Name
      );
      S := Stream( File );
      String'Write( S, "<?xml " & XML_Attribute( "version","1.0") & "?>" & LF );
      String'Write( S, "<" &  Tag_Class &  " " &
         XML_Attribute( "xmlns:cls", XML.NS )
         & " " &
         XML_Attribute( "name", Class_Name( Header ) ) 
         & ">" & LF );

      String'Write(S,"<" & QTag(Tag_Header) & ">" & LF );
      for i in Attribs'Range loop
         declare
	    Field  : constant String := To_String( Attribs(i) );
	    Offset : Natural := Lookup_Attribute( Header, Field );
	 begin
	    String'Write( S,
	        "   <" & QTag(Tag_Attribute) & " " & 
	               XML_Attribute("name", Field ) & " " &
		       XML_Attribute("offset", Natural'Image(Offset) ) &
		    "/>" & LF 
	    );
	 end;
      end loop;
      String'Write(S,"</" & QTag(Tag_Header) & ">" & LF );
      Free( Attribs );

      -- Write the data
      String'Write(S,
         "<" & QTag(Tag_Data) & " " &
             XML_Attribute("length", Natural'Image(OData'Length) ) &
	  " >" & LF );

      for i in OData'Range loop
         if Cnt > 15 then
	    String'Write( S, "   " & LF );
	    Cnt := 0;
	 end if;
	 String'Write( S, Encode( OData(i) ) & " " );
	 Cnt := Cnt + 1;
      end loop;
      
      String'Write(S,LF & "</" & QTag(Tag_Data) & ">" & LF);
      String'Write(S,"</" & Tag_Class & ">" );

      Close( File );

   end Write;
      	
end ODB.Object_Writer;

