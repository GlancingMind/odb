-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-object_loader.adb,v $
--  Description     : Load object from the an external xml file              --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 19-Aug-2002                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/10/08 04:43:55 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2003 Michael Erdmann                                       --
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
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Characters.Latin_1;            use Ada.Characters;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Streams;                       use Ada.Streams;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;

with Sax.Readers;                       use Sax.Readers;
with Sax.Exceptions;                    use Sax.Exceptions;
with Sax.Encodings;                     use Sax.Encodings;
with Sax.Locators;                      use Sax.Locators;
with Sax.Attributes;                    use Sax.Attributes;
with Sax.Models;                        use Sax.Models;

with Unicode.CES;                       use Unicode.CES;
with Unicode;                           use Unicode;
with Unicode.Names.Basic_Latin;         use Unicode.Names.Basic_Latin;
with Input_Sources.File;                use Input_Sources.File;

with Unchecked_Deallocation;

with ODB.Memory_Stream;			use ODB.Memory_Stream;
with ODB.Storage_Header;		use ODB.Storage_Header;
with ODB.Attribute_Dictionary;		use ODB.Attribute_Dictionary;
with ODB.XML;				use ODB.XML;
use  ODB;

package body ODB.Object_Loader is

   Version : constant String :=
       "$Id: odb-object_loader.adb,v 1.2 2003/10/08 04:43:55 merdmann Exp $";
					   
   Tag_Class     : constant String :=          ":" & XML.Tag_Class ;
   Tag_Header    : constant String := XML.NS & ":" & XML.Tag_Header;
   Tag_Attribute : constant String := XML.NS & ":" & XML.Tag_Attribute;
   Tag_Data      : constant String := XML.NS & ":" & XML.Tag_Data;

   type State_Type is ( S_Null, S_Class, S_Header, S_Data );

   type Buffer_Access is access Stream_Element_Array;

   -----------------
   -- Object_Data --
   -----------------
   type Object_Data_Type is record
         Locator    : Sax.Locators.Locator_Access;
         Attribs    : Attribute_Dictionary.Object( 200 );
	 Name       : Unbounded_String     := Null_Unbounded_String;
	 State      : State_Type           := S_Null;
	 Buffer     : Buffer_Access        := null;	
	 Next       : Stream_Element_Offset:= 1; 
	 OData      : Stream_Access        := null;
	 Header     : Storage_Header.Object; 
	 Path       : Unbounded_String     := To_Unbounded_String(".");
      end record;   		       
   
   ----------------
   -- Initialize --
   ----------------
   function Initialize return Object_Data_Access is 
      Data : Object_Data_Access := new Object_Data_Type;
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


   -- ************************************************************************ --
   -- ****      PROCEDURES/FUNCTIONS OVERWRITING THE SAX INTERFACE        **** --
   -- ************************************************************************ --

   -------------
   -- Warning --
   -------------
   procedure Warning(
      This   : in out Object;
      Except : in Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      Put_Line ("Sax.Warning ("
                & Get_Message (Except) & ", at "
                & To_String (Get_Locator (Except)) & ')');
   end Warning;

   -----------
   -- Error --
   -----------
   procedure Error(
      This : in out Object;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      Put_Line ("Sax.Error ("
                & Get_Message (Except) & ", at "
                & To_String (Get_Locator (Except)) & ')');
   end Error;

   -----------------
   -- Fatal_Error --
   -----------------
   procedure Fatal_Error
     (This : in out Object;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class) is
      Data : Object_Data_Access renames This.Data;
   begin
      Put_Line("Sax.Fatal_Error (" & Get_Message (Except) & ')' );
      Fatal_Error (Reader (This), Except);
   end Fatal_Error;

   -----------------
   -- Start_Cdata --
   -----------------
   procedure Start_Cdata (
      This : in out Object) is
      Data : Object_Data_Access       renames This.Data;
   begin
      Put_Line ("Sax.Start_Cdata () at " & To_String (Data.Locator.all));
   end Start_Cdata;

   ---------------
   -- End_Cdata --
   ---------------
   procedure End_Cdata (
      This : in out Object) is
      Data : Object_Data_Access       renames This.Data;
   begin
      Put_Line ("Sax.End_Cdata () at " & To_String (Data.Locator.all));
   end End_Cdata;

   --------------------------
   -- Set_Document_Locator --
   --------------------------
   procedure Set_Document_Locator(
      This : in out Object;
      Loc  : access Sax.Locators.Locator'Class) is
   begin
      -- Put_Line ("Sax.Set_Document_Locator ()");
      This.Data.Locator := Locator_Access (Loc);
   end Set_Document_Locator;

   --------------------------
   -- Start_Prefix_Mapping --
   --------------------------
   procedure Start_Prefix_Mapping(
      This    : in out Object;
      Prefix  : Unicode.CES.Byte_Sequence;
      URI     : Unicode.CES.Byte_Sequence) is
      Data    : Object_Data_Access renames This.Data;
   begin
      -- Put_Line("Mapping: " & Prefix & "=" & URI);
      if URI /= NS then
         raise Invalid_Object;
      end if;
   end Start_Prefix_Mapping;

   ------------------------
   -- End_Prefix_Mapping --
   ------------------------
   procedure End_Prefix_Mapping(
      This    : in out Object;
      Prefix  : Unicode.CES.Byte_Sequence) is
   begin
      null;
   end End_Prefix_Mapping;

   ----------------
   -- Characters --
   ----------------
   procedure Characters(
      This    : in out Object ;
      Ch      : Unicode.CES.Byte_Sequence ) is
      -- append the data elements into a single string.
      Data    : Object_Data_Access renames This.Data;

      ------------
      -- Decode --
      ------------
      function Decode(
         Value : in String ) return Stream_Element is
         Hi    : Character := Value(1);
         Lo    : Character := Value(2);

         function Nible2Positive(
            C : in Character ) return Natural is
         begin
            if C in '0'..'9' then
               return Character'Pos(C) - Character'Pos('0');
            else
               return Character'Pos(C) - Character'Pos('A') + 10;
            end if;
         end Nible2Positive;
      begin
         return Stream_Element(Nible2Positive( Lo ) + Nible2Positive(Hi)*16);
      end Decode;

   begin
      Data.Buffer(Data.Next) := Decode(ch);
      Data.Next := Data.Next + 1;
   end Characters;

   -------------------
   -- Start_Element --
   -------------------
   procedure Start_Element(
      This          : in out Object;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class) is
      -- This procedure is called in the begin of each xml tag. The allowed
      -- syntax is implemented by means of state machiene.
      Tag           : constant String := Namespace_URI & ":" & Local_Name;
      Data          : Object_Data_Access renames This.Data;
      State         : State_Type renames Data.State;


      function Value( 
         Name : in String ) return Unbounded_String is 
      begin      
	 return To_Unbounded_String( Get( Data.Attribs, Name ) );
      end Value;

      function IValue( 
         Name : in String ) return Integer is 
	 -- return the integer value of the referenced object
      begin
         return	 Integer'Value( To_String(Value(Name)) );
      end IValue;

   begin
      -- Put_Line( Tag  & " " & State_Type'Image(State) );
      for J in 0 .. Get_Length (Atts) - 1 loop
         Add( Data.Attribs, Get_Qname (Atts, J), Get_Value(Atts,J) );
      end loop;

      case State is
         when S_Null =>
            if Tag = Tag_Class then
               Data.Name := Value("name");
	       Class_Name( Data.Header, To_String(Data.Name) );

	       State := S_Class;
	    end if;

	 when S_Class =>
	    if Tag = Tag_Header then
	       State := S_Header;
	    elsif Tag = Tag_Data then	    
	       declare
	          Length : Stream_Element_Offset := Stream_Element_Offset(IValue( "length" ));
	       begin
	          Data.Buffer := new Stream_Element_Array( 1..Length );
	          Data.Next   := Data.Buffer'First;
	          Data.OData  := Memory_Stream.Stream( Length ); 
	       end ;

	       State := S_Data;
	    end if;

	 when S_Header =>
	    if Tag = Tag_Attribute then
	       declare
	          Name   : Unbounded_String := Value("name") ;
		  Offset : Natural :=  IValue( "offset" );
	       begin
	          Register_Attribute( Data.Header, Name, Offset );
	       end ;
	    end if;
	    
	 when S_Data =>
	    null;
      end case; 

   end Start_Element;

   -----------------
   -- End_Element --
   -----------------
   procedure End_Element
     (This : in out Object;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "") is
      -- This procedure is called at the end of a XML component. The
      -- collected data for this tag will be inserted into the tree
      Tag           : constant String := Namespace_URI & ":" & Local_Name;

      Data          : Object_Data_Access renames This.Data;
      State         : State_Type renames Data.State;
   begin
      Clear( Data.Attribs );

      case State is
         when S_Null =>
	    null;

	 when S_Class =>
	    if Tag = Tag_Class then
	       State := S_Null;
	    end if;

	 when S_Header =>
	    if Tag = Tag_Header then
	       State := S_Class ;
	    end if;
	    
	 when S_Data =>
	    if Tag = Tag_Data then
	       Copy_In( Data.OData, Data.Buffer.all );
	       State := S_Class ;
	    end if;
      end case; 

   end End_Element;

   ----------
   -- Read --
   ----------
   procedure Read(
      This  : in out Object;
      Name  : in String ) is
      -- read a configuration file and build the attribute tree.
      Data  : Object_Data_Access  renames This.Data;
      Input : File_Input;
   begin
      Open( To_String( Data.Path ) & "/" & Name, Input );
      Parse( This, Input);
      Close( Input );
   exception
      when others =>
         Close(Input);
         raise;
   end Read;

   ------------
   -- Header --
   ------------
   function Header(
      This : in Object ) return Storage_Header.Object is 
      Data : Object_Data_Access  renames This.Data;
   begin
      return Data.Header;
   end Header;

   ------------
   -- Stream --
   ------------
   function Stream( 
      This : in Object ) return Stream_Access is
      Data : Object_Data_Access  renames This.Data;
   begin
      return Data.OData;
   end Stream;

   ----------------
   -- Class_Name --
   ----------------
   function InstanceOf(
      This : in Object ) return String is
      Data : Object_Data_Access  renames This.Data;
   begin
      return To_String(Data.Name);
   end InstanceOf;


end ODB.Object_Loader;
