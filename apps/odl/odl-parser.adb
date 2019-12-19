-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/apps/odl/odl-parser.adb,v $
--  Description     : ODL Parser & Code generator                            --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 22-Dec-2000                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/10/06 04:27:56 $                           --
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

--* Ada
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Characters.Latin_1;            use Ada.Characters;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;

--* Translator packages
with ODL.Scanner;                       use ODL.Scanner;
with ODL.Options;                       use ODL.Options;

with Util.List;

package body ODL.Parser is

   Version : constant String :=
         "$Id: odl-parser.adb,v 1.17 2003/10/06 04:27:56 merdmann Exp $";

   ------------------------
   -- ODL_Reserved_Words --
   ------------------------
   type ODL_Reserved_Words is (
     ODL_Attribute,
     ODL_Persistent,
     ODL_Identifier,
     ODL_End_Of_File,
     ODL_ISA,

     Ada_Semicolon,
     Ada_Type,
     Ada_Package,
     Ada_Body,
     Ada_Is,
     Ada_Private,
     Ada_Begin,
     Ada_End,
     Ada_Record,
     Ada_Colon,
     Ada_Comma,
     Ada_Dot );

   type Keyword_List is array( ODL_Reserved_Words ) of Unbounded_String;

   Keywords : constant Keyword_List := (
      ODL_Attribute    =>  To_Unbounded_String("ATTRIBUTE"),
      ODL_Persistent   =>  To_Unbounded_String("PERSISTENT"),
      ODL_Isa          =>  To_Unbounded_String("ISA"),
      ODL_Identifier   =>  Null_Unbounded_String,
      ODL_End_Of_File  =>  Null_Unbounded_String,

      Ada_Type         =>  To_Unbounded_String("TYPE"),
      Ada_Private      =>  To_Unbounded_String("PRIVATE"),
      Ada_Is           =>  To_Unbounded_String("IS"),
      Ada_End	       =>  To_Unbounded_String("END"),
      Ada_Record       =>  To_Unbounded_String("RECORD"),
      Ada_Begin	       =>  To_Unbounded_String("BEGIN"),
      Ada_Package      =>  To_Unbounded_String("PACKAGE"),
      Ada_Body         =>  To_Unbounded_String("BODY"),

      Ada_Semicolon    =>  To_Unbounded_String(";"),
      Ada_Colon	       =>  To_Unbounded_String(":"),
      Ada_Comma	       =>  To_Unbounded_String(","),
      Ada_Dot	       =>  To_Unbounded_String(".")
   );

   ----------------------------
   -- Member_Definition_Type --
   ----------------------------
   type Member_Definition_Type is record
         Attribute      : Unbounded_String := Null_Unbounded_String;
	 Attribute_Type : Unbounded_String := Null_Unbounded_String;
      end record;

   package Members is new Util.List( Member_Definition_Type );

   ----------------------------
   -- Object_Definition_Type --
   ----------------------------
   type Object_Definition_Type is record
         Name       : Unbounded_String := Null_Unbounded_String;
	 Parent     : Unbounded_String := Null_Unbounded_String;
	 Attributes : Members.Handle   := Members.List;
      end record;

   package Objects is new Util.List( Object_Definition_Type );

   ---------------------
   -- ODL_Reader_Type --
   ---------------------

   type ODL_Reader_Type is record
         --- Data of the parser
         Current              : Unbounded_String := Null_Unbounded_String;
         Reader               : Scanner.File_Reader ;
         Nbr_Of_Syntax_Errors : Natural := 0;
         Nbr_Of_Warnings      : Natural := 0;

         Nesting_Level        : Natural := 0;
	 Ada_Declare	      : Boolean := False;
         -- currently processed tokens
         Current_Token        : Token_Type;
         Current_Keyword      : ODL_Reserved_Words;
         -- push back storage
         Push_Back            : Token_Type;
         Push_Back_Keyword    : ODL_Reserved_Words;
	 --
	 Package_Name         : Unbounded_String := Null_Unbounded_String;
	 Object_List          : Objects.Handle := Objects.List;
      end record;

   -- control the scanner
   type Get_Mode is (
      Suppress_Token,          -- dont copy into output file
      Accept_Token,            -- copy into output file
      Defere_Copy              -- keep the token in the output buffer, until
                               -- either a Accept or a Discard Symbol is
                               -- executed.
      );

   ---========================================================================---
   ---===             S U P P O R T    F U N C T I O N S                   ===---
   ---========================================================================---

   ---------------------
   -- New_ODL_Parser --
   ---------------------
   function New_ODL_Parser(
      F : in File_Reader ) return ODL_Reader is
      -- Create a new instance of the ODL Parser
      Result : ODL_Reader := new ODL_Reader_Type;
   begin
      Result.Reader := f;
      Result.Nbr_Of_Syntax_Errors := 0;

      return Result;
   end New_ODL_Parser;

   ------------------
   -- Parser_Input --
   ------------------
   procedure Parser_Input(
      E : in out ODL_Reader;
      F : in Scanner.File_Reader ) is
      -- change the input file for the parser and close the previous
      -- input file.
   begin
      E.Package_Name  := Null_Unbounded_String;
      E.Nesting_Level := 0;
      E.Ada_Declare   := False;

      Close( E.Reader );
      E.Reader := F;
   end Parser_Input;

   ----------------------
   -- Number_Of_Errors --
   ----------------------
   function Number_Of_Errors(
      E : ODL_Reader ) return Natural is
      -- Return the number of errors found by the parser.
   begin
      return E.Nbr_Of_Syntax_Errors;
   end Number_Of_Errors;

   ----------------------
   -- Number_Of_Errors --
   ----------------------
   function Number_Of_Warnings(
      E : ODL_Reader ) return Natural is
      -- Return the number of errors found by the parser.
   begin
      return E.Nbr_Of_Warnings;
   end Number_Of_Warnings;

   ------------------
   -- Syntax_Error --
   ------------------
   procedure Syntax_Error(
      E    : in ODL_Reader;
      Text : in String ) is
      -- Indicate a syntax error
   begin
      E.Nbr_Of_Syntax_Errors := E.Nbr_Of_Syntax_Errors + 1;
      Message( e.Reader, " error : " & text );
      raise Syntax_Exception;
   end Syntax_Error;

   -------------
   -- Warning --
   -------------
   procedure Warning(
      e    : ODL_Reader;
      text : in String ) is
      -- Indicate a warining to the user.
   begin
      E.Nbr_Of_Warnings := E.Nbr_Of_Warnings + 1;
      Message( e.Reader, " warning : " & text );
   end Warning;

   --------------
   -- Pedantic --
   --------------
   procedure Pedantic(
      e    : ODL_Reader;
      text : in String ) is
      -- indicate a pedantic warning to the user
   begin
      if Option_Pedantic then
         Warning( E, Text );
      end if;
   end Pedantic;

   ----------------
   -- Upper_Case --
   ----------------
   function Upper_Case(
      S      : in Unbounded_String ) return Unbounded_String is
      Result : String := To_String(S);
   begin
      for i in Result'Range loop
         Result(i) := To_Upper(Result(i));
      end loop;

      return To_Unbounded_String(Result);
   end Upper_Case;

   -----------
   -- Quote --
   -----------
   function Quote (
      ID : String) return String is
   begin
      return '"' & ID & '"';
   end Quote;

   -----------------
   -- Next_Symbol --
   -----------------
   function Next_Symbol(
      E        : ODL_Reader;
      mode     : Get_Mode := Suppress_Token ) return ODL_Reserved_Words is
      -- Read in the next symbol. It is checked against the
      -- list of reserved words. If it is not a reserved word
      -- we assume an identifier.
      T        : Token_Type renames E.Current_Token;
      Temp     : Unbounded_String;
   begin
      if E.Push_Back /= Null_Token then
          T                 := E.Push_Back;
          E.Current_Keyword := E.Push_Back_Keyword;
          E.Push_Back       := Null_Token;
      else
          -- read from file and strip of any comment token.
          Get_Token( E.Reader, T );
          while T.Lexical_Unit = Comment_Lex and
                T.Lexical_Unit /= End_Of_File_Lex
          loop
             Get_Token( E.Reader, T );
          end loop;
      end if;

      case mode is
         when Suppress_Token =>
            Ignore_Input( E.Reader );
         when Accept_Token =>
            Accept_Input( E.Reader );
         when Others =>
            null;
      end case;

      if T.Lexical_Unit = End_Of_File_Lex then
         if Option_Debug then
            Put_Line("Next_Symbol: end of file");
         end if;
         raise End_of_File_Exception;
      end if;

      -- match keywords in uper cases only
      Temp              := Upper_Case( To_Unbounded_String(Trim(T.Lexicon, Right)) );
      E.Current         := To_Unbounded_String(Trim(T.Lexicon, Right) );
      E.Current_Keyword := ODL_Identifier;

      for i in ODL_Reserved_Words loop
         if Keywords(I) = Temp then
            E.Current         := Temp;
            E.Current_Keyword := I;
            exit;
         end if;
      end loop;

      pragma Debug( Put_Line( "Next_Symbol: " & To_String(E.Current) ));
      return E.Current_Keyword;
   end Next_Symbol;

   -----------------------------
   -- Push_Back_Current_Token --
   -----------------------------
   procedure Push_Back_Current_Token(
      -- This allows to push back one token which will
      -- read in again upon invoktion of the Next_Symbol
      -- procedure.
      E : in ODL_Reader ) is
   begin
      E.Push_Back         := E.Current_Token;
      E.Push_Back_Keyword := E.Current_Keyword;
   end Push_Back_Current_Token;

   -------------------
   -- Accept_Symbol --
   -------------------
   procedure Accept_Symbol(
      E : in ODL_Reader ) is
      -- Accept the symbol for copying into the processing result.
   begin
      pragma Debug( Put_Line( "Accept_Symbol" ) );
      Accept_Input( E.Reader );
   end Accept_Symbol;

   --------------------
   -- Discard_Symbol --
   --------------------
   procedure Discard_Symbol(
      E  : in ODL_Reader ) is
   begin
      pragma Debug( Put_Line( "Discard_Symbol" ) );
      Ignore_Input( E.Reader );
   end Discard_Symbol;

   ------------
   -- Expect --
   ------------
   procedure Expect(
      E       : ODL_Reader;
      What    : ODL_Reserved_Words;
      Where   : String := "";
      Mode    : Get_Mode := Suppress_Token ) is
      -- Check for the expected token type. If the item is not found,
      -- an error message will be generated.
      Current : ODL_Reserved_Words;
      --
   begin
      Current := Next_Symbol(e, mode);
      if Current /= what then
         if where = ""  then
            Syntax_Error( e,
               " unexpected token " & ODL_Reserved_Words'Image(Current) &
               " found, expected " &  ODL_Reserved_Words'Image(what));
         else
            Syntax_Error( e,
               " unexpected token " & ODL_Reserved_Words'Image(Current) &
               " found in " & where &
               ", expected " &  ODL_Reserved_Words'Image(what));
         end if;
      end if;
   end Expect;

   ---------------
   -- Skip_Till --
   ---------------
   procedure Skip_Till(
      e        : ODL_Reader;
      what     : ODL_Reserved_Words;
      mode     : Get_Mode := Suppress_Token ) is
      -- Read all tokens till the given token is found.
      Current  : ODL_Reserved_Words;
      ---
   begin
      Current := Next_Symbol(e, mode);
      while Current /= what loop
         Current := Next_Symbol(e, mode);

         if Current = ODL_End_Of_File then
            Syntax_Error( e, "missing " & ODL_Reserved_Words'Image(what) );
            exit;
         end if;

      end loop;
   end Skip_Till;

   ----------------
   -- Identifier --
   ----------------
   function Identifier(
      E : in ODL_Reader ) return Unbounded_String is
      -- Get the current identifier
   begin
      return E.Current;
   end Identifier;

   ---------
   -- Ada --
   ---------
   procedure Ada(
      E             : in ODL_reader;
      Statement     : in String;
      Comment       : in String := "" ) is
      -- This procedure inserts a nice Ada statement into the result
      -- file applying the correct itendation level.
      Nesting_Level : Integer renames e.Nesting_Level;
      ---
   begin
      if comment = "" then
         Insert( E.Reader, (
             (Nesting_Level*3) * " " ) & statement );
      else
         Insert( E.Reader,
             ((Nesting_Level*3) * " " ) & statement &
             "-- " & comment
         );
      end if;
   end Ada;

   -----------------
   -- Ada_Declare --
   -----------------

   procedure Ada_Declare(
      E             : ODL_Reader ) is
      -- This outputs a declare statement. Applying this procedure
      -- will yield the correct layout for the variable declarations.
      Nesting_Level : Integer renames e.Nesting_Level;
      Ada_Declare   : Boolean renames e.Ada_Declare;
      --
   begin
      Ada( e, "declare" );
      Ada_Declare   := True;
      Nesting_Level := Nesting_Level + 1;
   end Ada_Declare;

   ---------------
   -- Ada_Begin --
   ---------------

   procedure Ada_Begin(
      e             : ODL_Reader ) is
      -- Refere to Ada_Declare
      Nesting_Level : Integer renames e.Nesting_Level;
      Ada_Declare   : Boolean renames e.Ada_Declare;
      --
   begin
      if not Ada_Declare then
         Ada(e, "begin");
         Nesting_Level := Nesting_Level + 1;
      else
         Ada_Declare := False;
         Nesting_Level := Nesting_Level - 1;
         Ada_Begin( e );
      end if;
   end Ada_Begin;

   -------------
   -- Ada_End --
   -------------
   procedure Ada_End(
      e    : in ODL_reader ) is
      -- Close a text block. For details refer to Ada_Declare.
      Nesting_Level   : Integer renames e.Nesting_Level;
   begin
      Nesting_Level := Nesting_Level - 1;
      Ada(e, "end;");
   end Ada_End;

   -------------------
   -- Ada_Exception --
   -------------------
   procedure Ada_Exception(
      E  : in ODL_Reader ) is
      -- Insert an exception clause
      Nesting_Level   : Integer renames E.Nesting_Level;
   begin
      Nesting_Level := Nesting_Level - 1;
      Ada( e, "exception" );
      Nesting_Level := Nesting_Level + 1;
   end Ada_Exception;
   ---========================================================================---
   ---===             P A R S E R    P R O C E D U R E S                   ===---
   ---========================================================================---

   -------------------
   -- ODL_Field_IDs --
   -------------------
   procedure ODL_Field_IDs(
      E : in out ODL_Reader;
      O : Object_Definition_Type ) is
      use Members;

      ID : Natural := 0;
      R  : List_Reader_Handle := List_Reader(O.Attributes);
      M  : Member_Definition_Type := First( R );
   begin
      while not List_End( R ) loop
         ID := ID + 1;
         M := Next( R );

      	 Ada( E,
	    "D_" & To_String(M.Attribute) &
	    ": constant Natural:=" & Natural'Image(ID) & ";"
	 );
      end loop;
   end ODL_Field_IDs;

   ---------------
   -- ODL_Write --
   ---------------
   procedure ODL_Serialize(
      E : in out ODL_Reader;
      O : Object_Definition_Type ) is
      use Members;

      R : List_Reader_Handle := List_Reader(O.Attributes);
      M : Member_Definition_Type := First( R );
   begin
      Ada( E, "");
      Ada( E, "procedure Serialize( ");
      Ada( E, "   Item   : in out " & To_String(O.Name) & ";" );
      Ada( E, "   Header : in out Storage_Header.Object; " );
      Ada( E, "   S      : in Stream_IO.Stream_Access ) is " );
      Ada_Begin(E);

      if O.Parent /= Null_Unbounded_String then
         Ada(E, "Serialize( " & To_String(O.Parent) & "( Item ), Header, S);" );
      end if;

      while not List_End( R ) loop
         M := Next( R );

         Ada( E,
	    "Register_Attribute( Header, D_" & To_String(M.Attribute) &
	    ", Write_Offset( S ), Object'Tag );");
      	 Ada( E, To_String(M.Attribute_Type) &
	         "'Output(S, Item." & To_String(M.Attribute) & ");");
      end loop;

      Ada_End(E);

   end ODL_Serialize;

   ---------------------
   -- ODL_Deserialize --
   ---------------------
   procedure ODL_Deserialize(
      E : in out ODL_Reader;
      O : Object_Definition_Type ) is
      use Members;

      R : List_Reader_Handle := List_Reader(O.Attributes);
      M : Member_Definition_Type := First( R );
   begin
      Ada( E, "");
      Ada( E, "procedure Deserialize( ");
      Ada( E, "   Item   : in out " & To_String(O.Name) & ";" );
      Ada( E, "   Header : in out Storage_Header.Object; " );
      Ada( E, "   S      : in Stream_IO.Stream_Access ) is " );

      Ada( E, "   Field  : String_Array.Handle := Attributes( Header );" );
      Ada_Begin(E);

      if O.Parent /= Null_Unbounded_String then
         Ada(E, "Deserialize( " & To_String(O.Parent) & "(Item), Header, S );" );
      end if;

      Ada( E, "for i in Field'Range loop");
      Ada_Declare(E);
      Ada( E, "ID     : Natural := 0;" );
      Ada( E, "Offset : Natural := 0;" );
      Ada( E, "Name   : constant String := To_String( Field(i) );" );
      Ada_Begin(E);
      Ada( E, "ID := Classes.Attribute( Object'Tag, Name );" );

      Ada( E, "if ID /= 0 then" );
      Ada( E, "   Offset := Storage_Header.Lookup_Attribute( Header, Name );");

      if Option_Debug_Code then
         Ada(E, " Text_IO.Put_Line( Name & Natural'Image(Offset) );" );
      end if;
       
      Ada( E, "   Read_Offset( S, Offset );" );

      Ada( E, "   case ID is ");
      while not List_End( R ) loop
         M := Next( R );
	 Ada( E, "      when D_" & To_String(M.Attribute) & " =>");
      	 Ada( E, "      Item." & To_String(M.Attribute) & ":=" &
	         To_String(M.Attribute_Type) & "'Input(S);");
      end loop;
      Ada( E, "      when Others => null ;");
      Ada( E, "   end case;" );
      Ada( E, "end if;" );
      Ada_End(E);

      Ada( E, "end loop ;" );
      Ada_Exception(E);
      Ada( E, "when Storage_Header.Unknown_Attribute => null;");
      Ada_End(E);

   end ODL_Deserialize;

   -----------------
   -- ODL_Factory --
   -----------------
   procedure ODL_Factory (
      E : in out ODL_Reader;
      O : Object_Definition_Type ) is
      use Members;
   begin
      Ada( E, "");
      Ada( E, "function Factory return Persistent.Reference is" );
      Ada_Begin(E);
      Ada( E, "return new " & To_String(O.Name) & ";" );
      Ada_End(E);
   end ODL_Factory;

   ------------------
   -- ODL_Register --
   ------------------
   procedure ODL_Register (
      E : in out ODL_Reader;
      O : Object_Definition_Type ) is
      use Members;

      ID : Natural := 1;
      R  : List_Reader_Handle := List_Reader(O.Attributes);
      M  : Member_Definition_Type := First( R );
   begin
      Ada( E, "");
      Ada( E, "procedure Register_" & To_String( O.Name ) & " is" );
      Ada( E, "  ID : Natural := 0;");
      Ada_Begin(E);
      Ada( E, "ID := Classes.Register_Factory(" &
                       To_String(O.Name) & "'Tag, Factory'Access );" );
      while not List_End( R ) loop
         ID := ID + 1;
         M := Next( R );

	 Ada( E, "Classes.Attribute( ID, "
	         & Quote( "D_" & To_String(M.Attribute) ) & ","
		 & "D_" & To_String(M.Attribute) & ");"  );
      end loop;

      Destroy(R);
      Ada_End(E);
   end ODL_Register;

   ----------------------------
   -- ODL_Generate_Glue_Code --
   ----------------------------
   procedure ODL_Generate_Glue_Code(
      E : in out ODL_Reader ) is
      -- generate the code as imposed by the interfaces of an persistent
      -- object.
      use Objects;

      R : List_Reader_Handle := List_Reader(E.Object_List );
      O : Object_Definition_Type := First( R );
   begin
      if Length( E.Object_List ) = 0 then
         Warning( E, "No persistent objects defined in package");
	 return;
      end if;

      while not List_End( R ) loop
         O := Next( R );

	 if Members.Length( O.Attributes ) = 0 then
	    Syntax_Error( E, "Persistent type " & To_String(O.Name)  & " contains no attributes");
	 end if;

	 ODL_Field_IDs( E, O );
	 ODL_Serialize( E, O );
	 ODL_Deserialize( E, O );
	 ODL_Factory( E, O );
	 ODL_Register( E, O );
      end loop;

      Ada( E, "", Comment => "**** End of the generated code ****");
   end ODL_Generate_Glue_Code;

   ------------------------
   -- ODL_Type_Defintion --
   ------------------------
   procedure ODL_Type_Definition(
      E       : in out ODL_Reader ) is
      -- process the defintion of an persistent object
      --  type <name> is persistent [record ....... | private ]
      --  type <name> isa <name> with [ record....  | private ] ';'
      --
      Current    : ODL_Reserved_Words := Next_Symbol(E, Mode => Accept_Token );
      Name       : Unbounded_String;
      Parent     : Unbounded_String;
      Field_Name : array(1..100) of Unbounded_String;

      Field_Type : array(1..100) of Unbounded_String;
      Field_Nbr  : Natural := Field_Name'First;

      procedure Add_Object_Information is
         O : Object_Definition_Type;
	 M : Member_Definition_Type;
	 Use Members;
      begin
         O.Name       := Name;
	 O.Parent     := Parent;
         O.Attributes := Members.List;

	 for i in 1..Field_Nbr-1 loop
	    M.Attribute      := Field_Name(i);
	    M.Attribute_Type := Field_Type(i);
	    Append( O.Attributes, M );
	 end loop;

	 Objects.Append( E.Object_List, O );
      end Add_Object_Information;

   begin
      Name := Identifier(E);

      Current := Next_Symbol( E, Mode => Defere_Copy );
      case Current is
         when ADA_IS =>
	    Accept_Symbol(E);
	    Discard_Symbol( E );
            Current := Next_Symbol(E, Mode => Defere_Copy );

            if Current /= ODL_Persistent then
               Accept_Symbol(E);
               return;
            end if;

	    Discard_Symbol(E);
            Accept_String( E.Reader," new Persistent.Object with " );

	 when ODL_ISA =>
	    Discard_Symbol(E);

	    Current := Next_Symbol(E, Mode => Defere_Copy );
	    Parent := Identifier(E);
	    Discard_Symbol(E);

	    Accept_String( E.Reader," is new " & To_String( Parent ) );
	    Current := Next_Symbol(E, Mode => Accept_Token );
         when Others =>
	    Accept_Symbol( E );
	    return;
      end case;

--      Put_Line( To_String(Parent) & "::" & To_String(Name) );
      Current := Next_Symbol( E, Mode => Accept_Token );

      if Current = Ada_Semicolon or Current = Ada_Private then
         return;
      end if;

      Current := Next_Symbol(E, Mode => Accept_Token);
      while Current /= Ada_End loop
         declare
	    Fields    : array( 1..50 ) of Unbounded_String ;
	    Field     : Natural := Fields'First;
	    Type_Name : Unbounded_String := Null_Unbounded_String;
	 begin
	    while Current /= Ada_Colon loop
	       Fields( Field ) := Identifier(E);

	       Current := Next_Symbol(E, Mode => Accept_Token );
	       if Current = Ada_Comma then
	          Field := Field + 1;
	          Current := Next_Symbol(E, Mode => Accept_Token );
	       end if ;
	    end loop;

	    Current := Next_Symbol(E, Mode => Defere_Copy );

	    if Current = ODL_Attribute then
	       Discard_Symbol(E);

	       Current   := Next_Symbol(E, Mode => Accept_Token);
	       Type_Name := Identifier(E);

	       for i in 1..Field loop
	          Field_Name( Field_Nbr ) := Fields(i);
	          Field_Type( Field_Nbr ) := Type_Name;

	          Field_Nbr := Field_Nbr + 1;
	       end loop;
	    else
	       Accept_Symbol(E);
	    end if;

	    Accept_Symbol(E);
	    Skip_Till( E, ADA_Semicolon, Mode => Accept_Token );
	    Current := Next_Symbol(E, Mode => Accept_Token );
	 end;
      end loop;
      Expect( E, Ada_Record, Mode => Accept_Token );
      Flush( E.Reader );

      Add_Object_Information;

      Ada( E, "" );
      Ada( E, "" );

      Ada( E, "procedure Serialize(");
      Ada( E, "   Item   : in out " & To_String( Name ) & ";" );
      Ada( E, "   Header : in out Storage_Header.Object ; " );
      Ada( E, "   S      : in Stream_IO.Stream_Access);" );

      Ada( E, "procedure Deserialize(");
      Ada( E, "   Item   : in out " & To_String( Name ) & ";");
      Ada( E, "   Header : in out Storage_Header.Object ; " );
      Ada( E, "   S      : in Stream_IO.Stream_Access);");

   end ODL_Type_Definition;

   -----------------
   -- ODL_Prelude --
   -----------------
   procedure ODL_Prelude(
      E : in out ODL_Reader ) is
   begin
      Ada( E, "with Ada.Tags;                    use Ada.Tags;");
      Ada( E, "with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;");
      Ada( E, "with Ada.Streams;                 use Ada.Streams;" );
      Ada( E, "with Ada.Streams.Stream_IO;");
      Ada( E, "use  Ada;");
      Ada( E, "");
      Ada( E, "with ODB.Persistent;              use ODB.Persistent;");
      Ada( E, "with ODB.Classes;                 use ODB.Classes;");
      Ada( E, "with ODB.Storage_Header;          use ODB.Storage_Header;");
      Ada( E, "with ODB.Memory_Stream;           use ODB.Memory_Stream;");
      Ada( E, "use  ODB;" );

      Ada( E, "with Util.String_Array;           use Util.String_Array;");
      Ada( E, "use  Util;" );
      Ada( E, "");
   end ODL_Prelude;

   -------------------
   -- ODL_Statement --
   -------------------
   procedure ODL_Statement(
      E       : in out ODL_Reader ) is
      Current : ODL_Reserved_Words;
   begin
      -- clear the transient data fo the parser
      if E.Nbr_Of_Syntax_Errors > Option_Error_Limit then
         Warning( E, "Number of errors exceed limit ");
         Warning( E, "*** Aborted ***" );
         raise End_Of_File_Exception;
      end if;

      -- process the file contents
      Current := Next_Symbol(E, Mode => Defere_Copy );

      if Current = Ada_Package and E.Package_Name = NUll_Unbounded_String then
         declare
	    In_Package_Body : Boolean := False;
	 begin
            Accept_Symbol(E);
	    Current := Next_Symbol( E, Mode => Defere_Copy );
	    Accept_Symbol(E);

	    if Current = Ada_Body then
	       In_Package_Body := True;
	       Current := Next_Symbol( E, Mode => Accept_Token );
	    end if;
	    E.Package_Name := Identifier(E);
	    Expect( E, ADA_Is, Mode => Accept_Token );

	    if In_Package_Body then
	       Flush( E.Reader );
	       ODL_Generate_Glue_Code( E );
	    end if;
	 end;
      elsif Current = Ada_Type then
         Accept_Symbol(E);
	 ODL_Type_Definition(E);
      else
         Accept_Symbol(E);
      end if;

      -- **********************************************************************
      -- *               H A N D L E   E X C E P T I O N S                    *
      -- **********************************************************************
   exception
      when Syntax_Exception =>
         Skip_Till( e, Ada_Semicolon );
      when End_Of_File_Exception =>
         raise;

      when The_Error : Others =>
         Warning( e, "Exception " & Exception_Name( The_Error ) & " occured " );
         Warning( E, "          " & Exception_Message( The_Error ) );
         raise;
   end ODL_Statement;

end ODL.Parser;
