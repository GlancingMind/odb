-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/adadoc/adadoc.adb,v $
--  Description     :                                                        --
--  Author          : Michael Erdmann                                        --
--  Created         : 26.4.2003                                              --
--  Last Modified By: $Author: merdmann $                                    --
--  Last Modified On: $Date: 2003/06/23 07:37:11 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2003 Michael Erdmann                                       --
--                                                                           --
--  CELLS is free software;  you can redistribute it  and/or modify it under --
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
--                                                                           --
--  Functional Description                                                   --
--  ======================                                                   --
--                                                                           --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  None                                                                     --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
--  Contact                                                                  --
--  =======                                                                  --
--  Michael Erdmann <michael.erdmann@snafu.de>                               --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Command_Line;              use Ada.Command_Line;
with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;             use Ada.Strings.Fixed;
with Ada.Strings;                   use Ada.Strings;
use  Ada;

with Text;                          use Text;

procedure Adadoc is

   Version                 : constant String := "$Revision: 1.1.1.1 $";

   Output_File             : File_Type ;
   Errors                  : Natural := 0;
   Warnings                : Natural := 0;

   Part_Name               : Unbounded_String := Null_Unbounded_String;
   Package_Name            : Unbounded_String := Null_Unbounded_String;
   Chapter_Title           : Unbounded_String := Null_Unbounded_String;
   HTML_File_Name          : Unbounded_String := Null_Unbounded_String;

   Tag_Ada95_Comment       : constant String := "--";
   Tag_Ada95_Private       : constant String := "private";
   Tag_Ada95_End           : constant String := "end";
   Tag_Ada95_Function      : constant String := "function";
   Tag_Ada95_Procedure     : constant String := "procedure";
   Tag_Ada95_Generic       : constant String := "generic";
   Tag_Ada95_Is            : constant String := "is";

   Tag_Pkg_Description     : constant String := "Functional Description  ";
   Tag_References          : constant String := "References  ";
   Tag_Contact             : constant String := "Contact  ";
   Tag_Restrictions        : constant String := "Restrictions  ";
   Tag_Shortdescription    : constant String := "Description  ";

   Tag_Unformated          : constant String := "& ";


   Default_Description     : Paragraph_Type;
   Pkg_Interface           : Paragraph_Type;
   Functional_Description  : Paragraph_Type;
   Restrictions            : Paragraph_Type;
   References              : Paragraph_Type;
   Contact                 : Paragraph_Type;

   Nbr_Of_API_Entries      : Natural := 1;
   Total_API_Entries       : Natural := 0;
   Total_Nbr_Of_Files      : Natural := 0;
   Option_Reverse_Style    : Boolean := False;
   Option_Silent           : Boolean := False;
   Option_Verbose          : Boolean := False;

   type API_Description_Type is record
         Description   : Paragraph_Type;
         Specification : Paragraph_Type;
      end record;

   API_Table : array( 1..1000 ) of API_Description_Type;

   type Scanner_State_Type is (
      S_Null,
      S_Free_Software,
      S_Functional_Description,
      S_References,
      S_Restrictions,
      S_Contact,
      S_API_Description,
      S_API_Specification,
      S_Pkg_Interface,
      S_Generic_Parameters
   );

   ---********************************************************************---
   ---**                  TOOL ENVIRONMENT                              **---
   ---********************************************************************---

   -----------
   -- Error --
   -----------
   procedure Error(
      S : in String ) is
      -- print out an error string and increment the error count
   begin
      Put_Line(S);
      Errors := Errors + 1;
   end Error;

   ----------
   -- Help --
   ----------
   procedure Help is
   begin
      Put_Line("Usage: adadoc [ global option(s) ] [ option(s) ] file(s)");
      Put_Line("");
      Put_Line("file(s)  Any number of Ada95 specification packages");
      Put_Line("sgmlfile Name of the the output unit. The file name where the ");
      Put_Line("         unit is stored in will be the same with the suffix sgml.");
      Put_Line("");
      Put_Line("global options" );
      Put_Line("   -h    Display this message ");
      Put_Line("   -l    Show the license agrement");
      Put_Line("   -r    Apply reverse order to comments and methods");
      Put_Line("   -v    Verbose mode which displays more information about ");
      Put_Line("         of the progress");

      Put_Line("");
      Put_Line("options");
      Put_Line("   -c    Name of the chpater where all packages shall be");
      Put_Line("         included.");
      Put_Line("   -o    Define output unit. This parameter is mandatory");
      Put_Line("");
   end Help;

   -------------
   -- License --
   -------------
   procedure License is
   begin
      if Option_Silent then
         return;
      end if;

      Put_Line("LICENSE:");
      Put_Line("   This program is free software; you can redistribute it and/or");
      Put_Line("   modify it under the terms of the GNU General Public License");
      Put_Line("   as published by the Free Software Foundation; version 2");
      Put_Line("   of the License.");
      Put_Line("");
      Put_Line("   This program is distributed in the hope that it will be useful,");
      Put_Line("   but WITHOUT ANY WARRANTY; without even the implied warranty of");
      Put_Line("   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the");
      Put_Line("   GNU General Public License for more details.");
      Put_Line("");
      Put_Line("   You should have received a copy of the GNU General Public License");
      Put_Line("   along with this program; if not, write to the Free Software");
      Put_Line("   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.");
      Put_Line("");
   end License;

   ------------
   -- Banner --
   ------------
   procedure Banner is
   begin
      if not Option_Silent then
         Put_Line("");
         Put_Line("ABE library documentation generator, " & Version & "." );
         Put_Line("(C) Copyright 2003 Michael Erdmann (http://www.purl.org/net/michael.erdmann/)");
         Put_Line("");
      end if;
   end Banner;

   ---********************************************************************---
   ---**                RECOGNISION OF DOCUMENT PATTERNS                **---
   ---** This section contains code to recognize important comment      **---
   ---** elements.                                                      **---
   ---**                                                                **---
   ---********************************************************************---

   function Basename(
      S : in String ) return String is
      L : Natural := Index( S, "/", Going => Backward );
   begin
      if L > 0 then
         return( S( L+1..S'Last ) );
      else
         return( S );
      end if;
   end Basename;

   --------------
   -- Contains --
   --------------
   function Contains(
      Line : in String;
      What : in String ) return Boolean is
   begin
      return Line'Length > 0 and then Index( Line, What ) > 0;
   end Contains;

   function Contains(
      Line : in Unbounded_String;
      What : in String ) return Boolean is
   begin
      return Contains( To_String(Line), What );
   end Contains;

   --------------------
   -- Is_header_Line --
   --------------------
   function Is_Header_Line(
      Line : in String ) return Boolean is
      -- returns true if the comment starts in the first columns
      -- which qualifies the line as an visible comment
   begin
      return Line'Length > 1 and then Line(1..2) = Tag_Ada95_Comment;
   end Is_Header_Line;

   ------------------
   -- Comment_Line --
   ------------------
   function Comment_Line(
      Line : in String ) return Boolean is
      -- returns true if there is nothing else then a comment
      -- in the line.
   begin
      for I in 1..Line'Length-1 loop
         if Line(I) = '-' and Line(I+1) = '-' then
            return True;
         end if;

         if Line(I) > ' ' then
            return False;
         end if;
      end loop;

      return False;
   end Comment_Line;

   ---------------
   -- Ends_With --
   ---------------
   function Ends_With(
      Line : in Unbounded_String;
      C    : in Character ) return Boolean is
      S    : constant String := Trim( To_String(Line), Right );
   begin
      if S'Length = 0 then
         return False;
      end if;

      return S(S'Last) = C;
   end Ends_With;

   -----------------
   -- Begins_With --
   -----------------
   function Begins_With(
      Line : in Unbounded_String;
      C    : in Character ) return Boolean is
      S    : constant String := Trim( To_String(Line), Left);
   begin
      if S'Length = 0 then
         return False;
      end if;

      return S(1) = C;
   end Begins_With;

   -----------------
   -- Begins_With --
   -----------------
   function Begins_With(
      Line : in Unbounded_String;
      C    : in String ) return Boolean is
      S    : constant String := Trim( To_String(Line), Left);
   begin
      if S'Length < C'Length then
         return False;
      end if;

      return S(1..C'Length) = C;
   end Begins_With;

   -----------------
   -- Begins_With --
   -----------------
   function Begins_With(
      Line : in String;
      C    : in String ) return Boolean is
   begin
      return Begins_With ( To_Unbounded_String( Line ),C );
   end Begins_With;

   -------------------
   -- Is_Decoration --
   -------------------
   function Is_Decoration(
      Line   : in String ) return Boolean is
      -- A decoration is a comment which contains more then 5 '='
      -- or '-' consecutive characters
      S      : constant String := Trim( Trim( Line, Left ), Right );
      Length : Natural := 0;
      J      : Natural := S'First;
   begin
      if not Begins_With( S, Tag_Ada95_Comment ) or S'Length < 2 then
         return False;
      end if;

      while S(J) = '-' or S(J) = ' ' loop
         J := J + 1;
         if not ( J in S'Range ) then
            return J > 5 ;
         end if;
      end loop;

      for I in J..S'Last-1 loop
         if ( S(I) = '=' or S(I) = '-' or S(I) = '*' ) and
            S(I) = S(I+1)
         then
            Length := Length + 1;
         else
            exit;
         end if;
      end loop;

      return Length >= 2;
   end Is_Decoration;

   ---------------------
   -- CVS_Information --
   ---------------------
   function CVS_Information(
      Line : in String ) return Boolean is
      I, J : Natural;
   begin
      I := Index( Line, "$", Going => Backward );
      J := Index( Line, "$" );
      return I > J ;
   end CVS_Information;

   ------------------
   -- Is_Sub_Title --
   ------------------
   function Is_Sub_Title(
      S : in Unbounded_String;
      T : in String ) return Boolean is
   begin
      return Begins_With( S, T ) and Ends_With( S, ':' );
   end Is_Sub_Title;

   function Strip_Comments(
      Line : in String ) return String;

   -----------
   -- Level --
   -----------
   function Level(
      S      : in Unbounded_String ) return Natural is
      -- return the intend level
      S1     : constant String := Strip_Comments( To_String(S) );
      Result : Natural := 0;
   begin
      for I in S1'Range loop
         if S1(I) <= ' ' then
            Result := Result + 1;
         else
            exit;
         end if;
      end loop;

      return Result;
   end Level;

   ----------------
   -- Blank_Line --
   ----------------
   function Blank_Line(
      S      : in Unbounded_String ) return Boolean is
      -- check if the line is a blank line
      S1     : constant String := Strip_Comments( To_String(S) );
   begin
      return Trim( Trim(S1, Right), Left )'Length = 0;
   end Blank_Line;

   ---********************************************************************---
   ---**            P R O C E S S   C O M M E N T S                     **---
   ---** This section contains operations for the manipulation of       **---
   ---** commentary elements.                                           **---
   ---********************************************************************---

   --------------------
   -- Strip_Comments --
   --------------------
   function Strip_Comments(
      Line : in String ) return String is
      Result : String( 1..Line'Length ) := Line;
      First  : Integer := Result'First;
      Last   : Integer := Line'Length;
      Idx    : Natural := 0;
   begin
      if Line'Length < 2 then
         return Line;
      end if;

      Last  := Index( Line, Tag_Ada95_Comment, Going => Backward );
      First := Index( Line, Tag_Ada95_Comment );

      if First > 0 then
         First := First + 2;
      else
         First := 1;
      end if;

      if Last-2 > First then
         Last := Last - 2;
      else
         Last := Line'Length;
      end if;

      if First < Last and then Result(First) = '|' then
         First := First + 1;
      end if;

      return Trim( Result( First..Last ), Right ) & " " ;
   exception
      when others =>
         raise;
   end Strip_Comments;

   -----------------------
   -- Fetch_Header_Line --
   -----------------------
   function Fetch_Header_Line(
      L1, L2 :in Unbounded_String ) return Unbounded_String is
      -- This function tries to identify header lines from the given
      -- two consecutive lines by applying the simple rule:
      --
      --  <header line  >
      --  "============="
      --
      S1     : constant String  := Trim( Trim( To_String(L1), Right),Left);
      S2     : constant String  := Trim( Trim( To_String(L2), Right),Left);
      First  : Natural          := 0;
      Last   : Natural          := 0;
      Ch     : Character        ;
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      if S1'Length < 2 or S2'Length < 2 then
         return Null_Unbounded_String;
      end if;

      Ch := S2(1);
      for I in S2'First+1..S2'Last loop
         if Ch = '=' and S2(I) = '=' and (I in S1'Range) then
            if First = 0 then
               First := I;
            end if;

            Last := I;
         else
            Ch    := S2(I);
            First := 0;
            Last  := 0;
         end if;
      end loop;

      if First > 0 then
         Result := To_Unbounded_String( S1(First-1..Last) );
      end if;

      return Result;
   end Fetch_Header_Line;

   ---********************************************************************---
   ---**                  DOCBOOK PRIMITIVES                            **---
   ---********************************************************************---
   ----------
   -- Emit --
   ----------
   procedure Emit(
      S : in String ) is
   begin
      Put_Line( Output_File, S );
   end Emit;

   ----------------
   -- To_Docbook --
   ----------------
   function To_Docbook(
      S : in String ) return String is
      R : Unbounded_String := Null_Unbounded_String;
   begin
      for I in S'Range loop
         if S(I) = '<' then
            R := R & "&lt;";
         elsif S(I) = '>' then
            R := R & "&gt;";
         else
            R := R & S(I);
         end if;
      end loop;

      return To_String(R);
   end To_Docbook;

   ----------------
   -- To_Docbook --
   ----------------
   function To_Docbook(
      S : in Unbounded_String ) return String is
   begin
      return To_Docbook( To_String(S) );
   end To_Docbook;

   ---------------
   -- DB_String --
   ---------------
   function DB_String(
      S : in Unbounded_String ) return String is
   begin
      return '"' & To_String(S) & '"' ;
   end DB_String;

   ----------------
   -- DB_Prelude --
   ----------------
   procedure DB_Prelude is
   begin
      Emit( "<part id=" & DB_String(Part_Name) & ">" );
      if Html_File_Name /= Null_Unbounded_String then
         Emit( "<?dbhtml filename=" & DB_String(Html_File_Name) & ">" );
      end if;

      Emit( "<title>"& To_Docbook( Chapter_Title ) & "</title>");
   end DB_Prelude;

   -----------------
   -- DB_Postlude --
   -----------------
   procedure DB_Postlude is
   begin
      Emit( "</part>" );
   end DB_Postlude;

   H0           : Unbounded_String := Null_Unbounded_String;
   H1           : Unbounded_String := Null_Unbounded_String;
   Para_Open    : Boolean := False;
   Listing_Open : Boolean := False;
   ------------
   -- End_H1 --
   ------------
   procedure End_H1 is
   begin
      if H1 /= Null_Unbounded_String then
         if H0 = Null_Unbounded_String then
            Emit( "</sect2>");
         end if;
         H1 := Null_Unbounded_String;
      end if;
   end End_H1;

   --------------
   -- Begin_H1 --
   --------------
   procedure Begin_H1(
      S : in String ) is
   begin
      End_H1;
      if H0 = Null_Unbounded_String then
         Emit("<sect2>");
         Emit("<title>" & To_Docbook(S) & "</title>" );
      else
         Emit( "<bridgehead renderas=sect4>" & To_Docbook(S) & "</bridgehead>" );
      end if;

      H1 := To_Unbounded_String(S);
   end Begin_H1;

   ------------
   -- End_H0 --
   ------------
   procedure End_H0 is
   begin
      End_H1;
      if H0 /= Null_Unbounded_String then
         Emit( "</sect2>");
         H0 := Null_Unbounded_String;
      end if;
   end;

   --------------
   -- Begin_H0 --
   --------------
   procedure Begin_H0(
      S : in String ) is
   begin
      End_H0;
      Emit("<sect2>");
      Emit("  <title>" & To_Docbook(S) & "</title>");
      H0 := To_Unbounded_String(S);
   end Begin_H0 ;

   ----------------
   -- Begin_Para --
   ----------------
   procedure Begin_Para is
   begin
      if Para_Open then
         Emit("</para>");
      end if;

      Para_Open := True;
      Emit("<para>");
   end Begin_Para;

   --------------
   -- End_Para --
   --------------
   procedure End_Para is
   begin
      if Para_Open then
         Emit("</para>");
      end if;
      Para_Open := False;
   end End_Para;

   ---********************************************************************---
   ---**                PRESENTATION OF DATA                            **---
   ---**                                                                **---
   ---** This section contains all procedures/functions used to print   **---
   ---** adadoc internal structures into the output file.               **---
   ---**                                                                **---
   ---********************************************************************---

   ---------------------
   -- Print_Paragraph --
   ---------------------
   procedure Print_Paragraph(
      P     : in Paragraph_Type ) is
      -- print a paragraph in docbook format
      R     : Text_Array_Access := To_Array(P);
      H     : Unbounded_String  := Null_Unbounded_String;
      I     : Natural := 0;

      End_Of_Block : exception;

      ---------------
      -- Next_Line --
      ---------------
      procedure Next_Line(
         Skip_Blank_Lines : in Boolean := False ) is
         -- fetch next non empty line
      begin
         I := I + 1;
         if not ( I in R.all'Range ) then
            raise End_Of_Block;
         end if;

         while Blank_Line(R(I)) and Skip_Blank_Lines loop
            I := I + 1;
            if not ( I in R.all'Range ) then
               raise End_Of_Block;
            end if;

         end loop;
      end Next_Line;

      -----------
      -- Width --
      -----------
      function Width(
         I : in Natural ) return Natural is
         Result : Natural := 0;
      begin
         for J in I..R.all'Last loop
            exit when Blank_Line( R(J) );

            if Length( R(J) ) > Result then
               Result := Length( R(J) );
            end if;
         end loop;

         return Result;
      end Width;

      ---------------------
      -- Print_List_Item --
      ---------------------
      procedure Print_List_Item(
         Top_Level : in Natural ) is
      begin
         while Begins_With(R(I), '-') or Level(R(I)) >  Top_Level loop
            Emit("<listitem>");
            Begin_Para;

            while not Blank_Line(R(I)) or Begins_With(R(I),'-')
            loop
               Emit( To_Docbook( R(I) ) );
               Next_Line;
            end loop;
            Next_Line;

            End_Para;
            Emit("</listitem>");
         end loop;
      exception
         when End_Of_Block =>
            End_Para;
            Emit("</listitem>");
      end Print_List_Item;

      -----------------
      -- Print_Block --
      -----------------
      procedure Print_Text_Block  is
         -- print a text block. Identify the following textual
         -- patterns:
         --
         -- Paragraphs
         -- lists and listelements
         Block_Width  : Natural := Width(I);
         Top_Level    : Natural := Level(R(I));
      begin
         Begin_Para;
         while I in R.all'Range loop
            exit when Blank_Line( R(I) ) or Level(R(I)) /= Top_Level;

            if not Contains( R(I), "Copyright (C)" ) then
               Emit( To_Docbook( R(I)) );
            end if;

            if  Length( R(I) ) in 2..(3 * Block_Width / 4) then
               Begin_Para;
            elsif Ends_With( R(I), ':' ) then
               declare
                  Top_Level : Natural := Level( R(I) );
               begin
                  Next_Line( Skip_Blank_Lines => True );
                  if Ends_With( R(I), ':') then
                     exit;
                  end if;

                  End_Para;
                  Emit("<itemizedlist mark=opencircle>");
                  Print_List_Item( Top_Level );
                  Emit("</itemizedlist>");
                  Begin_Para;
               end;
            end if;

            Next_Line;
         end loop;
         End_Para;

      exception
         when End_Of_Block =>
            End_Para;
            raise End_Of_Block;
      end Print_Text_Block;

      -------------------
      -- Print_Listing --
      -------------------
      procedure Print_Unformated is
      begin
         Emit("<programlisting>");
         while I in R.all'Range loop
            exit when not Begins_With( R(I), Tag_Unformated );

            declare
               L : constant String := To_String(R(I));
            begin
               Emit( To_Docbook( L( 3..L'Length) ) );
               Next_Line;
            end ;
         end loop;

         Emit("</programlisting>");

      exception
         when End_Of_Block =>
            Emit("</programlisting>");
            raise End_Of_Block;
      end Print_Unformated;

   begin
      if R = null then
         return;
      end if;

      I := R.all'First;
      while I in R.all'First..R.all'Last-1 loop
         declare
            H  : Unbounded_String := Null_Unbounded_String;
         begin
            if  Is_Sub_Title( R(I), "Description" ) then
               Begin_H1( "Description");
               Next_Line;
            elsif Is_Sub_Title( R(I), "Precondition" ) then
               Begin_H1( "Preconditions");
               Next_Line;
            elsif Is_Sub_Title( R(I), "Postcondition" ) then
               Begin_H1( "Postconditions");
               Next_Line;
            elsif Is_Sub_Title( R(I), "Exception") then
               Begin_H1( "Exceptions" );
               Next_Line;
            elsif Is_Sub_Title( R(I), "Note") then
               Begin_H1( "Notes" );
               Next_Line;
            else
               H := Fetch_Header_Line( R(I),R(I+1));
               if H /= Null_Unbounded_String then
                  Next_Line;   -- skip the underline

                  Begin_H0( To_String(H) );
                  Next_Line( Skip_Blank_Lines => True );
               end if;
            end if;

            if Begins_With( R(I), Tag_Unformated ) then
               Print_Unformated;
            else
               Print_Text_Block;
               if Blank_Line( R(I) ) then
                  Next_Line( Skip_Blank_Lines => True );
               end if;
            end if;

         exception
            when End_Of_Block =>
               exit;
         end ;
      end loop;

      End_H0;
   end Print_Paragraph;

   -------------------
   -- Print_Listing --
   -------------------
   procedure Print_Listing(
      List : in Paragraph_Type ) is
      R    : Text_Array_Access := To_Array(List);
   begin
      Emit("<programlisting>");
      for I in R.all'Range loop
         Emit( To_Docbook( R(I) ) );
      end loop;
      Emit("</programlisting>");
   end Print_Listing;

   -----------
   -- Print --
   -----------
   procedure Print(
      List : in Paragraph_Type ) is
      R    : Text_Array_Access := To_Array(List);
   begin
      if R /= null then
      for I in R.all'Range loop
         Put_Line( To_Docbook( R(I) ) );
      end loop;
      else
         Put_line("empty");
      end if;
      Put_Line("-------------------------");
   end Print;

   ---********************************************************************---
   ---**                  PROCESSING A FILE                             **---
   ---********************************************************************---

   ------------------
   -- Process_File --
   ------------------
   procedure Process_File(
      Name   : in String ) is
      -- process the given file
      Input      : File_Type;
      Line       : String(1..1024);
      Length     : Natural            := 0;
      Next_Line  : Unbounded_String   := Null_Unbounded_String;
      State      : Scanner_State_Type := S_Null;
      Comments   : Paragraph_Type;

      procedure Read_Line(
         Line   : in out String;
         Len    : out Natural ) is
         -- get either the saved line or a new line from the input
         -- file.
      begin
         if Next_Line /= Null_Unbounded_String then
            Move( Source => To_String(Next_Line), Target => Line );
            Len := To_String(Next_Line)'Length;

            Next_Line := Null_Unbounded_String;
         else
            Get_Line( Input, Line, Len );
         end if;
      end Read_Line;

      procedure Put_Back(
          S     : in String ) is
          -- put back the given line. The next get_Line call will
          -- return this value.
      begin
          Next_Line := To_Unbounded_String(S);
      end Put_Back;

   begin
      -- clear some global data used for each file
      H0           := Null_Unbounded_String;
      H1           := Null_Unbounded_String;
      Package_Name := Null_Unbounded_String;

      Para_Open    := False;

      Clear( Functional_Description );
      Clear( Default_Description );
      Clear( Restrictions );
      Clear( Contact );
      Clear( Pkg_Interface );

      Nbr_Of_API_Entries := 1;

      for I in API_Table'Range loop
         Clear( API_Table(I).Description );
         Clear( Api_Table(I).Specification );
      end loop;

      -- open file
      Put("Processing file " & Basename(Name) );
      Open( File => Input, Name => Name, Mode => In_File );
      Put(" ..");

      State := S_Null;

      <<Fetch_Next_Line>>
      -- process the contents of the file
      while not End_Of_File(Input) or Next_Line /= Null_Unbounded_String
      loop
         Read_Line( Line, Length );

         -- remove some special characters e.g. tabs, CR, LF etc. which
         -- wont be present in the sgml output file at all.
         for I in 1..Length loop
            if Line(I) < ' ' then
               Line(I) := ' ';
            end if;
         end loop;

         declare
            L   : constant String  := Line(1..Length) & " ";
            API : API_Description_Type renames API_Table(Nbr_Of_API_Entries);
            I,J : Natural;
         begin
            -- Put_Line(Scanner_State_Type'Image(State) & L );
            exit when Begins_With(L, Tag_Ada95_Private);

            if Contains(L,Tag_Pkg_Description) then
               State := S_Functional_Description;
            elsif Contains( L, Tag_References ) then
               State := S_References;
            elsif Contains( L, Tag_Contact ) then
               State := S_Contact;
            elsif Contains( L, Tag_Restrictions ) then
               State := S_Restrictions;
            elsif Contains( L, " is free software" ) or
                  Contains( L, " a special exception" )
            then
               State := S_Free_Software;
            end if;

            -- check for the package name
            if  Begins_With( L, "package " ) and Contains(L, " is" )
              and Package_Name = Null_Unbounded_String
            then
               I := Index( L, " is" );
               if not ( I > 0  ) then
                  I := L'Last;
               end if;
               J := Index( L, "package ") + 8;
               Package_Name := To_Unbounded_String( Trim(L(J..I), Right) );
               if Option_Reverse_Style then
                  State := S_API_Specification;
               else
                  State := S_API_Description;
               end if;
               goto Fetch_Next_Line;
            end if;

            -- collect data into paragrapgh depending on section processed

            case State is
               when S_Null =>
                  if not (Is_Decoration( L ) or CVS_Information( L )) and
                     Is_Header_Line(L)
                  then
                     Add( Default_Description, Strip_Comments(L) );
                  end if;

               when S_Free_Software =>
                  -- remove the GPL license header
                  if Trim(Strip_Comments(L),Right)'Length = 0 then
                     State := S_Null;
                  end if;

               when S_Functional_Description |
                    S_References |
                    S_Contact    |
                    S_Restrictions =>
                  -- read in the header of the package
                  if not Is_Header_Line(L) then
                     Put_Back(L);
                     State := S_Pkg_Interface;
                   else
                     Add( Functional_Description, Strip_Comments(L) );
                  end if;

               when S_Pkg_Interface =>
                  if Begins_With(L, "generic" ) then
                     State := S_Generic_Parameters;
                  end if;

               when S_Generic_Parameters =>
                  Add( Pkg_Interface, L );

               when S_API_Specification =>
                  -- Load the Ada code which is part of the specification it
                  -- terminated by comments which are not decorations
                  --
                  -- certain instructions are not stored, e.g. pragma statements.
                  if Begins_With(L, "--" ) and not Is_Decoration(L) then
                     if not Option_Reverse_Style then
                        Nbr_Of_API_Entries := Nbr_Of_API_Entries + 1;
                     end if;
                     State := S_API_Description;
                     Put_Back(L);
                  else
                     if not (Comment_Line(L) or Is_Decoration(L)) and
                        not (Contains(L,"pragma"))
                     then
                        Add( API.Specification, L );
                     end if;
                  end if;

               when S_API_Description =>
                  -- processing the API specification in the comments. Comments
                  -- which are no decosrations and certain Ada 95 keywords are
                  -- copied into the description section.
                  if Begins_With( L, "--" ) then
                     if not Is_Decoration( L ) and
                        not CVS_Information( L )
                     then
                        Add( API.Description, Strip_Comments(L));
                     end if;
                  elsif (Contains(L, "constant") and Contains(L,":=")) or
                         Contains(L, "exception") then
                        Add( API.Description, Tag_Unformated & L);
                  elsif   Contains( L, "procedure " )
                       or Contains( L, "function ")
                       or Contains( L, "generic ")
                  then
                     if Option_Reverse_Style then
                        Nbr_Of_API_Entries := Nbr_Of_API_Entries + 1;
                     end if;
                     Put_Back(L);
                     State := S_API_Specification;
                  end if;

               when others =>
                  null;
            end case;
         end;

      end loop;

      Close(Input);

      Put_Line(". complete. ");
   exception
      when NAME_ERROR =>
         Error(", which does not exist!");
   end Process_File;

   ------------
   -- Format --
   ------------
   procedure Format is
      -- display the result of a scan a docbook

      --------------------
      -- API_Entry_Name --
      --------------------
      function API_Entry_Name(
         T      : in Paragraph_Type ) return String is
         -- extract the procedure/function name from the text block
         P      : Text_Array_Access := To_Array( T );
         L      : Natural;
         Result : Unbounded_String := Null_Unbounded_String;

         procedure Append(
            S : in String ) is
         begin
            if Result = Null_Unbounded_String then
               Result := To_Unbounded_String(S);
            else
               Result := Result & ", " & S ;
            end if;
         end Append;

      begin
         if P = null then
            return "undefined";
         end if;

         for I in P.all'Range loop
            declare
               R : constant String := Trim( To_String( P(i)), Left );
            begin
               if R'Length >  0 then
                 if Contains( R, "procedure ") or Contains(R,"function ")
                 then
                    L := Index( R, "(" );
                    if L > 0 then
                       Append( R( 1..L-1 ) );
                    else
                       L := Index( R, " ;" );
                       if L > 0 then
                          Append(R( 1..L-1 ));
                       else
                          L := Index( R, " return" );
                          if L > 0 then
                             Append( R( 1..L-1 ) );
                          else
                             Append( R );
                          end if;
                       end if;
                    end if;
                 elsif Contains( R, "type " ) then
                    L := Index( R, "is " );
                    if L > 0 then
                       Append( R( 1..L-1 ) );
                    end If;
                 end if;
               end if;
            end;
         end loop;
         if Result /= Null_Unbounded_String then
            return To_String(Result);
         else
            return "Common Defintions";
         end if;

      end API_Entry_Name;

      -------------
      -- Warning --
      -------------
      procedure Warning(
         S : in String ) is
      begin
         Emit("<caution>");
         Emit("  <title>Warning</title>");
         Begin_Para;
         Emit(To_Docbook(S));
         End_Para;
         Emit("</caution>");
      end Warning;

   begin
      Emit("<chapter>");
      Emit("  <title>" & To_Docbook( Package_Name ) & "</title> ");

      Emit("<sect1>");
      Emit("  <title>Overview</title> ");
      if not Is_Empty( Functional_Description ) then
         Print_Paragraph( Functional_Description );
      else
         Print_Paragraph( Default_Description );
      end if;

      Print_Paragraph( Restrictions );
      Print_Paragraph( References );
      Print_Paragraph( Contact );

      Emit("</sect1>");

      if Nbr_Of_API_Entries > 0 and not Is_Empty(API_Table(1).Specification)
      then
         Emit("<sect1>");
         Emit("   <title>API Reference</title>");


         if not Is_Empty( Pkg_Interface ) then
            Emit("<sect2>");
            Emit("   <title>Generic Package Parameter(s)</title>");
            Print_Listing( Pkg_Interface );
            Emit("</sect2>");
         end if;

         for I in 1..Nbr_Of_API_Entries loop
            if not Is_Empty( API_Table(I).Specification ) then
               Begin_H0( API_Entry_Name(API_Table(I).Specification) );
               Print_Listing( API_Table(I).Specification );

               if not Is_Empty( API_Table(I).Description ) then
                  Print_Paragraph( API_Table(I).Description );
               end if;
               End_H0;

               Total_API_Entries := Total_API_Entries + 1;
            end if;
         end loop;

         Emit("</sect1>");
      end if;

      Emit("</chapter>");
   end Format;

   -------------
   -- Process --
   -------------
   procedure Process is
      Arg          : Natural := 1;
      Banner_Shown : Boolean := False;
   begin
      while Arg <= Argument_Count loop
         if Argument(Arg) = "-o" then
            Arg := Arg + 1;

            Part_Name := To_Unbounded_String(Argument(Arg));

            if Is_Open( Output_File ) then
               Close( Output_File );
            end if;

            Create(
               File => Output_File,
               Name => Argument(Arg) & ".sgml",
               Mode => Out_File
            );
            DB_Prelude;

         elsif Argument(Arg) = "-c" then
            Arg := Arg + 1;
            Chapter_Title := To_Unbounded_String(Argument(Arg));
         elsif Argument(Arg) = "-r" then
            Option_Reverse_Style := True;
         elsif Argument(Arg) = "-s" then
            Option_Silent        := True;
         elsif Argument(Arg) = "-v" then
            Option_Verbose       := True;
            Option_Silent        := False;
         elsif Argument(Arg) = "-h" then
            Help;
            return;
         elsif Argument(Arg) = "-l" then
            Banner;
            License;
            return;
         elsif Argument(Arg)(1) = '-' then
            Error("unknown option " & Argument(Arg) );
            Help;
            return;
         else
            if not Banner_Shown then
               Banner_Shown := True;
               Banner;
            end if;

            if Part_Name = Null_Unbounded_String then
               Error( "No output unit has been specified, use -o option!");
               return;
            end if;

            Process_File( Argument(Arg) );

            Total_Nbr_Of_Files := Total_Nbr_Of_Files + 1;
            Format;
         end if;

         Arg := Arg + 1;
      end loop;


      if Is_Open( Output_File ) then
         DB_Postlude;
         Close(Output_File);
      else
         Error( "No output unit has been specified, use -o option!");
      end if;

   end Process;

begin
   if Argument_Count < 1 then
      Banner;
      Help;
      Set_Exit_Status(1);
      return;
   else
      Process;
   end if;

   Put_Line("");
   if Option_Verbose and Total_Nbr_Of_Files > 0 then
      Put_Line("Number of modules     :" & Natural'Image(Total_Nbr_Of_Files));
      Put_Line("Number of API entries :" & Natural'Image(Total_API_Entries));
   end if;

   if Errors > 0 then
      Put_Line("Number of Errors      :" & Natural'Image(Errors) );
      Set_Exit_Status( 1 );
   else
      Set_Exit_Status( 0 );
   end if;
   Put_Line("");
end Adadoc;
