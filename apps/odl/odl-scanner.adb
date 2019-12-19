-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/apps/odl/odl-scanner.adb,v $
--  Description     : Scanner for the embedded SQL translator                --
--  Author          : Michael Erdmann                                        --
--  Created         : 8.12.2000                                              --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/06/29 16:53:00 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2003 Michael Erdmann                                       --
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
--                                                                           --
--  Functional Description                                                   --
--  ======================                                                   --
--  This module provides the lexical scanner for the esql translator. Each   --
--  token read in is copied into a output line if the user decides to accept --
--  the copy. If the tokenizer has reached the end of a line, the text       --
--  stored in the output buffer will be written into the output file.        --
--  Additionaly the user may insert additional strings into the output       --
--  line.                                                                    --
--                                                                           --
--  This module provides additionaly the service to print out messaged       --
--  according to the gcc style containing line and column number.            --
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
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Characters.Latin_1;            use Ada.Characters;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with ODL.Options;                       use ODL.Options;

package body ODL.Scanner is

   Version : constant String :=
      "$Id: odl-scanner.adb,v 1.4 2003/06/29 16:53:00 merdmann Exp $";

   type File_Reader_Type is record
          File              : File_Type;
          Line              : String(1..512);
          Line_Length       : Natural          := 1;
          Read_Pos          : Natural          := 0;
          Output            : File_Type;
          Accepted_Length   : Natural          := 0;
          Accepted_Line     : String(1..8000);
          Temp_Buffer       : String(1..8000);
          Temp_Pos          : Natural          := 0;
          Current_Character : Character        := ' ';
          Begin_Of_Token    : Natural          := 0;
          Current_Line_Nbr  : Natural          := 0;
          File_Name         : Unbounded_String := Null_Unbounded_String;
          Last_Line         : Boolean          := False;
       end record;

   Space : constant Character := ' ';
   Tab   : constant Character := Latin_1.HT;

   ------------------
   -- Accept_Input --
   ------------------
   procedure Accept_Input(
      F : in out File_Reader ) is
      -- procedures accepts the characters read so far
      -- are copied into the output file.
   begin
      for I in 1..F.Temp_Pos loop
         F.Accepted_Length := F.Accepted_Length + 1;
         F.Accepted_Line( F.Accepted_Length ) := F.Temp_Buffer(I);
      end loop;

      f.Temp_Pos := 0;
   end Accept_Input;

   ------------------
   -- Ignore_Input --
   ------------------
   procedure Ignore_Input(
       F : in out File_Reader ) is
       --  Don't copy the charcter to the output stream.
   begin
       f.Temp_Pos := 0;
   end Ignore_Input;

   -------------------
   -- Accept_String --
   -------------------
   procedure Accept_String(
      f : in out File_Reader;
      s : in String ) is
      -- This procedure inserts a string into the accept buffer.
   begin
      -- Put_Line( "(" & F.Accepted_Line( 1..F.Accepted_Length ) & ")" );
      for i in S'Range loop
         f.Accepted_Length := f.Accepted_Length + 1;
         f.Accepted_Line( f.Accepted_Length ) := S(i);
      end loop;

      -- Put_Line( "<" & F.Accepted_Line( 1..F.Accepted_Length ) &">" );
   end Accept_String;


   ----------------
   -- Substitute --
   ----------------
   procedure Substitute(
      S      : in  String;
      Result : out String;
      Length : out Natural ) is
      -- replace variable definitions within an input line.
      --
      -- R.1 - If we find a comment copy everything without
      --       any substitution
      -- R.2 - If we find a $ substitute.
      --
      -- NOTE:
      --   This is a horrible hack, but it works. Since this limits
      --   the substitution to simple lines this will be a subject
      --   of rework later on.
      I  : Natural := S'First;
      J  : Natural := S'First;

      End_Of_Input : exception;

      function Is_Identifier(
         Ch : in Character ) return Boolean is
      begin
         return (Ch in 'a'..'z') or (Ch in 'A'..'Z') or
                (Ch in '0'..'9') or Ch = '_';
      end Is_Identifier;

      -- copy a char into the result
      procedure Copy( Ch : in Character ) is
      begin
         if J not in Result'Range then
            raise End_Of_Input;
         else
            Result(J) := Ch;
            J      := J + 1;
            Length := Length + 1;
         end if;
      end Copy;

      -- get the next char from the input
      function Next_Char return Character is
         Ch : Character;
      begin
         if I not in S'Range then
            raise End_Of_Input;
         else
            Ch := S(I);
         end if;

         I := I + 1;
         return Ch;
      end Next_Char;

   begin
      Length  := 0;

      while I in S'Range loop
         -- R.1
         if S(I) = '-' and ( (I+1 in S'Range) and then S(I+1) = '-' )
         then
               while I in S'Range loop
                  Copy( Next_Char );
               end loop;
               exit;
         end if;

         if S(I) = ''' then
               Copy( Next_Char );
               Copy( Next_Char );
               Copy( Next_Char );
         elsif S(I) = '"' then
               Copy( Next_Char );
               while I in S'Range and then S(I) /= '"' loop
                  Copy( Next_Char );
               end loop;
         else
               Copy( Next_Char );
         end if;
      end loop;
   exception
      when End_Of_Input =>
         Put_Line( "end of input " & Result( 1..Length ) );
   end Substitute;

   --------------
   -- Get_Char --
   --------------
   procedure Get_Char(
       F    : in  File_Reader;
       CH   : out Character;
       Tmp  : Boolean := True ) is
       -- this function reads in the characters from the input
       -- file. Each line which has been read in completly
       -- but character wise processed.
       -- Each character read is first copied into a temporary
       -- buffer which is appended to the output line buffer,
       -- if the application accepts the token.
       -- The output buffer is written out  upon end of the line
       -- which contains only the accepted parts of the input
       -- line.
       R    : Natural renames F.Read_Pos;
       T    : Natural renames F.Temp_Pos;
       L    : Natural renames F.Line_Length;
    begin
       if    R = 0                -- get_char is called the first time
          or R = L + 1            -- or end of the line is reached
       then
          if not F.Last_Line then
             declare
                Tmp : String( 1..1024 );
             begin
                f.Current_Line_Nbr := f.Current_Line_Nbr + 1;

                -- invoke the substituation of prep. symbols.
                Get_Line( F.File, Tmp, L );
                Substitute( Tmp(1..L), F.Line, L );

                F.Line(L+1) := ' ';
                L := L + 1;
                -- this removes any CR/LF combintation as they are found
                -- with Windows NT.
                for I in 1..L loop
                   if F.Line(I) < ' ' then
                      F.Line(I) := ' ';
                   end if;
                end loop;
                if Option_DEBUG then
                   Put_Line(
                      "Processing line " &
                      Integer'Image(F.Current_Line_Nbr));
                end if;
             exception
                when others =>
                   F.Last_Line := True;
             end;
             R  := 1;
             CH := ' ';
          else
             --
             -- If we have reached End_Of_File make sure that
             -- any saved line is written to the output
             --
             if R = 1 and then F.Accepted_Length > 0 then
                 Put_Line( F.Output, F.Accepted_Line(1..f.Accepted_Length));
                 F.Accepted_Length := 0;
                 T := 0;
             end if;
             -- if this was the last line then indicate end of file
             if Option_Debug then
                Put_Line("EOF");
             end if;

             CH := ASCII.EOT;
          end if;
       else
          if R = 1 and then F.Accepted_Length > 0 then
             Put_Line( f.Output, F.Accepted_Line(1..f.Accepted_Length) );
             F.Accepted_Length := 0;
             T := 0;
          end if;

          CH := F.Line( R );
          R  := R + 1;

          if Tmp then
             T  := T + 1;
             F.Temp_Buffer( T ) := F.Current_Character;
          end if;
       end if;
   end Get_Char;

   ----------------
   -- Look_Ahead --
   ----------------
   function Look_Ahead(
       F    : in  File_Reader) return Character is
       -- this function reads in the characters from the input
       -- file. Each line which has been read in completly
       -- will be printed into the output stream. The trick of
       -- removing the embedded SQL string will be done later
       R    : Natural renames F.Read_Pos;
       T    : Natural renames F.Temp_Pos;
       L    : Natural renames F.Line_Length;
    begin
       if R = 0 or R = L + 1 then
          if F.Last_Line then
             return ASCII.EOT;
          else
             return ' ';
          end if;
       else
          return f.Line( R );
       end if;
   end Look_Ahead;

   -----------
   -- Flush --
   -----------
   procedure Flush(
      F     : in out File_Reader ) is
      -- Write out the already accepted code and the remaining part
      R    : Natural renames F.Read_Pos;
      T    : Natural renames F.Temp_Pos;
      L    : Natural renames F.Line_Length;
   begin
      if F.Accepted_Length > 0 then
         Put(f.Output, F.Accepted_Line(1..f.Accepted_Length) );
         f.Accepted_Length := 0;
      end if;
      Put_Line( f.Output, f.Line( R..L ) );
      T := 0;
      R := 0;
   end Flush;

   ---------------
   -- Get_Token --
   ---------------
   procedure Get_Token(
      F     : in out File_Reader;
      Token : out Token_Type ) is
      -- get a token from the input buffer
      CH    : Natural := 1;

      -- SAVE CHARACTER ----------------------------------------------------

      procedure Save_Character(
         Tmp : in Boolean := True ) is
      begin
         if CH < token.Lexicon'Length then
            Token.Lexicon(CH) := F.Current_Character;
            CH                := CH + 1;
         end if;
         Get_Char(F, F.Current_Character, Tmp );
      end Save_Character;

      -- GET IDENTIFIER ------------------------------------------------------
      --
      -- bug fix: 0502.2 the ' is part of an identifier in order to meet the
      --          lexical rules of SQL.
      procedure Get_Identifier is
      begin
         Token.Lexical_Unit := Identifier_Lex;
         Token.Lexicon      := Blank_Identifier;
         Save_Character;

         -- Add characters to the representation until invalid char found
         loop
            if (f.Current_Character not in 'a'..'z') and
               (f.Current_Character not in 'A'..'Z') and
               (f.Current_Character not in '0'..'9') and
               (f.Current_Character /= '_') and
               (F.Current_Character /= ''') and
               (F.Current_Character /= '.')
            then
               exit;
            else
               Save_Character;
            end if;
         end loop;

      end Get_Identifier;


      -- GET NUMERIC LITERAL ------------------------------------------

      procedure Get_Numeric_Literal is

         -- GET INTEGER ------------------------------------------------
         procedure Get_Integer is
         begin
            loop
               if   (f.Current_Character in '0'..'9')
                 or (f.Current_Character = '_') then
                  Save_Character;
               else
                  exit;
               end if;
            end loop;

         end Get_Integer;

         -- GET EXPONENT -------------------------------------------------
         procedure Get_Exponent is
         begin
            Save_Character;
            if  (f.Current_Character = '+') or
                (f.Current_Character = '-')
            then
               Save_Character;
            end if;
            Get_Integer;
         end Get_Exponent;


         -- GET EXTENDED INTEGER -----------------------------------
         procedure Get_Extended_Integer is
         begin
            loop
              if (f.Current_Character in '0'..'9') or
                 (f.Current_Character = '_') or
                 (f.Current_Character in 'a'..'f')
              then
                 Save_Character;
              else
                 exit;
              end if;
           end loop;
         end Get_Extended_Integer;


      begin -- GET NUMERIC LITERAL

         Token.Lexical_Unit := Numeric_Literal_Lex;
         Token.Lexicon      := Blank_Identifier;
         Save_Character;

         Get_Integer;

         if f.Current_Character = '.' then
            Save_Character;
            Get_Integer;
            if f.Current_Character = 'E' then
               Save_Character;
               Get_Exponent;
            end if;

         elsif f.Current_Character = '#' then
            Save_Character;
            Get_Extended_Integer;
            if f.Current_Character = '.' then
               Save_Character;
               Get_Extended_Integer;
            end if;
            if f.Current_Character = '#' then
               Save_Character;
               Get_Exponent;
            end if;
         end if;
      end Get_Numeric_Literal;

      -- GET STRING -----------------------------------------------
      procedure Get_Ada_String is
         String_Begin : Integer := F.Current_Line_Nbr;
      begin
         Token.Lexical_Unit := String_Lex;
         Token.Lexicon      := Blank_Identifier;
         Save_Character;

         -- Add chars until """ " or """ or """" found
         loop
            if f.Current_Character = '"' then
               Save_Character;

               if f.Current_Character = '"' then
                  Save_Character;
               else
                  exit;
               end if;
            else
               Save_Character;
            end if;

            if F.Current_Character = ASCII.EOT  then
               Message( F, " error : end of file in string starting at" &
                            Integer'Image(String_Begin) );
               raise Lexical_Error;
            end if;

            if Is_Control( F.Current_Character ) then
               Message( F, " error : control character in string starting at" &
                            Integer'Image(String_Begin) );
               raise Lexical_Error;
            end if;

         end loop;
      end Get_Ada_String;

      --|
      --| This is an SQL Style string
      --|
      procedure Get_SQL_String is
         String_Begin : Integer := F.Current_Line_Nbr;
      begin
         Token.Lexical_Unit := String_Lex;
         Token.Lexicon      := Blank_Identifier;
         Save_Character;

         -- Add chars until """ " or """ or """" found
         loop
            if f.Current_Character = ''' then
               Save_Character;

               if f.Current_Character = ''' then
                  Save_Character;
               else
                  exit;
               end if;
            else
               Save_Character;
            end if;

            if F.Current_Character = ASCII.EOT  then
               Message( F, " error : end of file in string starting at" &
                            Integer'Image(String_Begin) );
               raise Lexical_Error;
            end if;

            if Is_Control( F.Current_Character ) then
               Message( F, " error : control character in string starting at" &
                            Integer'Image(String_Begin) );
               raise Lexical_Error;
            end if;

         end loop;
      end Get_SQL_String;

      -- GET COMMENT --------------------------------------------------
      procedure Get_Comment is
      begin
         Token.Lexical_Unit := Comment_Lex;
         Token.Lexicon      := Blank_Identifier;
         Save_Character;

         if f.Current_Character = '-' then
            F.Temp_Pos                  := F.Temp_Pos + 1;
            F.Temp_Buffer( F.Temp_Pos ) := F.Current_Character;

            Accept_Input(F);
            Flush(F);
            Save_Character;
         else
            Token.Lexical_Unit := Delimiter_Lex;
         end if;
      end Get_Comment;

      -- GET DELIMITER -----------------------------------------------
      procedure Get_Delimiter is
      begin

         Token.Lexical_Unit := Delimiter_Lex;
         Token.Lexicon      := Blank_Identifier;

         -- Check for the single and double operators

         case f.Current_Character is
            when '=' =>
               Save_Character;
               if f.Current_Character = '>' then
                  Save_Character;
               end if;
            when '.' =>
               Save_Character;
               if f.Current_Character = '.' then
                  Save_Character;
               end if;
            when '*' =>
               Save_Character;
               if f.Current_Character = '*' then
                  Save_Character;
               end if;
            when ':' =>
               Save_Character;
               if f.Current_Character = '=' then
                  Save_Character;
               end if;
            when '/' =>
               Save_Character;
               if f.Current_Character = '=' then
                  Save_Character;
               end if;
            when '>' =>
               Save_Character;
               if (f.Current_Character = '=') or (f.Current_Character = '>') then
                  Save_Character;
               end if;
            when '<' =>
               Save_Character;
               if    (f.Current_Character = '=')
                  or (f.Current_Character = '<')
                  or (f.Current_Character = '>') then
                  Save_Character;
               end if;
            when '&' | '(' | ')' | '+' | ',' | '-' | ';' | '|' =>
               Save_Character;
            when ASCII.EOT =>
               Token.Lexical_Unit := End_Of_File_Lex;
            when others =>
               Token.Lexical_Unit := Unknown_Lex;
               Save_Character;
         end case;
      end Get_Delimiter;

   begin -- GET TOKEN
      -- Skip white space
      while (f.Current_Character = Space) or (f.Current_Character = Tab) loop
         Get_Char(f, f.Current_Character);
      end loop;

      f.Begin_Of_Token := f.Read_Pos;

      -- The first character determines token type
      case F.Current_Character is

         when  '$' | 'a'..'z' | 'A'..'Z' | '0'..'9' =>
            Get_Identifier;

         when Latin_1.Quotation   =>
            Get_Ada_String;

         when ''' =>                      -- bug fix: 0802.1
            Get_SQL_String;

         when '-'                 =>
            Get_Comment;

         when others              =>
            Get_Delimiter;

      end case;

      pragma Debug( Put_Line( "Current_character " & F.Current_Character ) );
      pragma Debug( Put_Line( "Temp_buffer<" & F.Temp_Buffer(1..F.Temp_Pos)&">"));

   end Get_Token;

   -------------
   -- Comment --
   -------------
   procedure Comment(
      F    : in out File_Reader;
      Text : in String ) is
      -- Place a comment in the output file. If the GNAT compiler is
      -- used we use the Source_Reference pragma to line up with the
      -- input line.
   begin
      Put_Line (F.Output,
         "--% " & To_String( F.File_Name ) &
         " at" & Natural'Image(f.Current_Line_Nbr) &
         " : "  & text
      );
   end Comment;

   ----------------------
   -- Source_Reference --
   ----------------------
   procedure Source_Reference(
      F    : in out File_Reader ) is
      -- Place a pragma into the output indicating the currently
      -- processed source line.
      --
      -- R.1 - If the procedure is called before the first line, the
      --       linenumber is still 0. We force it to 1.
      --
      Line : Natural := F.Current_Line_Nbr;
   begin
      if Line = 0 then       -- R.1
         Line := 1;
      end if;

      if Option_GNAT_Sref then
         Put_Line (F.Output,
            "pragma Source_Reference(" &
            Natural'Image(Line) & "," & '"' & To_String( F.File_Name ) & '"' &
            ");"
         );
      end if;
   end Source_Reference;

   ------------
   -- Insert --
   ------------
   procedure Insert(
      f    : in out File_Reader;
      -- Place a comment in the output file
      text : in String ) is
   begin
      Put_Line(f.Output, text );
   end Insert;

   -------------
   -- Message --
   -------------
   procedure Message(
      f    : in File_Reader;
      text : in String ) is
      -- Indicate a syntax error
      Col  : Natural renames f.Begin_of_Token;
      Row  : Natural renames f.Current_Line_Nbr;
   begin
      Put_Line( Standard_Error,
         To_String(f.File_Name) & ":" &
         Natural'Image(Row)     & ":" &
         Natural'Image(Col)     & ":" &
         text );
   end Message;

   ------------------
   -- Current_Line --
   ------------------
   function Current_Line(
      F : in File_Reader ) return Natural is
      -- Get the current line number
   begin
      return F.Current_Line_Nbr;
   end Current_Line;

   ----------
   -- Open --
   ----------
   function Open(
      Input_File_Name  : in String;
      Output_File_Name : in String ) return File_Reader is
      -- open the input and the output file
      Result : File_Reader := new File_Reader_Type;
   begin
       Open( File => Result.File,
             Mode => In_File,
             Name => Input_File_Name );

       Create( Result.Output,
             Mode => Out_File,
             Name => Output_File_Name );

       Result.File_Name := To_Unbounded_String(Input_File_Name);
       return Result;
   end Open;

   -----------
   -- Close --
   -----------
   procedure Close(
      f : in out File_Reader ) is
      -- close the input and the output file
   begin
      Close( f.File );
      Close( f.Output );
   end Close;

   ------------
   -- Delete --
   ------------
   procedure Delete(
      f : in out File_Reader ) is
   begin
      Delete( f.Output );
   end Delete;

end ODL.Scanner;


