-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/apps/odl/odl-scanner.ads,v $
--  Description     : Scanner for the ODL translator                         --
--  Author          : Michael Erdmann                                        --
--  Created         : 12.12.2000                                             --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/06/28 16:09:42 $                           --
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
--  This software is implemented to work with GNAT, the GNU Ada compiler.    --
--                                                                           --
--  Functional Description                                                   --
--  ======================                                                   --
--  This package provides the lexical scanner. Addtionaly for each token     --
--  the application may decide to copy the characters read so far into       --
--  the output file.                                                         --
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
with Ada.Strings.Unbounded;                 use Ada.Strings.Unbounded;

--* Ada
package ODL.Scanner is

   type File_Reader is private;


   type Lexical_Unit_Type is (
      Identifier_Lex,
      Numeric_Literal_Lex,
      Character_Literal_Lex,
      String_Lex,
      Delimiter_Lex,
      Comment_Lex,
      Unknown_Lex,
      End_Of_File_Lex );

   subtype Identifier_String is  String(1..48);
   Blank_Identifier : constant Identifier_String := (others=>' ');

   type Token_Type is record
         Lexicon      : Identifier_String := Blank_Identifier ;
         Lexical_Unit : Lexical_Unit_Type := Unknown_Lex;
      end record;

   Null_Token    : constant Token_Type := ( Blank_Identifier, Unknown_Lex );

   Lexical_Error : exception;

   -- Open a file reader on the given input file name. The output
   -- file is newly created. An exisiting file is overwritten.
   function Open(
      Input_File_Name  : in String;
      Output_File_Name : in String  ) return File_Reader;

   -- Close the file reader
   procedure Close(
       f : in out File_Reader );

   -- delete the file reader object
   procedure Delete(
       f : in out File_Reader );

   -- Get the next token. The procedure returns the End_Of_File_Lex
   -- token if file end has been reached.
   procedure Get_Token(
      f     : in out File_Reader;
      Token : out Token_Type );

   -- Mark the data read till the end of the current token as
   -- accepted, which means it will be copied into the output
   -- file
   procedure Accept_Input(
      f     : in out File_reader );

   -- Ingnore the source text till the end of the current token,
   -- which means the text is not copied into the output file.
   procedure Ignore_Input(
       f : in out File_Reader );

   -- Insert a string into the accept buffer
   procedure Accept_String(
      f : in out File_Reader;
      s : in String );

   -- Write out the accepted text and the remaining text till
   -- end of the line.
   procedure Flush(
      f    : in out File_Reader );

   procedure Message(
      f    : in File_Reader;
      text : in String );

   -- Place a comment in the output source
   procedure Comment(
      F     : in out File_Reader;
      text  : in String );

   -- insert a source reference pragma
   procedure Source_Reference(
      F     : in out File_Reader );

   -- inset the given string in the output
   procedure Insert(
      f     : in out File_Reader;
      text  : in String );

   -- get the current line.
   function Current_Line(
      F     : in File_Reader ) return Natural;


   Last_Variable_Name : Unbounded_String := Null_Unbounded_String;

   --------------------------------------------------------------------------
private
    type File_Reader_Type;
    type File_Reader is access File_Reader_Type;

end ODL.Scanner;


