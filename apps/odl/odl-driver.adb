-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/apps/odl/odl-driver.adb,v $
--  Description     : Main driver for the ODL Translator of ODB.	     --
--  Author          : Michael Erdmann                                        --
--  Created         : 18.12.2000                                             --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/10/02 19:30:28 $
--  Status          : $State: Exp $
--                                                                           --
--  Copyright (C) 2003 Michael Erdmann                                       --
--                                                                           --
--  ODB is free software;  you can redistribute it  and/or modify it under   --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion. GNADE is distributed in the hope that it will be useful, but WITH- --
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
--                                                                           --
--  Functional Description                                                   --
--  ======================                                                   --
--  This package contains the main driver of the translator, which means     --
--  the command line is processed and for each file, the ODL translator      --
--  is invoked.                                                              --
--  If an error happens in the ODL translator, the processed file is         --
--  closed and the next file is invoked for processing.                      --
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

--* Ada
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Calendar;                      use Ada.Calendar;

with Ada.Unchecked_Conversion;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Command_Line;                  use Ada.Command_Line;

--* Translator packages
with ODL.Scanner;                       use ODL.Scanner;
with ODL.Parser;                        use ODL.Parser;
with ODL.Options;                       use ODL.Options;
use  ODL;

package body ODL.Driver is

   Version : constant String :=
       "$Id: odl-driver.adb,v 1.6 2003/10/02 19:30:28 merdmann Exp $";

   Copyright_Shown      : Boolean := False;
   Show_Help_Info       : Boolean := False;

   Nbr_Of_Errors        : Natural := 0;
   Nbr_Of_Warnings      : Natural := 0;
   Nbr_Of_Files         : Natural := 0;
   Nbr_Of_Files_Success : Natural := 0;

   ------------------
   -- Process_File --
   ------------------
   procedure Process_File(
      Parser : in out ODL_Reader;
      Result : out  Exit_Status ) is
   begin
      loop
         ODL_Statement( Parser );
      end loop;

   exception
      when End_of_File_Exception =>
         Nbr_Of_Errors   := Nbr_Of_Errors   + Number_Of_Errors( Parser );
         Nbr_Of_Warnings := Nbr_Of_Warnings + Number_Of_Warnings( Parser );

         if Number_Of_Errors( Parser ) > 0 then
            Result := Failure;
         else
            Result := Success;
         end if;
      when Others =>
      	 raise;
   end Process_File;

   ---------------------
   -- Process_Package --
   ---------------------
   procedure Process_Package(
      Name   : in String;
      Result : out Exit_Status ) is
      F      : Scanner.File_Reader;
      P      : ODL_Reader := New_ODL_Parser(F);
   begin
      F := Open( Name & ".ods" , Name & ".ads" );
      if Option_GNAT_SREF then
         Source_Reference( F );
      end if;

      P := New_ODL_Parser(F);
      ODL_Prelude( P );
      Process_File( P, Result );

      F := Open( Name & ".odb" , Name & ".adb" );

      if Option_GNAT_SREF then
         Source_Reference( F );
      end if;
      Parser_Input( P, F );
      Process_File( P, Result );
      Close( F );

   exception
      when The_Error : Others =>
         Message( F,
            "Fatal error, uncatched exception " &
            Exception_Name( The_Error ) &
            " occured" );
         Message( F, "*** Processing of file aborted ***" );
         Result := Failure;
   end Process_Package;

			      -----------
   -- Error --
   -----------
   procedure Error(
      s : String ) is
      -- print out an error message
   begin
      Put_Line( Standard_Error, "Error : " & s );
   end Error;

   ------------------
   -- Option_Flags --
   ------------------
   procedure Handle_Option_Flag(
      S       : in String;
      Current : in out Natural ) is
      -- Handle the option flags of the odl translator
   begin
      Current := Current + 1;
      if S = "-pedantic" then
         Option_Pedantic        := True;
      elsif s = "-v" then
         Option_Verbose         := True;
      elsif s = "-debugcode" then
         Option_Debug_Code      := True;
      elsif S = "-debug" then
         Option_DEBUG           := True;
      elsif S = "-s" then
         Option_Silent          := True;
      elsif S = "-xschema" then
         Create( Schema_File,
             Mode => Out_File,
             Name => Argument(Current) );
         Option_SCHEMA := True;
         Current := Current + 1;
      elsif S = "-limit" then
         Option_Error_Limit := Integer'Value(Argument(Current));
         Current := Current + 1;
      elsif S = "-gnatnosref" then
         Option_GNAT_Sref := False;
      elsif S = "-h" or S = "-help" or S = "--help" then
         Show_Help_Info := True;
      end if;
   end Handle_Option_Flag;

   ----------
   -- Name --
   ----------

   function Name(
      s   : String ) return String is
      --
      -- Return the part for the name after the extentsion has been
      -- removed from the name, e.g. test.help.esql returns test.help.
      --
      pos : Integer;
   begin
      pos := Index( s, ".", Backward );
      if pos = 0 then
         return s;
      else
         return s(1..pos-1);
      end if;
    end Name;

    ---------------
    -- Copyright --
    ---------------

    procedure Copyright(
       Version : in String ) is
       -- the copyright will be shown only once
    begin
       if not Option_SILENT and not Copyright_Shown then
          Put_Line("");
          Put_Line("Ada 95 Object Defintion Language Translator; Version " & Version);
          Put_Line("Copyright (C) 2003 Michael Erdmann (http://www.purl.org/net/michael.erdmann)");
          Put_Line("");

          Copyright_Shown := True;
       end if;
    end Copyright;

    -------------
    -- Summary --
    -------------
    procedure Summary is
    begin
       Put_Line("");

       if Nbr_Of_Files_Success /= Nbr_Of_Files then
          Put( "Processed" &
             Natural'Image(Nbr_Of_Files-Nbr_Of_Files_Success) & " of" &
             Natural'Image(Nbr_Of_Files) & " file(s) not successfully" );
          Put("," & Natural'Image( Nbr_Of_Errors ) & " error(s)");
       else
          Put( "Processed" & Natural'Image(Nbr_Of_Files) & " file(s) successfully" );
       end if;

       if Nbr_Of_Warnings > 0 then
          Put( "," & Natural'Image( Nbr_Of_Warnings ) & " warning(s)");
       end if;
       Put_Line( "" );
       Put_Line( "" );

    end Summary;

    ---------------
    -- Help_Text --
    ---------------
    procedure Help_Text is
    begin
      Put_Line( "" );
      Put_Line( "Usage: odl [<options>] package(s) ");
      Put_Line("" );
      Put_Line( "options : " );
      Put_Line( "   -pedantic        Pedantic checking");
      Put_Line( "   -debugcode       Generate inline debug code" );
      Put_Line( "   -v               Print out processing information" );
      Put_Line( "   -s               Do not print the copyright notice" );
      Put_Line( "   -limit <nbr>     Max. number of errors" );
      Put_Line( "   -xschema <file>  Generate the XML schema into the given file");
      Put_Line( "   -gnatnosref      Add no Source_Reference Pragma");
      Put_Line( "   -D<name>=<value> Define a substitution variable");
      Put_Line( "" );
      Put_Line( "" );
      Put_Line( "   -h, --help       This message ");

      Put_Line( "" );
      Put_Line( "file(s) is any Ada 95 source file contining embedded SQL code. ");
      Put_Line( "" );
      Put_Line( "The ODL translator expects two input files:");
      Put_Line( "   <package>.ods - Object defintion file like an ads file.");
      Put_Line( "   <package>.odb - The Ada 95 code like an adb file.");
      Put_Line( "Out of theses files the ads and the adb files will be generated");
      Put_Line( "for further compilation.");
      Put_Line( "" );
   end Help_Text;

   ----------
   -- Main --
   ----------
   procedure Main(
      Version : in String ) is
      -- Process the command line. Files and options are
      -- processed in the same order as they are specified.
      -- In case of fatal error in a file, the next files
      -- will be invoked for translation.
      Arg     : Natural     := 1;
      Result  : Exit_Status := Success;
      --
   begin
      if Argument_Count = 0 then
         Copyright( Version );
         Help_Text;
         Set_Exit_Status( Failure );
      end if;

      while Arg in 1..Argument_Count loop
         if Argument(Arg)(1) = '-' then
            Handle_Option_Flag(Argument(Arg), Arg);
            if Show_Help_Info then
               Copyright( Version );
               Help_Text;
               exit;
            end if;
         else
            begin
               Copyright( Version );

               if Option_Verbose then
                  Put_Line("Processing Package " & Argument(Arg) & " : " );
               end if;

               Nbr_Of_Files := Nbr_Of_Files + 1;

               Process_Package( Argument(Arg), Result );

               if Result = Success then
                  Nbr_Of_Files_Success := Nbr_Of_Files_Success + 1;
               end if;

               Option_SILENT := True;

            exception
              when NAME_ERROR =>
                 Error("can't open input file : " & Argument(Arg));
                 Nbr_Of_Errors := Nbr_Of_Errors + 1;
           end;
           Arg := Arg + 1;
         end if;
      end loop;

      if Option_Schema then
         Close( Schema_File );
      end if;

      if Nbr_Of_Files > 0  then
         Summary;
      else
         if Nbr_Of_Errors = 0 and Argument_Count > 0 then
            Help_Text;
         end if;
      end if;

      if Nbr_Of_Errors > 0 then
         Result := Failure;
      end if;

      Set_Exit_Status( Result );
   end Main;

end ODL.Driver;

