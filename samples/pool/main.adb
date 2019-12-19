-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/samples/pool/main.adb,v $
--  Version         : $Revision: 1.5 $                                       --
--  Description     : ODB Database connection                                --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 21-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/07/12 19:07:56 $                           --
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
--  This object represents the connection to a database.                     --
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
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;

with ODB.Persistent;				use ODB.Persistent;
with ODB.Storage.File;				use ODB.Storage.File;
with Person;					use Person;
with Version;

procedure Main is
   Cmd_Insert  : constant Unbounded_String := To_Unbounded_String("add");
   Cmd_Help    : constant Unbounded_String := To_Unbounded_String("?");
   Cmd_Save    : constant Unbounded_String := To_Unbounded_String("save");
   Cmd_Query   : constant Unbounded_String := To_Unbounded_String("find");
   Cmd_Managed : constant Unbounded_String := To_Unbounded_String("mgr");

   Line : String( 1..1024 );
   Last : Natural := 0;

   Cmd  : Unbounded_String := Null_Unbounded_String;
   Argc : Natural := 0;
   Args : array( 1..1024 ) of Unbounded_String;

   ----------
   -- Info --
   ----------
   procedure Info is 
   begin
      Put_Line(" Usage: " );
      Put_Line("    <command> [ <arguments> ]");
      Put_Line("    arguments := { <argument> [ <arguments> ] | empty }");
      Put_Line("");
      Put_Line(" Commands are:");
      Put_Line( "   add  <name> <firstname>  -- add new user ");
      Put_Line( "   mgr  <name> <by>         -- name is managed by ");
      Put_Line( "   save <name>              -- save current object space");
      Put_Line( "   find <name>              -- Return the user information");
      Put_Line( "   <number>                 -- Return the user info for object id"); 
      Put_Line( "   ?                        -- This information");
   end Info;

   ------------------
   -- Parse_Input --
   ------------------
   procedure Parse_Input( 
      Ln       : in String ) is 
      -- Parse the input. The first word is always a command and the rest is 
      -- a list of words seperated by spaces.
      Arg      : Unbounded_String;
      Next_Pos : Positive := Ln'First;

      procedure Skip_Spaces is 
      begin
         while Next_Pos in Ln'Range and then ln(Next_Pos) = ' ' loop
      	    Next_Pos := Next_Pos + 1;
         end loop;
      end Skip_Spaces;

      function Next_Word return Unbounded_String is 
         Result : Unbounded_String := Null_Unbounded_String;
      begin
         while Next_Pos in Ln'Range and then ln(Next_Pos) > ' ' loop
	    Result := Result & Ln(Next_Pos) ;
	    Next_Pos := Next_Pos + 1;
	 end loop;

	 Skip_Spaces;

	 return Result;
      end Next_Word;
   begin
      Argc := 0;

      Cmd := Next_Word;
      Arg := Next_Word;
      while Arg /= Null_Unbounded_String loop
	 Argc := Argc + 1; 
         Args( Argc ) := Arg;
	 Arg := Next_Word;
      end loop;
      
   end Parse_Input;

   --------------------
   -- Execute_Insert --
   --------------------
   procedure Execute_Insert is 
      R : Reference := Person.Create( Args(1), Args(2) );
   begin
      Put_Line("Obj. Id" & Natural'Image( Object_ID( R ) ) );
   end Execute_Insert;
   ------------------------
   -- Execute_Managed_By --
   ------------------------
   procedure Execute_Managed_by is
      Subject : Reference := Person.Create( Args(1), Args(2) );
      Manager : Reference := Person.Create( Args(3), Args(4) );
   begin
      Managed_By( Subject, Manager );
   end Execute_Managed_By;


begin
   Put_Line("ODB Pool Example, Version " & Version.ID );

   Load;

   loop
      Put_Line(""); Put("$");
      Get_line( Line, Last );

      exit when Line(1) =  'q' ;
      Parse_Input( Line(1..Last) );
      if Cmd = Cmd_Save then 
         Save ;
      elsif Cmd = Cmd_Help then
	 Info;
      elsif Cmd = Cmd_Insert then
	 Execute_Insert;
      elsif Cmd = Cmd_Query then
         Display( Args(1), Args(2) );
      elsif Cmd = Cmd_Managed then
         Execute_Managed_By;
      else
         declare
	    Id  : Natural := Natural'Value( To_String(Cmd) );
	    Ref : Reference := Get_Reference( Id );
	 begin
	    if Ref /= null then
	       Display( Ref );
	    else 
	       Put_Line("Not found");
	    end if;
	 end;
      end if;

   end loop;

   Save;
end Main;
