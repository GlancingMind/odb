-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/samples/dungon/main.adb,v $
--  Version         : $Revision: 1.8 $                                       --
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
with Ada.Command_Line;                  	use Ada.Command_Line;
with Ada.Exceptions;				use Ada.Exceptions;

with ODB.Persistent;				use ODB.Persistent;
with ODB.Storage.File;				use ODB.Storage.File;
use  ODB.Storage;
use  ODB;

with Game;					use Game;
with Player;					use Player;
with Room;					use Room;

with Version;

procedure Main is
   Cmd_Save   : constant Unbounded_String := To_Unbounded_String("save");
   Cmd_Help   : constant Unbounded_String := To_Unbounded_String("?");
   Cmd_Status : constant Unbounded_String := To_Unbounded_String("status");
   Cmd_Look   : constant Unbounded_String := To_Unbounded_String("look");
   Cmd_Claim  : constant Unbounded_String := To_Unbounded_String("claim");

   Line : String( 1..1024 );
   Last : Natural := 0;

   Cmd  : Unbounded_String := Null_Unbounded_String;
   Argc : Natural := 0;
   Args : array( 1..1024 ) of Unbounded_String;

   The_Game   : Reference := null;
   The_Player : Reference := null;
   The_Room   : Reference := null;

   File_Store : File.Object;

   procedure Argument_Info is
   begin
      Put_Line("Usage: ");
      Put_Line("   dungon <context> ");
      Put_Line("");
      Put_Line("The context is any string without blanks.");
      Put_Line("");
   end Argument_Info;

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
      Put_Line( "   left, right, forward, back  - direction commands");
      Put_Line( "   take, drop                  - fetch or drop things"); 
      Put_Line( "   look                        - This information");
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

   ----------------
   -- Start_Game --
   ----------------
   function Start_Game( 
      Name    : in String )  return Reference is
      Current : Reference := null;
   begin
      Current := Game.Create( Name );

      Put_Line("Enter your name:" );
      Get_Line( Line, Last );
      The_Player := Game.Add_Player( Current, Line(1..Last) );

      return Current;
   end Start_Game;     

   ------------
   -- Action --
   ------------
   procedure Movement( 
      The_Player : in Reference ) is 
      DX, DY     : Integer := 0;
   begin
      if Cmd = "left" then
         DX := -1;
      elsif Cmd = "right" then
         DX := 1;
      elsif Cmd = "back" then
         DY := -1;
      elsif Cmd = "forward" then
         DY := 1;
      elsif Cmd = "drop" then
         for i in 1..Argc loop
            Game.Drop( The_Game, The_Player, Args(i) );
	 end loop;
      end if;

      -- if there is a movement change the position
      if DX + DY /= 0 then
         Game.Move( The_Game, The_Player, DX, DY );
      end if;
   exception
      when Game.Invalid_Move =>
         Put_Line("It is not allowed to go " & To_String(Cmd) );
   end Movement;

begin
   Put_Line("");
   Put_Line("Dungons of Wisdom; Version " & Version.ID & "." );
   Put_Line("Copyright (C) 2003 Michael Erdmann (http://www.purl.org/net/michael.erdmann).");
   Put_Line("");				  

   if Argument_Count < 1 then
      Argument_Info;
      Set_Exit_Status( Failure );
      return;
   end if;

   File.Index_Path( File_Store, "./dungon.pool" );
   File.Pool_Path( File_Store,  "./data/" );
   File.Load( File_Store );

   The_Game := Start_Game( Argument(1) );
   Put_Line("");
   Game.Display_Game( The_Game );
   Put_Line("");
   Put_Line("For help type ?.");
   Put_Line("");
   loop
      Game.Display_Location( The_Game, The_Player );

      Put_Line(""); Put("$");
      Get_line( Line, Last );
      exit when Line(1) =  'q' ;

      Parse_Input( Line(1..Last) );
      if Cmd = Cmd_Save then 
         null ;
      elsif Cmd = Cmd_Status then
         Game.Display_Game( The_Game );
      elsif Cmd = Cmd_Help then
         Info;
      elsif Cmd = Cmd_Look then
         Display_Location( The_Game, The_Player );
      elsif Cmd = Cmd_Claim then
         Claim_Room( The_Game, The_Player );
      elsif Is_Object( The_Game, The_Player, Cmd ) then
         Set_Attribute( The_Game, The_Player, Cmd, Args(1), Args(2) );
      elsif Cmd = "drop" then
         for i in 1..Argc loop
            Game.Drop( The_Game, The_Player, Args(i) );
	 end loop;
      else
         Movement( The_Player );
      end if;

   end loop;

   File.Save(File_Store);

   Set_Exit_Status( Success );

exception
   when Error : Others => 
      Put_Line("**** Exception ");
      Put_Line("     " & Exception_Name( Error ) & "/" & Exception_Message( Error));

      Put_Line("Leaving the game without saving the state!");
      Set_Exit_Status( Failure );
end Main;
