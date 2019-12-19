-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/adadoc/text.adb,v $
--  Description     : Persistent message qeue handling                       --
--  Author          : Michael Erdmann                                        --
--  Created         : 31.12.2001                                             --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/06/23 07:37:04 $
--  Status          : $State: Exp $
--                                                                           --
--  Copyright (C) 2001 Michael Erdmann                                       --
--                                                                           --
--  ASCL  is free software;  you can redistribute it  and/or modify it under --
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
--  This software is implemented to work with GNAT, the GNU Ada compiler.    --
--                                                                           --
--  Functional Description
--  ======================
--
--  Component Data
--  ==============
--
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--                                                                           --
--                                                                           --
--  Contact                                                                  --
--  =======                                                                  --
--  Error reports and suggestions shall be send to the Address:              --
--               Michael.Erdmann@snafu.de                                    --
--                                                                           --
--  General Informations will be found at:                                   --
--               purl:/net/michael.erdmann                                   --
--                                                                           --
-------------------------------------------------------------------------------
with Unchecked_Deallocation;

package body Text is

   type Text_Line_Type;
   type Text_Line_Access is access Text_Line_Type;

   type Text_Line_Type is record
         Next : Text_Line_Access := null;
         Text : Unbounded_String := Null_Unbounded_String;
      end record;

   -------------------------
   -- Paragraph_Data_Type --
   -------------------------
   type Paragraph_Data_Type is record
         Head  : Text_Line_Access := null;
         Tail  : Text_Line_Access := null;
         Lines : Natural := 0;
      end record;

   --------------
   -- Is_Empty --
   --------------
   function Is_Empty(
      This : in Paragraph_Type ) return Boolean is
      Data : Paragraph_Data_Access := This.Data;
   begin
      if Data = null then
         return True;
      end if;
      return Data.Lines = 0;
   end Is_Empty;
   ---------
   -- Add --
   ---------
   procedure Add(
      This : in out Paragraph_Type;
      S    : in String ) is
      Data : Paragraph_Data_Access := This.Data;
      Elem : Text_Line_Access        := null;
   begin
      if This.Data = null then
         This.Data := new Paragraph_Data_Type;
         Data := This.Data;

         Data.Lines := 0;
         Data.Head  := null;
      end if;

      Elem := new Text_Line_Type;
      Elem.Text := To_Unbounded_String(S);
      Elem.Next := null;

      Data.Lines := Data.Lines + 1;

      if Data.Head = null then
         Data.Head := Elem;
      else
         Data.Tail.Next := Elem;
      end if;

      Data.Tail := Elem;
   end Add;

   --------------
   -- To_Array --
   --------------
   function To_Array(
      This   : in Paragraph_Type ) return Text_Array_Access is
      Result : Text_Array_Access := null;
      P      : Text_Line_Access;
      Data   : Paragraph_Data_Access renames This.Data;
   begin
      if Data /= null  and then Data.Lines > 0 then
         Result := new Text_Array_Type( 1..Data.Lines );

         P := Data.Head;
         for I in Result.all'Range loop
            Result(I) := P.Text;

            P := P.Next;
            exit when P = null;
         end loop;
      end if;

      return Result;
   end To_Array;

   -----------
   -- Clear --
   -----------
   procedure Clear(
      This : in out Paragraph_Type ) is
      Data : Paragraph_Data_Access := This.Data;
      P,Q  : Text_Line_Access        := null;

      procedure Free is
         new Unchecked_Deallocation( Text_Line_Type, Text_Line_Access);
   begin
      if Data = null then
         return;
      end if;

      P := Data.Head;
      while P /= null loop
         Q := P.Next;
         Free( P );

         P := Q;
      end loop;

      Data.Lines := 0;
      Data.Head  := null;
      Data.Tail  := null;
   end Clear;

end Text;


