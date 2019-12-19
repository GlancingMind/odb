-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/util/util-list.adb,v $
--  Description     : Utility package tree                                   --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 19-Aug-2002                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/10/11 19:26:32 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2000-2003 Michael Erdmann                                  --
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
--  Restrictions                                                             --
--  ============                                                             --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Text_IO;                                use Ada.Text_IO;

with Unchecked_Deallocation;

package body UTIL.List is

   Version : constant String := 
      "$Id: util-list.adb,v 1.5 2003/10/11 19:26:32 merdmann Exp $";

   List_Number : Natural := 0;

   -----------------------
   -- List_Element_Type --
   -----------------------
   type List_Element_Type;
   type List_Element_Access is access List_Element_Type;

   type List_Element_Type is record
         Next : List_Element_Access := null;
         Item : Item_Type;
         List : Handle := null ;
      end record;

   procedure Free is
      new Unchecked_Deallocation( List_Element_Type, List_Element_Access);

   ---------------
   -- List_Data --
   ---------------
   type List_Data is record
         Head : List_Element_Access := null;
         Tail : List_Element_Access := null;
         Id   : Natural := 0;
      end record;

   procedure Free is
      new Unchecked_Deallocation( List_Data, Handle);

   ----------------------
   -- List_Reader_Data --
   ----------------------
   type List_Reader_Data is record
         List    : Handle              := null;
         Head    : List_Element_Access := null;
         Next    : List_Element_Access := null;
         Current : List_Element_Access := null;
      end record;

   procedure Free is
      new Unchecked_Deallocation( List_Reader_Data, List_Reader_Handle);

   ----------
   -- List --
   ----------
   function List return Handle is
      Result : Handle := new List_Data;
   begin
      Result.Id   := List_Number;
      List_Number := List_Number + 1;

      return Result;
   end List;

   -------------
   -- Destroy --
   -------------
   procedure Destroy(
      This : in out Handle ) is
      -- destroy the list
      P    : List_Element_Access := This.Head;
      Q    : List_Element_Access;
   begin
      while P /= null loop
         Q := P;
         P := P.Next;

         Free( Q );
      end loop;

      Free( This );
   end Destroy;

   ------------
   -- Append --
   ------------
   procedure Append(
      This : in Handle;
      Item : in Item_Type;
      Sub  : in Handle := Null_Handle ) is
      -- append an item to the list
      Q    : List_Element_Access := new List_Element_Type;
      Head : List_Element_Access renames This.Head;
      Tail : List_Element_Access renames This.Tail;
   begin
      Q.Item := Item;
      Q.List := Sub;

      if Head = null then
         Head := Q;
      else
         Tail.Next := Q;
      end if;

      This.Tail := Q;
   end Append;

   ------------
   -- Length --
   ------------
   function Length(
      This   : in Handle ) return Natural is
      P      : List_Element_Access ;
      Result : Natural := 0;
   begin
      if This = null then
         raise Invalid_List;
      end if;

      P := This.Head;
      while P /= null loop
         Result := Result + 1;
         P := P.Next;
      end loop;

      return Result;
   end Length ;
   -------------
   -- Execute --
   -------------
   procedure Execute(
      This : in Handle;
      It   : in out Iterator'Class ) is
      -- iterate through the list. If a list is found in the list a new
      -- iteration sequence is started using the same iterator object.
      Q    : List_Element_Access := This.Head;
   begin
      It.Stop := False;
      while Q /= null loop
         Execute( It, Q.Item );
         if Q.List /= Null_Handle then
            Execute( Q.List, It );
         end if;

         exit when It.Stop ;

         Q := Q.Next;
      end loop;
   end Execute;

   ----------
   -- stop --
   ----------
   procedure Stop(
      It   : in out Iterator'Class ) is
   begin
      It.Stop := True;
   end Stop;

   -----------------
   -- List_Reader --
   -----------------
   function List_Reader(
      This   : in Handle ) return List_Reader_Handle is
      -- create a liar reader from the list handle.
      Result : List_Reader_Handle := new List_Reader_Data;
   begin
      if This = null then
         Destroy( Result );
         raise Invalid_List;
      end if;

      Result.List    := This;

      Result.Head    := This.Head;
      Result.Next    := Result.Head;
      Result.Current := Result.Head;

--      Put_Line("New List_Reader, length=" & Natural'Image( Length( This ) ) );
      return Result;
   end List_Reader;

   -----------------
   -- List_Reader --
   -----------------
   procedure Destroy(
      Reader : in out List_Reader_Handle ) is
   begin
      Free( Reader );
   end Destroy;

   -----------
   -- First --
   -----------
   function First(
      Reader : in List_Reader_Handle ) return Item_Type is
      Data   : List_Element_Access;
   begin
      Reader.Next    := Reader.List.Head;
      Reader.Current := Reader.List.Head;

      if Reader.Next = null then
         raise End_Of_List;
      end if;

      Data :=  Reader.Next;
      -- Reader.Next := Data.Next;

      return Data.Item;
   end First;

   ----------
   -- next --
   ----------
   function Next(
      Reader : in List_Reader_Handle ) return Item_Type is
      Data   : List_Element_Access;
   begin
      if Reader.Next /= null then
         Data           := Reader.Next;

         Reader.Current := Reader.Next;
         Reader.Next    := Data.Next;
      else
         raise End_Of_List;
      end if;

      return Data.Item;
   end Next;

   --------------
   -- List_End --
   --------------
   function List_End  (
      Reader : in List_Reader_Handle ) return Boolean is
   begin
--      Put_Line("End of List " & Integer'Image( Reader.List.Id ) &
--               Boolean'Image( Reader.Next = null ) );
      return Reader.Next = null ;
   end List_End;

   -------------
   -- Child   --
   -------------
   function Child  (
      Reader : in List_Reader_Handle ) return Handle is
      Data   : List_Element_Access renames Reader.Current;
   begin
      if Data = null then
         return Null_Handle;
      end if;
      return Data.List;
   end Child ;

   -----------
   -- Child --
   -----------
   procedure Child(
      Reader : in List_Reader_Handle;
      List   : in Handle ) is
      Data   : List_Element_Access renames Reader.Current;
   begin
      Data.List := List;
   end Child;

   -------------
   -- Current --
   -------------
   function Current(
      Reader : in List_Reader_Handle ) return Item_Type is
      Data   : List_Element_Access renames Reader.Current;
   begin
      return Data.Item;
   end Current;

   ------------
   -- Append --
   ------------
   procedure Append(
      Reader : in List_Reader_Handle;
      Data   : in Item_Type ) is
   begin
      Append( Reader.List, Data );
   end Append;

end UTIL.List;
