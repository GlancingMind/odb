-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/util/util-stack.adb,v $
--  Description     : Utility package tree                                   --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 19-Aug-2002                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/10/06 04:27:57 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2000-2002 Michael Erdmann                                  --
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
with Unchecked_Deallocation;

package body UTIL.Stack is
 
   Version : constant String := "$Id: util-stack.adb,v 1.1 2003/10/06 04:27:57 merdmann Exp $";

   type Stack_Element_Type;
   type Stack_Element_Access is access Stack_Element_Type;

   type Stack_Element_Type  is record
         Previous : Stack_Element_Access;
         Data     : Item_Type;
      end record;


   type Stack_Data_Type is record
         Current : Stack_Element_Access := null;
      end record;

   ---------------
   -- New_Stack --
   ---------------
   function New_Stack return Handle is
      Result : Handle := new Stack_Data_Type;
   begin
      Result.Current := null;
      return Result;
   end New_Stack;

   -------------
   -- Destroy --
   -------------
   procedure Destroy(
      This    : in out Handle ) is
      Current : Stack_Element_Access renames This.Current;
      P       : Stack_Element_Access;

      procedure Free is
         new Unchecked_Deallocation( Stack_Element_Type, Stack_Element_Access);

      procedure Free is
         new Unchecked_Deallocation( Stack_Data_Type, handle);

   begin
      while Current /= null loop
         P := Current;
         Current := Current.Previous;

         Free( P );
      end loop;

      Free( This );
   end Destroy;

   ----------
   -- Push --
   ----------
   procedure Push(
      This    : in Handle;
      Value   : in Item_Type ) is
      Current : Stack_Element_Access renames This.Current;
      Item    : Stack_Element_Access := new Stack_Element_Type;
   begin
      Item.Data := Value;

      if Current /= null then
         Item.Previous := Current;
      end if;

      Current := Item;
   end Push;

   ---------
   -- Pop --
   ---------
   procedure Pop(
      This    : in Handle;
      Value   : in out Item_Type ) is
      Current : Stack_Element_Access renames This.Current;
   begin
      if Current = null then
         raise Stack_Empty;
      end if;

      Value   := Current.Data;
      Current := Current.Previous;
   end Pop;

   -------------
   -- Current --
   -------------
   function Current(
      This    : in Handle ) return Item_Type is
      Current : Stack_Element_Access renames This.Current;
   begin
      if Current = null then
         raise Stack_Empty;
      end if;

      return Current.Data;
   end Current;

   --------------
   -- Is_Empty --
   --------------
   function Is_Empty(
      This    : in Handle ) return Boolean is
   begin
      return This.Current = null;
   end Is_Empty;
end UTIL.Stack;
