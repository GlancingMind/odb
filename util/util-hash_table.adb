-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/util/util-hash_table.adb,v $
--  Description     : Typed hash table                                       --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 19-Mar-2002                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/10/11 19:26:32 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2002 Michael Erdmann                                       --
--                                                                           --
--  GNADO is free software;  you can redistribute it  and/or modify it under --
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
--  GNADO is implemented to work with GNAT, the GNU Ada compiler.            --
--                                                                           --
--  Functional Description                                                   --
--  ======================                                                   --
--                                                                           --
--  This module implements the a hash table management and retrieval         --
--  based on a simple scattering mechanism [1].                              --
--  A key may be stored in this table and is translated into an index        --
--  might be used for any purpose in the application.                        --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  R.1 - Not Task save                                                      --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  [1] - Algorithm & Datastructures                                         --
--        N. Wirth 1979                                                      --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Text_IO;     use Ada.Text_IO;

package body Util.Hash_Table  is

   Version : constant String :=
   "$Id: util-hash_table.adb,v 1.2 2003/10/11 19:26:32 merdmann Exp $";

   --------------------
   -- Container_Data --
   --------------------
   type Hash_Record is record
         Key   : Key_Type;
         Used  : Boolean := False;
      end record;

   type Hash_Table_Type is array (Natural range <>) of Hash_Record;

   ----------------
   -- Table_Data --
   ----------------
   type Table_Data (Capacity : Natural) is record
         Nbr_Of_Entries    : Natural := 0;
         Nbr_Of_Collisions : Natural := 0;

         HT : Hash_Table_Type ( 0..Capacity );
      end record;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize (
      This : in out Table_Type) is
      Data : Table_Data_Access := new Table_Data( This.Capacity );
   begin
      Data.Nbr_Of_Entries    := 0;
      Data.Nbr_Of_Collisions := 0;

      This.Data := Data;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize (
      This : in out Table_Type ) is
      HT   : Hash_Table_Type renames This.Data.HT;
   begin
      for I in HT'Range loop
         HT(I).Used := False;
      end loop;

--      Put_Line ( "colisions/entries"  &
--                 Natural'Image(This.Data.Nbr_Of_Collisions) & "/" &
--                 Natural'Image(This.Data.Nbr_Of_Entries) );
   end Finalize;

   -----------
   -- Write --
   -----------
   procedure Write(
      This : in out Hash_Stream;
      -- this procedure is used to create the hash value
      Item : in Stream_Element_Array) is
   begin
      for X in Item'Range loop
         This.Hash_Value := This.Hash_Value + Natural(Item(X));
         This.Length := This.Length + 1;
      end loop ;
   end Write;

   ----------
   -- Read --
   ----------
   procedure Read (
      This : in out Hash_Stream ;
      Item : out Stream_Element_Array ;
      Last : out Stream_Element_Offset) is
      -- dummy. This is not used.
   begin
      Last    := 0;
      Item(1) := 0;
   end Read;

   ----------
   -- Hash --
   ----------
   function Hash(
      Key : in Key_Type ) return Natural is
      -- caclulate a simple hash value
      S : aliased Hash_Stream;
   begin
      S.Hash_Value := 0;
      S.Length     := 0;
      Key_Type'Output(S'Access, Key);
      return S.Hash_Value;
   end Hash;

   --------------
   -- Is_Empty --
   --------------
   function Is_Empty(
      This : in Table_Type) return Boolean is
   begin
      return This.Data.Nbr_Of_Entries = 0;
   end Is_Empty;

   ---------------------
   -- Next_Hash_Entry --
   ---------------------
   procedure Next_Hash_Entry(
      H    : in out Natural;
      R    : in out Natural;
      N    : in Natural ) is
      -- this procedure selects the next hash entry by using scattering
   begin
      H := ( H + R ) mod N;
      R := R + 2;
   end Next_Hash_Entry;

   --------------
   -- Contains --
   --------------
   function Contains(
      This      : in Table_Type;
      Key       : in Key_Type ) return Boolean is
      -- check if a key is in the container
      HT        : Hash_Table_Type renames This.Data.HT;
      Hash_Size : constant Natural := This.Capacity;

      H         : Natural := Hash( Key ) mod Hash_Size;
      R         : Natural := 1;
   begin
      while R < Hash_Size loop
         if HT(H).Used and then HT(H).Key = Key then
            return True;
         end if;

         Next_Hash_Entry( H, R, Hash_Size );
      end loop;

      return False;
   end Contains;

   -----------
   -- Clear --
   -----------
   procedure Clear(
      This      : in out Table_Type) is
      HT        : Hash_Table_Type renames This.Data.HT;
   begin
      for I in HT'Range loop
         if HT(I).Used then
            HT(I).Used := False;
         end if;
      end loop;
      This.Data.Nbr_Of_Entries    := 0;
      This.Data.Nbr_Of_Collisions := 0;
   end Clear;

   ----------------
   -- Dictionary --
   ----------------
   procedure Dictionary(
      This      : in  Table_Type;
      Keys      : out Dictionary_Table;
      Length    : out Natural ) is
      -- copy the keys of the container into the given table.
      HT        : Hash_Table_Type renames This.Data.HT;
      P         : Natural := Keys'First;
   begin
      Length := 0;

      for I in HT'Range loop
         if HT(I).Used then
            if not ( P in Keys'Range ) then
               raise Dictionary_Overflow;
            end if;

            Keys(P) := HT(I).Key;
            P := P + 1;
         end if;
      end loop;
      Length := P - Keys'First;
   end Dictionary;

   ---------
   -- Put --
   ---------
   procedure Put(
      This      : in out Table_Type;
      Key       : in Key_Type;
      Value     : out Natural ) is
      -- put a key / object pair into the container
      HT        : Hash_Table_Type renames This.Data.HT;
      Nbr       : Natural renames This.Data.Nbr_Of_Entries;
      Collision : Natural renames This.Data.Nbr_Of_Collisions;

      Hash_Size : constant Natural := This.Capacity;

      H         : Natural := Hash( Key ) mod Hash_Size;
      R         : Natural := 1;
   begin
      -- locate the next free entry in the hash table
      while HT(H).Used and then HT(H).Key /= Key loop
         Collision := Collision + 1;

         Next_Hash_Entry( H, R, Hash_Size );

         if R > Hash_Size then
            raise Table_Full;
         end if;
      end loop;

      HT(H).Used  := True;
      HT(H).Key   := Key;
      Nbr         := Nbr + 1;

      Value := H;
   end Put;

   ---------
   -- Get --
   ---------
   function Get(
      This      : in Table_Type;
      Key       : in Key_Type ) return Natural is
      -- retrive a object from the keyed container.
      HT        : Hash_Table_Type renames This.Data.HT;
      Hash_Size : constant Natural := This.Capacity;

      H         : Natural := Hash( Key ) mod Hash_Size;
      R         : Natural := 1;
   begin
      while R < Hash_Size loop
         if HT(H).Used and then HT(H).Key = Key then
            return H;
         end if;

         Next_Hash_Entry( H, R, Hash_Size );
      end loop;

      raise Key_Not_Existing;
   end Get;

   ------------
   -- Remove --
   ------------
   procedure Remove(
      This      : in out Table_Type;
      Key       : in Key_Type ) is
      -- remove an object from the container.
      HT        : Hash_Table_Type renames This.Data.HT;
      Hash_Size : constant Natural := This.Capacity;

      H         : Natural := Hash( Key ) mod Hash_Size;
      R         : Natural := 1;
   begin
      while R < Hash_Size loop
         if HT(H).Used and then HT(H).Key = Key then
            HT(H).Used := False;
            return;
         end if;

         Next_Hash_Entry( H, R, Hash_Size );
      end loop;

      raise Key_Not_Existing;
   end Remove;

   ---------
   -- Key --
   ---------
   function Key(
      This      : in Table_Type;
      Hash      : in Natural ) return Key_Type is
      -- evaluate the key value from the hash value
      HT        : Hash_Table_Type renames This.Data.HT;
      Hash_Size : constant Natural := This.Capacity;

   begin
      if HT(Hash).Used then
         return HT(Hash).Key;
      end if;

      raise Key_Not_Existing;
   end Key;

end Util.Hash_Table ;
