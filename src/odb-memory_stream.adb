-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-memory_stream.adb,v $
--  Description     : Stream which writes into memory                        --
--  Author          : Michael Erdmann                                        --
--  Created         : 31.12.2001                                             --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/10/06 04:27:57 $
--  Status          : $State: Exp $
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
--  This software is implemented to work with GNAT, the GNU Ada compiler.    --
--                                                                           --
--  Functional Description                                                   --
--  ======================                                                   --
--  This packages contains a stream class which write into a memory section  --
--  with the specified size.                                                 --
--                                                                           --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  Only Linux                                                               --
--                                                                           --
--  Contact                                                                  --
--  =======                                                                  --
--  Error reports and suggestions shall be send to the Address:              --
--               Michael.Erdmann@snafu.de                                    --
--                                                                           --
--  General Informations will be found at:                                   --
--               purl/net/michael.erdmann                                    --
--                                                                           --
-------------------------------------------------------------------------------
--* Ada
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Text_IO;           
with Ada.Streams;                       use Ada.Streams;
use  Ada;
with System;                            use System;
with System.Storage_Elements;           use System.Storage_Elements;
with Unchecked_Deallocation;

package body ODB.Memory_Stream is

   Version : constant String :=
      "$Id: odb-memory_stream.adb,v 1.4 2003/10/06 04:27:57 merdmann Exp $";

   ---====================================================================---
   ---===                O B J  E C T     D A T A                      ===---
   ---====================================================================---

   -----------------
   -- Object_Data --
   -----------------
   type Object_Data( Size : Stream_Element_Offset ) is record
         Xmit_Buff       : Stream_Element_Array(1..Size)  := (others => 0) ;
         Xmit_Last       : Stream_Element_Offset          := 1 ;

         Recv_Buff       : Stream_Element_Array(1..Size)  := (others => 0) ;
         Recv_Stored     : Stream_Element_Offset := 0 ;
         Recv_Read       : Stream_Element_Offset := 0 ;
      end record;

   type Handle is access all Object;
   ---=====================================================================---
   ---===         L O C A L   S U P P O R T   P R O C E D U R E S       ===---
   ---=====================================================================---

   procedure Dump(
      Data : in Stream_Element_Array ) is 
      ------------
      -- Encode --
      ------------
      function Encode(
         Value  : in Stream_Element ) return String is
         Hi     : Natural := ( Natural(Value) / 16 ) mod 16;
         Lo     : Natural :=   Natural(Value)  mod 16;
	 HexVal : constant String := "0123456789ABCDEF";
      begin
         return HexVal( Hi+1 ) & HexVal( Lo+1 );
      end Encode;
   begin
      for i in Data'Range loop
         Text_IO.Put( Encode( Data(i) ) & " " );
	 if i mod 16 = 0 then
	    Text_IO.Put_Line("");
	 end if;
      end loop;
      Text_IO.Put_Line("");
   end Dump;

   ----------
   -- Read --
   ----------
   procedure Read (
      This : in out Object ;
      Item : out Stream_Element_Array ;
      Last : out Stream_Element_Offset) is
      -- Procedure to read data from the communication link.
      Data : Object_Data_Access renames This.Data;
   begin
      if (Data.Recv_Read > Data.Recv_Stored ) or Data.Recv_Stored = 0 then
         Text_IO.Put_Line(Stream_Element_offset'Image( Data.Recv_Read ) & 
	          Stream_Element_Offset'Image( Data.Recv_Stored ) );
         raise Buffer_Underrun;
      end if;

      for X in Item'Range loop
         Item(X) := Data.Recv_Buff(Data.Recv_Read);
         Last := X;

         Data.Recv_Read := Data.Recv_Read + 1;
         exit when Data.Recv_Read > Data.Recv_Stored;
      end loop;
      -- Dump( Item( 1..Last ) );
   end Read;

   -----------
   -- Write --
   -----------
   procedure Write(
      this     : in out Object;
      Item     : in Stream_Element_Array) is
      -- Write the data into the output stream
      Data     : Object_Data_Access renames This.Data;
   begin
      for X in Item'Range loop
         if not ( Data.Xmit_Last in Data.Xmit_Buff'Range ) then
            raise Buffer_Overrun;
         else
            Data.Xmit_Buff( Data.Xmit_Last ) := Item(X);
            Data.Xmit_Last := Data.Xmit_Last + 1;
         end if;
      end loop ;
   end Write;

   ------------
   -- Stream --
   ------------
   function Stream(
      Size   : Stream_Element_Offset ) return Stream_Access is
      Result : Handle := new Object;
   begin
      Result.Data := new Object_Data( Size );

      return Stream_Access(Result);
   end Stream;

   --------------
   -- Finalize --
   --------------
   procedure Destroy(
      This : in out Stream_Access ) is
      -- Finalize the instance by releasing the object data
      -- finalizing the extension.
      H : Handle := Handle( This );

      procedure Free is
            new Unchecked_Deallocation( Object_Data, Object_Data_Access);

     procedure Free is
            new Unchecked_Deallocation( Object, Handle);
   begin
      Free( H.Data );
      H.Data := null;

      Free( H );
      This := null;
   end Destroy;

   -----------
   -- Clear --
   -----------
   procedure Clear( 
      This : in Stream_Access ) is 
      -- returns the number of elements written so far
      Data   : Object_Data_Access renames Handle(This).Data;
   begin
      Data.Xmit_Last   := Data.Xmit_Buff'First;
      Data.Recv_Read   := Data.Recv_Buff'First;
      Data.Recv_Stored := 0;
   end Clear;

   ----------
   -- Size --
   ----------
   function Write_Offset( 
      This : in Stream_Access ) return Natural is 
      -- returns the number of elements written so far
      Data   : Object_Data_Access renames Handle(This).Data;
   begin
      return Natural( Data.Xmit_Last );
   end Write_Offset;

   -----------------
   -- Read_Offset --
   -----------------
   procedure Read_Offset( 
      This   : in Stream_Access;
      Offset : in Natural ) is
      Data   : Object_Data_Access renames Handle(This).Data;
   begin
      --Text_IO.Put_Line( Stream_Element_Offset'Image(Data.Recv_Stored) );
      Data.Recv_Read := Stream_Element_Offset(Offset);
   end Read_Offset;

   -------------
   -- Copy_In --
   -------------
   procedure Copy_In( 
      This   : in Stream_Access;
      Source : in Stream_Element_Array ) is
      -- copy the source array into the read buffer
      Data   : Object_Data_Access renames Handle(This).Data;
      Dest   : Stream_Element_Offset := Data.Recv_Buff'First;
   begin
      Data.Recv_Stored := Source'Length ;
      Data.Recv_Read   := Data.Recv_Buff'First;

      for i in Source'Range loop
         Data.Recv_Buff( Dest ) := Source(i);
	 Dest := Dest + 1;
      end loop;
   end Copy_In;

   --------------
   -- Copy_Out --
   --------------
   procedure Copy_Out(
      This   : in Stream_Access;
      Target : in out Stream_Element_Array;
      Last   : out Stream_Element_Offset ) is 
      -- copy the data which has been written so far into a buffer
      Data   : Object_Data_Access renames Handle(This).Data;
      S      : Stream_Element_Offset := Data.Xmit_Buff'First;
   begin
      Last := Stream_Element_Offset(0);

      for i in Target'Range loop
         exit when not ( S < Data.Xmit_Last );

         Target(i) := Data.Xmit_Buff(S);
	 S := S + 1;
	 Last := Last + 1;
      end loop;

      Data.Xmit_Last := Data.Xmit_Buff'First;
   end Copy_Out;

   ----------
   -- Size --
   ----------
   function Size(
      This   : in Stream_Access ) return Natural is
      Data   : Object_Data_Access renames Handle(This).Data;
   begin
      return Natural( Data.Recv_Stored );
   end Size;


end ODB.Memory_Stream;
