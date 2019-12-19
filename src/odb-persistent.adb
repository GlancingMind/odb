-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-persistent.adb,v $
--  Version         : $Revision: 1.14 $                                       --
--  Description     : Definition of a persitent object                       --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 21-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/07/19 21:08:36 $                           --
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
--  Each object allocted in the pool of persistent objects is assigned to    --
--  an object table entry and some address space where the object data is    --
--  stored.								     --
--  The data structure of used in the persistent pool is shown below.        --
--		     							     --
--  Object 								     --
--   Id									     --
--   |     Object Table							     --
--   |   +---------------						     --
--   |   |              |						     --
--   |   :              :						     --
--   +-->+--------------+	      Allocated Space			     --
--       | Name         |            +--------------+			     --
--       | Self         |----------->| Object Id.   | 			     --
--       | ....         |	     |	            |			     --
--       +--------------+	     |		    |			     --
--       :              :	     +--------------+ <--- Address given to  --
--       |              |	     | Object Space |	   caller for the    --
--       +--------------+	     | XXXXXXXXXXXX |			     --
--                                   :              :			     --
--                                   | XXXXXXXXXXXX |			     --
--                                   +--------------+			     --
--									     --
--  Upon allocating a persistent object, a free entry in the object table is --
--  selected. The object table contains all information about the object it  --
--  self. The space requested to store the object is requested from the OS   --
--  and stored in the object table entry.                                    --
--  In order to gues from the object address the object table address, the   --
--  object storage space is preceeded by an Information area which contains  --
--  the object index.							     --
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
with Ada.Streams.Stream_IO;			use Ada.Streams.Stream_IO;
with Ada.Tags;					use Ada.Tags;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Exceptions;           			use Ada.Exceptions;
with Ada.IO_Exceptions;				use Ada.IO_Exceptions;

with System.Address_To_Access_Conversions;
use  System;
use  Ada;

with Util.List;

package body ODB.Persistent is

   Version : constant String := "$Id: odb-persistent.adb,v 1.14 2003/07/19 21:08:36 merdmann Exp $";

   ---------------------------
   -- Object_Storage_Access --
   ---------------------------
   type Object_Storage_Access is access Storage_Array;

   Word_Size      : constant Integer := Natural'Size;
   Byte_Size      : constant Integer := Storage_Element'Size;

   -----------------------------
   -- Object_Table_Entry_Type --
   -----------------------------
   type Object_Table_Entry_Type is record
	 Name     : Unbounded_String      := Null_Unbounded_String;
	 Self     : Reference             := null;
	 Size     : Storage_Count         := 0;
	 Next     : Natural               := 0;
	 Previous : Natural               := 0;
	 Item     : Object_Storage_Access := null;
      end record; 

   First_Free : Natural := 0;
   First_used : Natural := 0;

   OT : array( 1..Max_Nbr_Of_Objects ) of Object_Table_Entry_Type ;

   type Load_Phase_Type is ( Loading , Resolving );

   ----------
   -- Dump --
   ----------
   procedure Dump(
      P : Address) is
      package Hexout is new
         Ada.Text_IO.Modular_IO ( Num => Storage_Element ) ;

      W : Storage_Array( 1..20 );
      for W'Address use P;
   begin
      Put_Line("Base: " & Integer_Address'Image(To_Integer( P )) );
      for I in w'Range loop
         Hexout.Put (
            Item => w(I),
            Base => 16);
      end loop;
      Put_Line("");

   end Dump;

   --- ********************************************************************** ---
   --- ***         S T O R A G E    P O O L     M A N A G E R             *** ---
   --- ********************************************************************** ---

   --------------
   -- Allocate --
   --------------
   procedure Allocate( 
      Pool            : in out Instance_Pool_Type;
      Storage_Address : out Address;
      Size            : in  Storage_Count;
      Alignment       : in  Storage_Count ) is 
      E               : Natural;
   begin
      if First_Free = 0 then
	 Raise_Exception(To_Many_Objects'Identity, "");
      end if;

      -- remove first free element from the head of the list
      E := First_Free;
      First_Free := OT(E).Next;
      if First_Free /= 0 then
         OT(First_Free).Previous := 0;
      end if;

--      Put_Line("Allocating" & Natural'Image(E) & Storage_Count'Image(Size) );

      OT(E).Size    := Size;
      OT(E).Item    := new Storage_Array ( 1 .. (4 + Size + Alignment)  );

      OT(E).Item(1)   := Storage_Element(E mod 256 );
      OT(E).Item(2)   := Storage_Element(E / 256 );
      Storage_Address := OT(E).Item(5)'Address;

      -- add the used element to the front of the used list
      if First_Used /= 0 then
         OT(First_Used).Previous := E;
      end if;

      OT(E).Next     := First_Used;
      OT(E).Previous := 0;

      First_Used          := E;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------
   procedure Deallocate( 
      Pool            : in out Instance_Pool_Type;
      Storage_Address : in Address;
      Size            : in Storage_Count;                       
      Alignment       : in Storage_Count ) is
      -- deallocate an element from the buffer 
      P               : Storage_Array(1..4);
      for P'Address use Storage_Address - 4;
      I               : Natural := Natural( P(1) );
   begin   
      -- remove from list of used elements
      if OT(I).Previous /= 0 then
         OT( OT(I).Previous ).Next := OT(I).Next;
      else 
         First_Used := OT(I).Next;
      end if;

      -- put into list of free elements
      if First_Free /= 0 then
         OT(First_Free).Previous := I;
      end if;
      First_Free := I;
   end Deallocate;


   ------------------
   -- Storage_Size --
   ------------------
   function Storage_Size ( 
      Pool   : in Instance_Pool_Type ) return Storage_Count is
      P      : Natural := First_Used;
      Result : Storage_Count := 0;
   begin
      while P /= 0 loop
         Result := Result + OT(P).Size;
	 P := OT(P).Next;
      end loop;

      return Result;
   end Storage_Size;	       

   --- ********************************************************************** ---
   --- ***          O B J E C T    M A N A G E M E N T 			  *** ---
   --- ********************************************************************** ---

   --------------------
   -- Nbr_Of_Objects --
   --------------------
   function Nbr_Of_Objects return Natural is 
      Result : Natural := 0;
   begin
      for i in OT'Range loop
         if OT(I).Name /= Null_Unbounded_String then
	    Result := Result + 1;
	 end if;
      end loop;

      return Result;
   end Nbr_Of_Objects;

   -------------------
   -- Get_Reference --
   -------------------
   function Get_Reference( 
      Id    : in Natural;
      Force : Boolean := False ) return Reference is 
   begin
      if OT(id).Name = Null_Unbounded_String and not Force then
	 Raise_Exception(Anonymous_Object'Identity, "Object id: " & Natural'Image(Id) );
      end if;
         
      return OT(Id).Self;
   end Get_Reference;

   ---------------
   -- Object_Id --
   ---------------
   function Object_Id( 
      Ref    : in Reference ) return Natural is
      Result : Natural := 0;
      P      : Natural := First_Used;

      Header : Storage_Array( 1..4 );
      for Header'Address use Ref.all'Address - 4;
   begin
      return  Natural( Header(1) ) + Natural( Header(2)) * 256 ;
   end Object_Id;

   ---------------
   -- Object_Id --
   ---------------
   function Object_Id( 
      Ref    : in Object'Class ) return Natural is
      Result : Natural := 0;
      P      : Natural := First_Used;

      Header : Storage_Array( 1..4 );
      for Header'Address use Ref'Address - 4;
   begin
      return  Natural( Header(1) ) + Natural( Header(2)) * 256 ;
   end Object_Id;

   function Is_Persistent(
      Ref   : in Reference ) return Boolean is 
   begin
      return not ( Ref.Name = Null_Unbounded_String );
   end Is_Persistent;


   -----------------
   -- Name_Object --
   -----------------   
   procedure Name_Object(
      Ref  : in Reference ;
      Name : in String ) is
      Id   : Natural := Object_Id( Ref ); 
   begin
      if Lookup_Object( Name ) /= null then
	 Raise_Exception(Duplicate_Name'Identity, Name );
      end if;         
      OT(Id).Name := To_Unbounded_String(Name);
      OT(Id).Self := Ref;
      Ref.Name    := OT(Id).Name;

   end Name_Object;

   -----------------
   -- Name_Object --
   -----------------   
   function Object_Name( 
      Ref  : in Reference ) return String is
   begin
      return To_String( Ref.Name );
   end Object_Name;
   
   -------------------
   -- Lookup_Object --
   -------------------
   function Lookup_Object(
      Name : in String ) return Reference is 
      S    : constant Unbounded_String := To_Unbounded_String(Name);
      P    : Natural := First_Used;
   begin
      while P /= 0 loop
	 if OT(P).Name = S then
	    return OT(P).Self;
	 end if;
	 P := OT(P).Next;
      end loop;

      return null;
   end Lookup_Object;

   -- *********************************************************************** --
   -- ***          S E R I A L I Z I N G    S U P P O R T                 *** --
   -- *********************************************************************** --

   ---------------------
   -- Write_Reference --
   ---------------------
   procedure Write_Reference(
      Stream : access Root_Stream_Type'Class;
      Item   : in Reference ) is
      -- Dereference a pointer to an element in a pool
   begin
      -- Put_Line("Write Reference");
      if Item /= null then
         Unbounded_String'Output( Stream, Item.Name );
      else
         Unbounded_String'Output(Stream, Null_Unbounded_String );
      end if;
   end Write_Reference;

   --------------------
   -- Read_Reference --
   --------------------
   function Read_Reference(
      Stream : access Root_Stream_Type'Class ) return Reference is
      -- read in a reference from the stream and try to resove it. If 
      -- the name is not yet known in the pool, we store the address of the 
      -- Reference for later resolution. 
      Name   : constant Unbounded_String := Unbounded_String'Input(Stream);
      Result : Reference := null;
   begin
      -- Put_Line("Reading reference");
      if Name /= Null_Unbounded_String then
         Result := Lookup_Object( To_String(Name) );
      end if;

      return Result;
   end Read_Reference;

begin
   First_Free := OT'First;
   First_Used := 0;

   for i in OT'Range loop
      if (i+1) in OT'Range then
         OT(i).Next := i + 1;
      end if;

      if (i-1) in OT'Range then
         OT(i).Previous := i - 1;
      end if;
   end loop;
end ODB.Persistent;
