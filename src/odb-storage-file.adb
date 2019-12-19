-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-storage-file.adb,v $
--  Version         : $Revision: 1.5 $                                       --
--  Description     : Filebased persistency                                  --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 21-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/10/06 18:29:44 $                           --
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
--  This package defines the storage strategy. The object data is put into a --
--  xml wrapper                              				     --
--                                                                           --
--                                                                           --
--  objectfile ::= header data		  				     --
--  header     ::= <header> attribute* </header>  (see odb.storage_header)   --
--  attribute  ::= <attribite name= offset= />				     --
--									     --
--  data       ::= <data> hex data </data>				     --
--                          						     --
--  Restrictions                                                             --
--  ============                                                             --
--  R.1 -                                                                    --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Streams.Stream_IO;			use Ada.Streams.Stream_IO;
with Ada.Characters.Latin_1;			use Ada.Characters.Latin_1;
with Ada.Strings;				use Ada.Strings;
with Ada.Strings.Fixed;				use Ada.Strings.Fixed;
with Ada.Tags;					use Ada.Tags;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Exceptions;           			use Ada.Exceptions;
with Ada.IO_Exceptions;				use Ada.IO_Exceptions;
with Ada.Text_IO;
use  Ada;

with ODB.Memory_Stream;				use ODB.Memory_Stream;
with ODB.Storage_Header;			use ODB.Storage_Header;
with ODB.Object_Loader;				use ODB.Object_Loader;
with ODB.Object_Writer;				use ODB.Object_Writer;

package body ODB.Storage.File is

   Version : constant String := 
       "$Id: odb-storage-file.adb,v 1.5 2003/10/06 18:29:44 merdmann Exp $";

   Max_Object_Size : constant Stream_Element_Offset := 60_000;
   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type is record
         Pool_Path  : Unbounded_String := Null_Unbounded_String;
	 Index_Path : Unbounded_String := Null_Unbounded_String;
      end record;

   ----------------
   -- Initialize --
   ----------------
   function Initialize return Object_Data_Access is 
      Result : Object_Data_Access := new Object_Data_Type;
   begin
      Result.Index_Path := To_Unbounded_String("./pool.idx");
      Result.Pool_Path  := To_Unbounded_String("./pool/");

      return Result;
   end Initialize;

   ---------------
   -- Pool_Path --
   ---------------
   procedure Pool_Path( 
      This  : in out Object;
      Value : String ) is 
      Data  : Object_Data_Access renames This.Data;
   begin
      Data.Pool_Path := To_Unbounded_String( Value );
   end Pool_Path;

   ----------------
   -- Index_Path --
   ----------------
   procedure Index_Path( 
      This  : in out Object;
      Value : String ) is
      Data  : Object_Data_Access renames This.Data;
   begin
      Data.Index_Path := To_Unbounded_String( Value );
   end Index_Path;

   ----------
   -- Save --
   ----------
   procedure Save( 
      This : in out Object ) is 
      -- save the contents of the pool into the given file name
      Data : Object_Data_Access renames This.Data;

      File : Stream_IO.File_Type ;
      Nbr  : constant Natural := Persistent.Nbr_Of_Objects;

      OBS  : Stream_Access renames This.Obs;
      IDX  : Stream_Access renames This.Index;
      Ref  : Reference;

      Writer : Object_Writer.Object;
   begin
      OBS  := Memory_Stream.Stream(Max_Object_Size);
      
      Create( File => File, 
         Mode => Out_File, 
	 Name => To_String(Data.Index_Path)
      );
      IDX := Stream( File );

      Natural'Output(IDX, Nbr );

      Path( Writer, To_String(Data.Pool_Path) );

      for i in 1..Persistent.Max_Nbr_Of_Objects loop
	 Ref := Get_Reference( i, Force => True );
	 if Ref /= null and then Is_Persistent( Ref ) then
	    declare
	       Name   : constant String := Object_Name( Ref );
               Last   : Stream_Element_Offset := 0;
	       Buffer : Stream_Element_Array( 1..Max_Object_Size );
	    begin
	       String'Output( IDX, Name );

	       Save_Instance( This, Ref ); 
	       Copy_Out( OBS, Buffer, Last );

               Class_Name( This.Header, External_Tag(Ref.all'Tag)  );
	       Write( Writer, Name, This.Header, Buffer( 1..Last ) );

	       Storage_Header.Clear( This.Header );
	    end ;
	 end if;
      end loop;

      Close(File);
   end Save;

   ----------
   -- Load --
   ----------
   procedure Load(
      This   : in out Object ) is 
      -- load the objects from the given file
      Data   : Object_Data_Access renames This.Data;

      Start  : Stream_IO.Positive_Count;
      File   : Stream_IO.File_Type ;

      IDX    : Stream_Access renames This.Index;
      OBS    : Stream_Access renames This.Obs;

      Name   : Unbounded_String ;

      procedure Load_Index( 
         Phase : in Load_Phase_Type ) is
	 -- run through the index file and load the named objects
         Nbr   : Natural := 0;
      begin
         Set_Index( File, Start );

	 Nbr := Natural'Input( IDX );
         for i in 1..Nbr loop
	    declare
               Name   : constant String := String'Input(IDX);
      	       Loader : Object_Loader.Object;
	    begin
               Path( Loader, To_String(Data.Pool_Path) );
	       Read( Loader, Name );

               This.Header := Header( Loader );
	       This.OBS    := Stream( Loader );

	       Load_Instance( This, Name, InstanceOf( Loader ), Phase );

	       Destroy( This.OBS );
	       Clear( This.Header );

	    exception
	       when Error : Others =>
	          Text_IO.Put_Line("Exception while loading object " & Name );
		  Text_IO.Put_Line(Exception_Name(Error) & ", " &
		                   Exception_Message( Error ) );
		  raise;
	    end ;
	 end loop;
      end Load_Index;

   begin
      Open( File => File , 
         Mode => In_File, 
	 Name => To_String(Data.Index_Path));

      Start := Index( File );
      IDX   := Stream(File);

      Load_Index( Loading );
      Load_Index( Resolving );

      Close(File);

   exception
      when ADA.IO_EXCEPTIONS.NAME_ERROR =>
	 null;
      when Error : others =>
         if Is_Open( File ) then
	    Close(File);
	 end if;
         raise;
   end Load;

end ODB.Storage.File;
