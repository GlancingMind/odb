-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-memory_stream.ads,v $
--  Description     : Streams on top of message queues                       --
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
--  This package contains all defintions needed f7or the linux operating      --
--  system. This may have to be adopted for other environments.              --
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
--               purl:/net/michael.erdmann                                   --
--                                                                           --
-------------------------------------------------------------------------------
with System;                            use System;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Streams;                       use Ada.Streams;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;

package ODB.Memory_Stream is

   type Object is new Ada.Streams.Root_Stream_Type with private ;

   Buffer_Overrun  : exception;
   Buffer_Underrun : exception;

   ---------------------------------------------------------------------------
   -- Description: 
   --    Create a memory stream
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   function Stream(
      Size : in Stream_Element_Offset ) return Stream_Access;

   ---------------------------------------------------------------------------
   -- Description: 
   --    Destroy the memory stream
   -- Preconditions:
   --    C.1 - The stream has to be valid
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Destroy(
      This : in out Stream_Access );

   ---------------------------------------------------------------------------
   -- Description: 
   --   Clear the memory stream.
   -- Preconditions:
   --   C.1 - Stream has to be valid
   -- Postconditions:
   --   P.1 - Read and write pointer are reseted.
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Clear( 
      This : in Stream_Access );

   ---------------------------------------------------------------------------
   -- Description: 
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   function Write_Offset( 
      This : in Stream_Access ) return Natural;

   ---------------------------------------------------------------------------
   -- Description: 
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Read_Offset( 
      This   : in Stream_Access;
      Offset : in Natural ) ;

   ---------------------------------------------------------------------------
   -- Description: 
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Copy_In( 
      This   : in Stream_Access;
      Source : in Stream_Element_Array );

   ---------------------------------------------------------------------------
   -- Description: 
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Copy_Out(
      This   : in Stream_Access;
      Target : in out Stream_Element_Array;
      Last   : out Stream_Element_Offset );


   function Size(
      This   : in Stream_Access ) return Natural;


private
   type Object_Data;
   type Object_Data_Access is access all Object_Data;

   type Object is
      new Ada.Streams.Root_Stream_Type with record
            Data : Object_Data_Access := null;
         end record;

   -- requiered by the streams interface
   procedure Read (
      this  : in out Object ;
      Item  : out Ada.Streams.Stream_Element_Array ;
      Last  : out Ada.Streams.Stream_Element_Offset ) ;

   procedure Write (
      this  : in out Object ;
      Item  : in     Ada.Streams.Stream_Element_Array) ;

end ODB.Memory_Stream;


