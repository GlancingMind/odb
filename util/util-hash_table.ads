-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/util/util-hash_table.ads,v $
--  Description     : keyed container for fixed types                        --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 16-Mar-2002                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/10/11 19:26:32 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2000-2003 Michael Erdmann                                  --
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
--  Functional Description                                                   --
--  ======================                                                   --
--  This package implements an container which stores pairs of objects.      --
--  derived from the Keys and the Container_Element package.                 --
--  Each object derived from the Keys package is called a key.               --
--  This package allows to retrieve (address) the stored Container_Element   --
--  by means of the key object.                                              --
--                                                                           --
--  The Keyed container has a maximum capacity for such pairs.               --
--                                                                           --
--  Upon storing the container creates a complete copy of the <key,object>   --
--  pair in order to ensure that there are no references from outside        --
--  into sub components of the stored objects.                               --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  R.1 - The handling of the tree nodes is currently not task save          --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Streams;                       use Ada.Streams;
with Ada.Finalization;                  use Ada.Finalization;
use  Ada;

generic
   type Key_Type is private ;

package Util.Hash_Table  is

   type Table_Type( Capacity : Natural ) is private;

   Table_Full        : exception;
   Key_Already_Used  : exception;
   Key_Not_Existing  : exception;

   ---------------------------------------------------------------------------
   -- Description:
   --    This method stores the key and the container element in the
   --    keyed container.
   -- Preconditions:
   --    P.1  - Item /= null
   --    P.2  - Number of stored keys < capacity.
   --    P.3  - Key not already stored
   -- Postconditions:
   --    C.1  - An additional key/value pair has been added to the
   --           container.
   -- Exceptions:
   --    Table_Full - P.2 violated
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Put(
       This   : in out Table_Type;
       Key    : in Key_Type;
       Value  : out Natural );

   ---------------------------------------------------------------------------
   -- Description:
   --    Retrieve a container element from the container.
   -- Preconditions:
   --    P.1 - Key exisits in container
   -- Postconditions:
   --    C.1 - Funtion yield handle to copy of the container element
   -- Exceptions:
   --    Key_Not_Existing  - P.1 violated
   -- Notes:
   --    None
   ---------------------------------------------------------------------------
   function Get(
       This : in Table_Type;
       Key  : in Key_Type ) return Natural;

   ---------------------------------------------------------------------------
   -- Description:
   --    Get the key associated with the hash code
   -- Preconditions:
   --    P.1 - Hash code is used
   -- Postconditions:
   --    C.1 - The yields key
   -- Exceptions:
   --    Key_Not_Existing - P.1 violated
   -- Notes:
   --    None
   ---------------------------------------------------------------------------
   function Key(
       This : in Table_Type;
       Hash : in Natural ) return Key_Type;

   ---------------------------------------------------------------------------
   -- Description:
   --    Remove a key from the container
   -- Preconditions:
   --    P.1 - Key exists in container
   -- Postconditions:
   --    C.1 - The container_element held by the container is deallocated
   -- Exceptions:
   --    Key_Not_Existing - P.1 violated
   -- Notes:
   ---------------------------------------------------------------------------
   procedure Remove(
      This : in out Table_Type;
      Key  : in Key_Type );

   ---------------------------------------------------------------------------
   -- Description:
   --    Clear the container
   -- Preconditions:
   --    None
   -- Postconditions:
   --    C.1 - All objects held by the container are destroyed. The
   --          nbr of free entries = capacity.
   -- Exceptions:
   --    None
   -- Notes:
   --    None
   ---------------------------------------------------------------------------
   procedure Clear(
       This : in out Table_Type);

   ---------------------------------------------------------------------------
   -- Description:
   --    Check if empty
   -- Preconditions:
   --    None
   -- Postconditions:
   --    C.1 - return true if nbr_of_entries = 0
   -- Exceptions:
   --    None
   -- Notes:
   ---------------------------------------------------------------------------
   function Is_Empty(
      This    : in Table_Type) return Boolean;

   ---------------------------------------------------------------------------
   -- Description:
   --    check if key or a element is in the container
   -- Preconditions:
   --    None
   -- Postconditions:
   --    C.1 - either true or false or the key handle.
   -- Exceptions:
   --    None
   -- Notes:
   --    Both procedures are not optimized for speed. In case of large
   --    tables this might be a real problem for the performance of your
   --    application code.
   ---------------------------------------------------------------------------
   function Contains(
      This    : in Table_Type;
      Key     : in Key_Type ) return Boolean;

   ---------------------------------------------------------------------------
   -- Description:
   --    Returns the list of all keys in the container
   -- Preconditions:
   --    P.1 - The number of entries in the container is not larger then
   --          the provided dictionary table
   -- Postconditions:
   --    C.1 - length is set to the actual number of entries in the
   --          dictionary.
   -- Exceptions:
   --    Dictionary_Overflow - P.1 violated.
   -- Notes:
   --    None
   ---------------------------------------------------------------------------
   type Dictionary_Table is array( Natural range <> ) of Key_Type;

   procedure Dictionary(
      This    : in  Table_Type;
      Keys    : out Dictionary_Table;
      Length  : out Natural );		  

   Dictionary_Overflow : exception;

   ---------------------------------------------------------------------------
private
   type Table_Data;
   type Table_Data_Access is access all Table_Data;

   type Table_Type( Capacity : Natural ) is new Controlled with
      record
         Data : Table_Data_Access := null;
      end record;

   procedure Initialize(
      This : in out Table_Type );

   procedure Finalize(
      This : in out Table_Type );

   ----------------
   -- Hash_Stram --
   ----------------

   type Hash_Stream is new Ada.Streams.Root_Stream_Type with record
      Hash_Value : Natural := 0;
      Length     : Natural := 0;
   end record;

   procedure Write(
      This : in out Hash_Stream;
      Item : in Stream_Element_Array);

   procedure Read (
      This : in out Hash_Stream ;
      Item : out Stream_Element_Array ;
      Last : out Stream_Element_Offset);

end Util.Hash_Table ;
