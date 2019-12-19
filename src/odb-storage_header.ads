-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-storage_header.ads,v $
--  Version         : $Revision: 1.5 $                                       --
--  Description     : A collection of persistent objects                     --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 30-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/10/08 04:43:55 $                           --
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
--  Public License  distributed with ODB;  see file COPYING.  If not, write  --
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
--  Each object is described by a so called Storage_Header. The storage-     --
--  header contains the names of each field which has been declared as an    --
--  attrbute in the ODL specification.					     --
--  +-------------------+	                                             --
--  | Object Class Name |                                                    --
--  +-------------------+	                                             --
--  | D_Name   | offset |------+    Object Header       		     --
--  +-------------------+      |					     --
--  | D_Street | offset |---+  |					     --
--  :			:   |  |					     --
--  |			|   |  |					     --
--  +-------------------+ ...........................			     --
--  | Michael Erdmann   |<--+  |					     --
--  | Some where        |<-----+					     --
--  +-------------------+                  				     --
--  									     --
--  This package provides methods to manipulate the contents of the Object   --
--  header.                                                                  --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--                                                                           --
--  References                                                               --                  
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;                     use Ada.Streams.Stream_IO;
with Ada.Streams;                               use Ada.Streams;
with Ada.Tags;					use Ada.Tags;
use  Ada;

with Util.String_Array;				use Util.String_Array;
with Util.List;
use  Util;

package ODB.Storage_Header is

   type Object is private;
   type Handle is access all Object;

   Unknown_Attribute : exception;
   ---------------------------------------------------------------------------
   -- Description: 
   --    Resgister the attrbute based on the field id as it is registered in
   --    the class it self.
   -- Preconditions:
   --   C.1 - Object is valid.
   --   C.2 - Tag references an object derived from ODB.Persistent.Object
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Register_Attribute(
      This   : in out Object;
      Name   : in Unbounded_String;
      Offset : in Natural );

   procedure Register_Attribute(
      This   : in out Object;
      Id     : in Natural;
      Offset : in Natural;
      Cls    : in Tag );

   ---------------------------------------------------------------------------
   -- Description: 
   --    Return the offset for the named attribute in the header.
   -- Preconditions:
   --    C.1 - The attribute Name has been registered via Register_Attribute
   -- Postconditions:
   --    P.1 - Returns offset
   -- Exceptions:
   --    Unknown_Attribute : C.1 violated
   -- Notes:
   ---------------------------------------------------------------------------  
   function Lookup_Attribute(
      This   : in Object;
      Name   : in String ) return Natural;

   ---------------------------------------------------------------------------
   -- Description:
   --   Return all attribute names which are registered in the header. 
   -- Preconditions:
   --   C.1 - Object is valid
   -- Postconditions:
   --   P.1 - Returns null if the header is empty
   --   P.2 - Returns the point to a string array.
   -- Exceptions:
   -- Notes:
   --   The string array has to be destroyed by means of the 
   --   operation Free in Util.String_Array.
   ---------------------------------------------------------------------------  
   function Attributes(
      This   : in Object ) return String_Array.Handle;

   ---------------------------------------------------------------------------
   -- Description: 
   --    Clear the header.
   -- Preconditions:
   --    C.1 - Object is valid
   -- Postconditions:
   --    P.1 - All attributes are removed from the header.
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Clear(
      This   : in out Object );

   function Class_Name(
      This   : in Object ) return String ;

   procedure Class_Name(
      This   : in out Object;
      Value  : in String );

private
   type Object_Data_Type;
   type Object_Data_Access is access all Object_Data_Type;

   function Initialize return Object_Data_Access;

   type Object is record
         Data : Object_Data_Access := Initialize;
      end record;


end ODB.Storage_Header;
