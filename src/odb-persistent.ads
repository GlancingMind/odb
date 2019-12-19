-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-persistent.ads,v $
--  Version         : $Revision: 1.14 $                                       --
--  Description     : Definition of a persitent object                       --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 21-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/10/06 04:27:57 $                           --
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
--  This package provides an persistent object storage. All object expected  --
--  to be persistent have to be derived from the Persistent.Object type.     --
--  If such an object is to be allocated always use the Reference type       --
--  Only this access type is associated with the persistent storage pool.    --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  R.1 - This package is not designed to be task save                       --
--  R.2 - The object storage cannot be shared between applications.          --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with System;                      		use System;
with System.Storage_Pools;        		use System.Storage_Pools;
with System.Storage_Elements;     		use System.Storage_Elements;

with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;                     use Ada.Streams.Stream_IO;
with Ada.Streams;                               use Ada.Streams;
use  Ada;

with ODB.Storage_Header;			use ODB.Storage_Header;
with ODB;					use ODB;

with Util.List;				
use  Util;

package ODB.Persistent is

   -- this type is the root type of all persistent objects
   type Object is abstract tagged private;

   -- use this as references to persistent types
   type Reference is access all Object'Class;

   -- the following two procedures should be ignored, since they
   -- are required internaly to read and write references in the 
   -- object store. They should never be called directly, only 
   -- by means of the Input, Output attributes.
   procedure Write_Reference(
      Stream : access Root_Stream_Type'Class;
      Item   : in Reference );
   for Reference'Output use Write_Reference;

   function Read_Reference(
      Stream : access Root_Stream_Type'Class ) return Reference;
   for Reference'Input use Read_Reference;

   -- This execption indicates that an object reference cannot be 
   -- resolved.
   Unresolved_Reference : exception ;
   Unknown_Attribute    : exception ;

   -- Maximum number of objects in the Persistent pool.
   Max_Nbr_Of_Objects   : constant Natural := 32_000;

   ---------------------------------------------------------------------------
   -- Description: 
   --    This procedure writes out the object into the stream provided by
   --    the Persistency module. Each field of the object may be written 
   --    out by means of the Output attribute. References to other objects
   --    are automatically expanded into a logical representation which
   --    can be read in later if the Method Reference'Output is used.
   -- Preconditions:
   --    None
   -- Postconditions:
   --    None
   -- Exceptions:
   --    None
   -- Notes:
   --    Please note, that the application delveloper has to provide this
   --    implemenation, but it should never be called somewhere in the 
   --    application.
   --    This procedure is only called by the persistency manager and should
   --    be placed in the private section.
   ---------------------------------------------------------------------------  
   procedure Serialize(
      Item   : in out Object; 
      Header : in out Storage_Header.Object;
      S      : in Stream_Access ) is abstract;

   ---------------------------------------------------------------------------
   -- Description:
   --    Read in all field of the given object. This procedure is called
   --    by the persistency manager when loading the objects from an external
   --    file.
   --    The procedure may read each field of the object by means of the 
   --    Input attribute except for references to other instances in the 
   --    object space. These entires have to be loaded by means of the 
   --    Reference'Input method.
   -- Preconditions:
   --    None
   -- Postconditions:
   --    It is assumed, that all fields have been read.
   -- Exceptions:
   -- Notes:
   --    Please ensure, that the number of field written is identical to the 
   --    number of fields read in. If not the file becomes unreadable.
   --    This procedure is only called by the persistency manager and should
   --    be placed in the private section.
   ---------------------------------------------------------------------------  
   procedure Deserialize(
      Item   : in out Object; 
      Header : in out Storage_Header.Object;
      S      : in Stream_Access ) is abstract;

   ---------------------------------------------------------------------------
   -- Description:
   --    The factory funtion creates a new object of the implenting class.
   --    Under normal circumstances this will be an new Object operation. This
   --    function will be registered together with the External_Tag of the 
   --    implementation in order to allow the persistency manager to create
   --    instances when reading in the data from a file.
   -- Preconditions:
   --    None
   -- Postconditions:
   --    The function returns a reference to the newly created object
   -- Exceptions:
   -- Notes:
   --    Please be aware, that any initialization done in this procedure will
   --    be overwritten later when the actual object is restored.
   ---------------------------------------------------------------------------  
   function Factory return Reference is abstract;
   
   ---------------------------------------------------------------------------
   -- Description:
   --    Returns the object identifier
   -- Preconditions:
   --    None
   -- Postconditions:
   --    Returns the object identifier which is a Natural number.
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   function Object_Id( 
      Ref   : in Reference ) return Natural;
   ---------------------------------------------------------------------------
   -- Description:
   --    Assign a unique name to the object
   -- Preconditions:
   --    C.1 - The Reference has to be valid
   --    C.2 - Name has to be unique.
   -- Postconditions:
   -- Exceptions:
   --    Invalid_Object - violation of C.1
   --    Duplicate_Name - violation of C.2
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Name_Object(
      Ref  : in Reference ;
      Name : in String );      

   ---------------------------------------------------------------------------
   -- Description:
   --    Ask for the object name
   -- Preconditions:
   --    P.1 - Reference is valid
   -- Postconditions:
   --    Returns a string with the name of the object
   -- Exceptions:
   --    None
   -- Notes:
   ---------------------------------------------------------------------------  
   function Object_Name(
      Ref  : in Reference ) return String;

   ---------------------------------------------------------------------------
   -- Description:
   --    Lookup the object from the object table.
   -- Preconditions:
   -- Postconditions:
   --    Returns the reference if the object does exist.
   --    Returns null if the object does not exist.
   -- Exceptions:
   --    None
   -- Notes:
   ---------------------------------------------------------------------------  
   function Lookup_Object(
      Name : in String ) return Reference;
   ---------------------------------------------------------------------------
   -- Description:
   --    Check if the referenced object is persistent
   -- Preconditions:
   --    C.1 - The Reference has to be valid
   -- Postconditions:
   --    - The function returns true, if the object is persistent.
   -- Exceptions:
   --    Invalid_Object - violation of C.1
   -- Notes:
   ---------------------------------------------------------------------------  
   function Is_Persistent(
      Ref   : in Reference ) return Boolean;

   ---------------------------------------------------------------------------
   -- Description:
   --    Return the reference to an object from the object identifier
   -- Preconditions:
   --    None
   -- Postconditions:
   --    Returns the reference to the object if it exists
   --    Returns null, if the object does not exist.
   -- Exceptions:
   --    None
   -- Notes:
   ---------------------------------------------------------------------------  
   function Get_Reference( 
      Id    : in Natural;
      Force : Boolean := False ) return Reference;

   ---------------------------------------------------------------------------
   -- Description:
   --    Number of objects in the persistent pool
   -- Preconditions:
   --    None
   -- Postconditions:
   --    - Returns the number of already stored objects in the pool.
   -- Exceptions:
   --    None
   -- Notes:
   ---------------------------------------------------------------------------  
   function Nbr_Of_Objects return Natural;

private

   type Attribute_Element_Type is record
         Name   : Unbounded_String := Null_Unbounded_String;
	 Offset : Natural          := 0;
      end record;

   package Attribute_Directory is new Util.List( 
      Item_Type => Attribute_Element_Type );

   ------------
   -- Object --
   ------------
   type Object is abstract tagged record
         Name   : Unbounded_String := Null_Unbounded_String;
         Attrib : Attribute_Directory.Handle;   -- list of attributes
      end record;

   ------------------------
   -- Instance_Pool_Type --
   ------------------------
   type Instance_Pool_Type is new Root_Storage_Pool with null record;
  
   procedure Allocate( 
      Pool            : in out Instance_Pool_Type;
      Storage_Address : out    Address;
      Size            : in     Storage_Count;
      Alignment       : in     Storage_Count );

   procedure Deallocate( 
      Pool            : in out Instance_Pool_Type;
      Storage_Address : in     Address;
      Size            : in     Storage_Count;                       
      Alignment       : in     Storage_Count );

   function Storage_Size ( 
      Pool : in Instance_Pool_Type ) return Storage_Count;

   Instance_Pool: Instance_Pool_Type;
   for Reference'Storage_Pool use Instance_Pool;
   
end ODB.Persistent;
