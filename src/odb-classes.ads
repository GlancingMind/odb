-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-classes.ads,v $
--  Version         : $Revision: 1.2 $                                       --
--  Description     : Definition of a persitent object                       --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 21-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/10/11 19:26:32 $                           --
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
--  This package maintains a list of all classes (packages) which are        --
--  derived from the persistent type ODB.Persistent.Object if the package    --
--  has registered it self.						     --
--									     --
--  The following information is available per class;			     --
--  - Pointer to the so called factory					     --
--  - All attribute names of a class.					     --
--                                                                           --
--  If the application shall support persistence, the following pieces of    --
--  code has to be added some where during the initialization.		     --
--									     --
--  Class_ID := Classes.Register_Factory( Object'Tag, Factory'Access );      --
--									     --
--  Classes.Attribute( Class_ID, "D_Name",     D_Name );		     --
--  Classes.Attribute( Class_ID, "D_Used",     D_Used );      		     --
--  Classes.Attribute( Class_ID, "D_Elements", D_Elements ); 		     --
--   
--  This example registers the funtion Factory with Object'tag and adds the  --
--  attributes D_Name, D_Used and D_Elements.				     --
--                                                                           --
--  The factor function creates an instance of the class for which is  the   --
--  function has been registered.                                            --
--  Restrictions                                                             --
--  In order to serialize/deserialize the object data, for each attribute of --
--  a persistent object a name has to be defined. This is done by means of   --
--  the Attribute functions (for a typlica example refer to ODB.Collection). --
--									     --
--  Restrictions                                                             --
--  ============                                                             --
--  R.1 -                                                                    --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Tags;					use Ada.Tags;
with Ada.Exceptions;           			use Ada.Exceptions;
use  Ada;

with ODB.Persistent;				use ODB.Persistent;

package  ODB.Classes is

   -- this pointer referes to the Factory function belonging to a certain
   -- class.
   type Factory_Access is access function return Reference ;

   Invalid_Attribute_ID : exception;

   ---------------------------------------------------------------------------
   -- Description:
   --    Register the factory with the given class name.
   -- Preconditions:
   --    - Class is not yet registered
   -- Postconditions:
   --    - Factory will return a valid value
   -- Exceptions:
   --    None
   -- Notes:
   ---------------------------------------------------------------------------  
   function Register_Factory(
      Name    : in Tag;
      Creator : in Factory_Access ) return Natural;

   ---------------------------------------------------------------------------
   -- Description:
   --    Find the factory and return the pointer to the factory function.
   -- Preconditions:
   --    - Factory has been registered for the given given persistent class
   -- Postconditions:
   --    None
   -- Exceptions:
   --    None
   -- Notes:
   ---------------------------------------------------------------------------  
   function Factory(
      Name : in Tag ) return Factory_Access;

   ---------------------------------------------------------------------------
   -- Description:
   --    Returns the attribute name for a given attribute id.
   -- Preconditions:
   --    - Function returns attribute name
   -- Postconditions:
   --    None
   -- Exceptions:
   --    None
   -- Notes:
   ---------------------------------------------------------------------------  
   function Attribute(
      This   : in Tag;
      Id     : in Natural ) return String ;

   ---------------------------------------------------------------------------
   -- Description:
   --    Associate an attribute name and a id with the class id as it 
   --    has been returned by Register_Factory.
   -- Preconditions:
   --    - Factory has been registered for the given given persistent class
   --    - Attrbiute has not been defined before.
   -- Postconditions:
   --    None
   -- Exceptions:
   --    None
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Attribute(
      This : in Natural;
      Name : in String;
      Id   : in Natural );

   ---------------------------------------------------------------------------
   -- Description:
   --    Translate a attribute name into a attribute id. 
   -- Preconditions:
   --    - Package class has been registered
   -- Postconditions:
   --    - Return 0 if not found
   --    - Returns id if the attribute has been associated with Attute.
   -- Exceptions:
   --    None
   -- Notes:
   ---------------------------------------------------------------------------  
   function Attribute(
      This   : in Tag;
      Name   : in String ) return Natural;

end ODB.Classes;
