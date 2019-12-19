-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-storage-file.ads,v $
--  Version         : $Revision: 1.2 $                                       --
--  Description     : Definition of a persitent object                       --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 21-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/07/13 19:49:28 $                           --
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
--  This package provides the environment to save persistent objects in a    --
--  file system. The storage is based upon two files:			     --
--      The pool index file.                                                 --
--	                                                                     --
--      The object pool directory.					     --
--									     --
--  The path for pool index and object may be different.		     --
--                                                                           --
--  The index file contains a list of all objects which are persistent.      --
--                                                                           --
--  Each individual object is stored as a file using the assigned name in    --
--  a directory which denoted by the Pool_Path attribute.		     --
--                                                                           --
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
with ODB;					use ODB;

package ODB.Storage.File is

   type Object is new Storage.Object with private;   
   ---------------------------------------------------------------------------
   -- Description: 
   --     Set the pool path. This path will be used to locate the place
   --     where the object files are stored.
   -- Preconditions:
   --    None
   -- Postconditions:
   --    None
   -- Exceptions:
   --    None
   -- Notes:
   --    None
   --------------------------------------------------------------------------- 
   procedure Pool_Path( 
      This  : in out Object;
      Value : String );

   ---------------------------------------------------------------------------
   -- Description: 
   -- Preconditions:
   --    None
   -- Postconditions:
   --    None
   -- Exceptions:
   --    None
   -- Notes:
   --    None
   --------------------------------------------------------------------------- 
   procedure Index_Path( 
      This  : in out Object;
      Value : String );

   ---------------------------------------------------------------------------
   -- Description: 
   -- Preconditions:
   --    None
   -- Postconditions:
   --    None
   -- Exceptions:
   --    None
   -- Notes:
   --    None
   --------------------------------------------------------------------------- 
   procedure Load(
      This  : in out Object );

   ---------------------------------------------------------------------------
   -- Description: 
   -- Preconditions:
   --    None
   -- Postconditions:
   --    None
   -- Exceptions:
   --    None
   -- Notes:
   --    None
   --------------------------------------------------------------------------- 
   procedure Save( 
      This : in out Object );

private
   type Object_Data_Type;
   type Object_Data_Access is access Object_Data_Type ;

   function Initialize return Object_Data_Access ;

   type Object is new Storage.Object with record
         Data : Object_Data_Access := Initialize;
      end record;

end ODB.Storage.File;
