-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/util/util-lock_table.ads,v $
--  Version         : $Revision: 1.2 $                                       --
--  Description     : ODB Utility root package                               --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 21-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/10/12 14:05:06 $                           --
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
--  This package provides a general resource locking functionality. The      --
--  call need to address each resource by means of a ntural nunber. This     --
--  identifier will be stored in the so call lock_table object. If a         --
--  resource is allready seized the seize function block until the resource  --
--  becomes available.                                                       --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Finalization;                  use Ada.Finalization;
use  Ada;

with Util.Hash_Table;
use  Util;

package Util.Lock_Table is

   type Object( Size : Positive ) is private;

   type Lock_Handle_Type is private;

   Invalid_Lock_Handle : exception;

   ---------------------------------------------------------------------------
   -- Description: 
   --    Seize a resource which is identified by the given natural number. The
   --    function blocks till the given resource is available.
   -- Preconditions:
   -- Postconditions:
   --    C.1 - Returns a so called lock identifier which has to be used 
   --          to unlock the seized resource.
   -- Exceptions:
   -- Notes:
   --------------------------------------------------------------------------- 
   function Seize(
      This : in Object;
      Id   : in Natural ) return Lock_Handle_Type;

   ---------------------------------------------------------------------------
   -- Description: 
   --    Make the resource identified by the lock identifier available for 
   --    other seizures.
   -- Preconditions:
   --    P.1 - Lock identifier exisits.
   -- Postconditions:
   --    C.1 - The resource is available for other seizures.
   -- Exceptions:
   -- Notes:
   --------------------------------------------------------------------------- 
   procedure Release(
      This : in out Object;
      Lock : in out Lock_Handle_Type );

private
   type Object_Data_Type;
   type Object_Data_Access is access all Object_Data_Type;

   type Object( Size : Positive ) is new Controlled with record 
         Data : Object_Data_Access := null;
      end record;

   procedure Initialize(
      This : in out Object );

   procedure Finalize(
      This : in out Object );

   type Lock_Handle_Type is record
         Id        : Natural := 0;
	 Plus_Code : Natural := 0;
      end record;

   package Resource_Hash is new Hash_Table( Key_Type => Natural );

end Util.Lock_Table;
