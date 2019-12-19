-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-storage.ads,v $
--  Version         : $Revision: 1.3 $                                       --
--  Description     : A collection of persistent objects                     --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 30-June-2003                                           --
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
use  Ada;

with ODB.Persistent;				use ODB.Persistent;
with ODB.Storage_Header;			use ODB.Storage_Header;

with Util.List;

package ODB.Storage is

   type Object is tagged private;
   type Handle is access all Object'Class;

   type Load_Phase_Type is ( Loading , Resolving );
   ---------------------------------------------------------------------------
   -- Description: 
   --    Save the object in the storage media
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Save_Instance(
      This  : in out Object'Class;
      Item  : in Reference );

   ---------------------------------------------------------------------------
   -- Description: 
   --    Load a named object from the storage media
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Load_Instance(
      This   : in out Object'Class;
      Name   : in String;
      Cls    : in String;
      Phase  : in Load_Phase_Type ); 

private
   type Object is tagged record
         Header  : Storage_Header.Object;
         Index   : Stream_Access;		 -- Index stream
	 Obs     : Stream_Access;		 -- Object stream
      end record;

end ODB.Storage;
