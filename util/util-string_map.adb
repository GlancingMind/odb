-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/util/util.ads,v $
--  Version         : $Revision: 1.2 $                                       --
--  Description     : ODB Utility root package                               --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 21-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/06/27 15:09:41 $                           --
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
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------

package body Util.String_Map is

   Version : constant String := "$Id$" ;

   ---------
   -- Put --
   ---------
   procedure Put(
      This  : in out Object;
      Key   : in String;
      Value : in Value_Type ) is 

      H     : Natural;
      use String_Hash;

   begin
      Put( This.Hash, To_Unbounded_String(Key), H );
      This.Value(H) := Value;
   end Put;

   ---------
   -- Get --
   ---------
   function Get(
      This : in Object;
      Key  : in String ) return Value_Type is 

      use String_Hash;
   begin
      return This.Value(Get( This.Hash, To_Unbounded_String(Key)));
   end Get;
      
   --------------
   -- Is_Empty --
   --------------
   function Is_Empty(
      This : in Object ) return Boolean is
   begin
      return Is_Empty( This.Hash );
   end Is_Empty;

   ---------
   -- Key --
   ---------
   function Key(
      This : in Object;
      H    : in Natural ) return String is
   begin
      return To_String( Key( This.Hash, H ) );
   end Key;


end Util.String_Map;
