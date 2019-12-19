-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-storage.adb,v $
--  Version         : $Revision: 1.3 $                                       --
--  Description     : This package defines the general storage strategy      --
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
--  This package contains the code to save and restore the data of an single --
--  object. The object contents is written into a memory buffer by calling   --
--  the serialize proceudre provided by the class of the object which has to --
--  be stored.                                                               --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;                     use Ada.Streams.Stream_IO;
with Ada.Streams;                               use Ada.Streams;
with Ada.Tags;					use Ada.Tags;
with Ada.Exceptions;           			use Ada.Exceptions;
with Ada.IO_Exceptions;				use Ada.IO_Exceptions;
use  Ada;

with ODB.Classes;				use ODB.Classes;
with ODB.Memory_Stream;				use ODB.Memory_Stream;
use  ODB;

with Util.String_Array;				use Util.String_Array;
use  Util;

package body ODB.Storage is

   -----------------
   -- Save_Object --
   -----------------
   procedure Save_Instance(
      This  : in out Object'Class;
      Item  : in Reference ) is 
      -- write a single object into the object file
      Obs   : Stream_Access renames This.ObS ;
   begin
      Memory_Stream.Clear( OBS );
      Serialize( Item.all, This.Header, Obs );
   end Save_Instance;   

   -----------------
   -- Load_Object --
   -----------------
   procedure Load_Instance(
      This   : in out Object'Class;
      Name   : in String;
      Cls    : in String;
      Phase  : in Load_Phase_Type )  is
      -- load an object from the object file
      Obs    : Stream_Access renames This.ObS ;
      Result : Reference        := null; 
   begin
      if Phase = Loading then 
         Result := Classes.Factory( Internal_Tag(Cls) ).all;
	 Name_Object( Result, Name );
      else
         Result := Lookup_Object( Name );
      end if;

      if Result = null then
         Raise_Exception(Unresolved_Reference'Identity, Name);
      end if;

      Deserialize( Result.all, This.Header, Obs );
   end Load_Instance;

end ODB.Storage;
