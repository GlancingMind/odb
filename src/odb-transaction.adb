-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-transaction.adb,v $
--  Version         : $Revision: 1.2 $                                       --
--  Description     : ODB Transaction Manager                                --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 21-June-2003                                           --
--  Last Modified By: $Author: merdmann $				     --
--  Last Modified On: $Date: 2003/10/12 14:05:05 $                           --
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
with Ada.Tags;					use Ada.Tags;
with Ada.Text_IO;				use Ada.Text_IO;
use  Ada;

with Unchecked_Deallocation;

with Util.Lock_Table;				use Util.Lock_Table;
use  Util;

with ODB.Classes;				use ODB.Classes;
use  ODB;

package body ODB.Transaction is

   Version : constant String := 
      "$Id: odb-transaction.adb,v 1.2 2003/10/12 14:05:05 merdmann Exp $";

   type Lock_Table_Access is access Lock_Table.Object;

   TT : Lock_Table_Access := null;

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type is record
         Id     : Natural := 0;
         Backup : Persistent.Reference;
	 Handle : Lock_Table.Lock_Handle_Type;
      end record;

   ----------
   -- Free --
   ----------
   procedure Free is
      new Unchecked_Deallocation( Object_Data_Type, Object_Data_Access);

   ----------
   -- Free --
   ----------
   procedure Free is
      new Unchecked_Deallocation( Lock_table.Object, Lock_Table_Access);

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize(
      This : in out Object ) is
   begin
      if This.Data = null then
         This.Data := new Object_Data_Type;
      end if;      
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize(
      This : in out Object ) is 
   begin
      Free( This.Data );
   end Finalize;
   
   ----------------
   -- Initialize --
   ----------------   	
   procedure Initialize(
      Size : in Natural ) is 
   begin
      if TT /= null then
         raise Invalid_Usage;
      end if;
      TT := new Lock_Table.Object( Size );
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize is
   begin
      Free( TT );      
   end Finalize;

   -----------
   -- Start --
   -----------
   procedure Start(
      This     : in out Object;
      Instance : in Persistent.Reference ) is
      -- start a transaction
      Data     : Object_Data_Access renames This.Data;
   begin
      Data.Id     := Object_ID( Instance );
      Data.Handle := Seize( TT.all, Data.ID );

      Data.Backup := Classes.Factory( Instance.all'Tag ).all;
      Data.Backup.all := Instance.all;   
   end Start;

   ------------
   -- Commit --
   ------------
   procedure Commit(
      This   : in out Object ) is 
      Data   : Object_Data_Access renames This.Data;
      Target : Reference := Get_Reference( Data.ID ); 
   begin
      if Data.Id = 0 then
         raise Invalid_Transaction;
      end if;

      Release( TT.all, Data.Handle );
   end Commit;

   ------------
   -- Cancel --
   ------------
   procedure Cancel(
      This : in out Object ) is 
      Data : Object_Data_Access renames This.Data;
   begin
      if Data.Id = 0 then
         raise Invalid_Transaction;
      end if;

      Release( TT.all, Data.Handle );
      Data.Id := 0;
   end Cancel;

   -------------
   -- Rolback --
   -------------
   procedure Rollback(
      This : in out Object ) is 
      Data : Object_Data_Access renames This.Data;
   begin
      if Data.Id = 0 then
         raise Invalid_Transaction;
      end if;

      Get_Reference( Data.ID ).all := Data.Backup.all;
   end Rollback;

end ODB.Transaction;
