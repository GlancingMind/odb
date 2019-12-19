-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/util/util-lock_table.adb,v $
--  Version         : $Revision: 1.2 $                                       --
--  Description     : Resource Table Handler                                 --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 11-Oct-2003                                            --
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
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Text_IO;				use Ada.Text_IO;
with Ada.Task_Identification;           	use Ada.Task_Identification;

with Unchecked_Deallocation;

package body Util.Lock_Table is

   Version : constant String := 
      "$Id: util-lock_table.adb,v 1.2 2003/10/12 14:05:06 merdmann Exp $";

   Plus_Code : Natural := 0;

   Null_Lock_Handle : constant Lock_Handle_Type := ( Id => 0, Plus_Code => 0);

   -------------------
   -- Resource_Type --
   -------------------
   protected type Resource_Type is 
      entry Seize( 
         Id   : in Natural; 
	 User : in Task_ID;
	 H    : out Lock_Handle_Type );

      procedure Release(
         H  : in out Lock_Handle_Type );

      procedure Audit;

   private
      Handle    : Lock_Handle_Type := Null_Lock_Handle;
      Watch_Dog : Natural := 0;
      User      : Task_ID;
   end Resource_Type;

   protected body Resource_Type is 

      -----------
      -- Seize --
      -----------
      entry Seize( 
         Id   : in Natural;
	 User : in Task_ID;
	 H    : out Lock_Handle_Type)  when Handle = Null_Lock_Handle is 
      begin
	 Plus_Code := Plus_Code + 1;

         Handle.Id := Id;
	 Handle.Plus_Code := Plus_Code;

	 Watch_Dog := 0;

	 H := Handle;
      end Seize;

      -------------
      -- Release --
      -------------
      procedure Release(
         H : in out Lock_Handle_Type ) is 
      begin
         if H = Handle then
            Handle    := Null_Lock_Handle ;
	    Watch_Dog := 0;

	    H := Null_Lock_Handle;
	 else
	    raise Invalid_Lock_Handle;
	 end if;
      end Release;

      -----------
      -- Audit --
      -----------
      procedure Audit is
      begin
         if Handle = Null_Lock_Handle then
	    return;
	 end if;

         if Watch_Dog > 2 then
	    Handle    := Null_Lock_Handle;
	    Watch_Dog := 0;

            if Is_Callable( User ) then
               Abort_Task( User );
	    end if;

	 else 
            Watch_Dog := Watch_Dog + 1;
	 end if;
      end Audit;

   end Resource_Type;

   ----------------
   -- Lock_Array --
   ----------------
   type Lock_Array is array( Positive range <> ) of Resource_Type;
 
   ----------------
   -- Audit_Task --
   ----------------
   task type Audit_Task( Data : Object_Data_Access );
   type Audit_Task_Access is access Audit_Task;

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type( Size : Positive ) is record
         Locks : Lock_Array( 1..Size );         
	 Hash  : Resource_Hash.Table_Type( Size );
	 Audit : Audit_Task_Access := null;
      end record;

   ----------
   -- Free --
   ----------
   procedure Free is
      new Unchecked_Deallocation( Object_Data_Type, Object_Data_Access);
   
   ----------------
   -- Audit_Task --
   ----------------
   task body Audit_Task is
      -- this task runs every second and cleans up locks which
      -- are older then 3 seconds. 
   begin
      loop
         delay 1.0;
         for i in Data.Locks'Range loop
	    Data.Locks(i).Audit;
	 end loop;
      end loop;
   end Audit_Task;
   
   ----------------
   -- Initialize --
   ----------------
   procedure Initialize(
      This : in out Object ) is 
   begin
      if This.Data = null then
         This.Data := new Object_Data_Type( This.Size );
      end if;

      This.Data.Audit := new Audit_Task( This.Data );
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize(
      This : in out Object ) is 
      Data : Object_Data_Access renames This.Data;
   begin
      abort Data.Audit.all;
      Free( Data );
   end Finalize;

   -----------
   -- Seize --
   -----------
   function Seize(
      This   : in Object;
      Id     : in Natural ) return Lock_Handle_Type is
      Data   : Object_Data_Access renames This.Data;
      Result : Lock_Handle_Type ;
      use  Resource_Hash;

      H  : Natural; 
   begin
      Put( Data.Hash, Id, H );
      Data.Locks(H).Seize(Id, Current_Task, Result);
      return Result;
   end Seize;

   -------------
   -- Release --
   -------------
   procedure Release(
      This : in out Object;
      Lock : in out Lock_Handle_Type ) is
      use Resource_Hash;
       
      Data : Object_Data_Access renames This.Data;
      H    : Natural := Get( Data.Hash, Lock.Id );
   begin
      Data.Locks(H).Release( Lock );       
   end Release;

end Util.Lock_Table;
