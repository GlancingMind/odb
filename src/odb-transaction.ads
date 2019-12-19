-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-transaction.ads,v $
--  Version         : $Revision: 1.2 $                                       --
--  Description     : Transction Package                                     --
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
--  This package implements a elementary transaction system by providing the --
--  cpability of locking an instance.                                        --
--  At the begon of each transation, a copy of the original persistent object--
--  will be created and the object is locked for other transactions. From    --
--  this point on, the owner process of the transaction may manipulate the   --
--  contents of the object.                                                  --
--  Be aware, since every process may retrieve at any time the object        --
--  reference from the persistency manager it is possible that a thread may  --
--  see intermediate results unless the implementation starts first a        --
--  transaction.                                                             --
--                                                                           --
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

with ODB.Persistent;			use ODB.Persistent;
use  ODB;

package ODB.Transaction is

   type Object is private;

   Invalid_Transaction : exception;
   Invalid_Usage       : exception;

   ---------------------------------------------------------------------------
   -- Description: 
   --    Initialize the transaction manager package to handle the given 
   --    number of transactions.
   -- Preconditions:
   --    P.1 - Transaction Manager not initialized.
   -- Postconditions:
   --    C.1 - The object value is the same as at the time of the invokation
   --          of the start method.
   --    C.2 - Transaction is still active.
   -- Exceptions:
   --    P.1 - Unvalid_Use
   -- Notes:
   --------------------------------------------------------------------------- 
   procedure Initialize(
      Size : in Natural );

   ---------------------------------------------------------------------------
   -- Description: 
   --    Shuttdown the transaction management
   -- Preconditions:
   --    P.1 - Transaction manager is Initialized.
   -- Postconditions:
   --    C.1 - All tranaction data is lost.
   --    C.2 - Transaction Monitoring is stoped.
   -- Exceptions:
   -- Notes:
   --------------------------------------------------------------------------- 
   procedure Finalize;

   ---------------------------------------------------------------------------
   -- Description: 
   --    Start a transaction. This procedure blocks if other processes 
   --    do have an active transaction. The transaction is closed either 
   --    by the Commit of the Cancel method.
   -- Preconditions:
   --    P.1 - The transaction package has been initialized by means of the 
   --          initialize procedure.
   -- Postconditions:
   --    C.1 - A copy of the given persistent object is created.
   --    C.2 - The object is locked for other transactions. The method will
   --          block until the instance has been aquiered by the process.
   -- Exceptions:
   -- Notes:
   --------------------------------------------------------------------------- 
   procedure Start(
      This     : in out Object;
      Instance : in Persistent.Reference );

   ---------------------------------------------------------------------------
   -- Description: 
   --    Close the transaction. 
   -- Preconditions:
   --    P.1 - Transaction has been Started
   -- Postconditions:
   --    C.1 - The backup copy of the object is deleted and the object is 
   --          available for other processes to aquiere the object by means
   --          of a start operation.
   -- Exceptions:
   -- Notes:
   --------------------------------------------------------------------------- 
   procedure Commit(
      This : in out Object );

   ---------------------------------------------------------------------------
   -- Description: 
   --    Cancel the current transaction.
   -- Preconditions:
   --    P.1 - Transction has been started
   -- Postconditions:
   --    C.1 - The Original value of the object is restored
   --    C.2 - object is available for other transactions.
   -- Exceptions:
   -- Notes:
   --------------------------------------------------------------------------- 
   procedure Cancel(
      This : in out Object );

   ---------------------------------------------------------------------------
   -- Description: 
   --    Restore the contents of the object to the original value
   -- Preconditions:
   --    P.1 - Transaction is active
   -- Postconditions:
   --    C.1 - The object value is the same as at the time of the invokation
   --          of the start method.
   --    C.2 - Transaction is still active.
   -- Exceptions:
   -- Notes:
   --------------------------------------------------------------------------- 
   procedure Rollback(
      This : in out Object );

private
   type Object_Data_Type;
   type Object_Data_Access is access Object_Data_Type;

   type Object is new Controlled with record
         Data : Object_Data_Access := null;
      end record;

   procedure Initialize(
      This : in out Object );

   procedure Finalize(
      This : in out Object );
      	
end ODB.Transaction;
