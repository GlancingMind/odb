-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/util/util-list.ads,v $
--  Description     : Utility package tree                                   --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 19-Aug-2002                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/06/27 15:09:41 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2000-2002 Michael Erdmann                                  --
--                                                                           --
--  XMLS  is free software;  you can redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion. XMLS is distributed in the hope that it will be useful, but WITH-  --
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
--  This generic package provides a basic list for a given data type.        --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
generic
   type Item_Type is private;

package UTIL.List is

   type Handle is private;
   Null_Handle : constant Handle;

   ---------------------------------------------------------------------------
   -- Description:
   --    Create a list.
   -- Preconditions:
   -- Postconditions:
   --    The function returns a list handle.
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  

   function List return Handle;
   ---------------------------------------------------------------------------
   -- Description:
   --    Destroy the list
   -- Preconditions:
   --    C.1 - The list handle is valid.
   -- Postconditions:
   --    All allocated resources are returned.
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Destroy(
      This : in out Handle );

   ---------------------------------------------------------------------------
   -- Description:
   --    Append an element to the given list handle
   -- Preconditions:
   --    C.1 - The list Handle has to be valid.
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Append(
      This : in Handle;
      Item : in Item_Type;
      Sub  : in Handle := Null_Handle );

   ---------------------------------------------------------------------------
   -- Description:
   --    Get the length of the list
   -- Preconditions:
   --	 - C.1 List is valid.
   -- Postconditions:
   --    - P.1 Nothing changed
   --    - P.2 Returns the length of the list. If the list is empty a 0
   --          is returned.
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   function Length(
      This : in Handle ) return Natural;

   ---------------------------------------------------------------------------
   -- Description:
   --    Append an element to the given list handle
   -- Preconditions:
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   type Iterator is abstract tagged private;

   procedure Execute(
      It      : in out Iterator;
      Element : in out Item_Type ) is abstract;

   procedure Execute(
      This : in Handle;
      It   : in out Iterator'Class );

   procedure Stop(
      It   : in out Iterator'Class );

   type List_Reader_Handle is private;
   Null_List_Reader_Handle : constant List_Reader_Handle ;

   ---------------------------------------------------------------------------
   -- Description:
   --    Create a list reader for the given list
   -- Preconditions:
   --    C.1 - List is valid
   -- Postconditions:
   --    P.1 - The list reader points to the begin of the list.
   -- Exceptions:
   -- Notes:
   --    In order to loop through a list, use the First function to obtain
   --    the first element in the list.
   ---------------------------------------------------------------------------  
   function List_Reader(
      This : in Handle ) return List_Reader_Handle;

   ---------------------------------------------------------------------------
   -- Description:
   --    Dstroy the list reader
   -- Preconditions:
   --    C.1 - Listreader is valid
   -- Postconditions:
   --    P.1 - All allocated resources are deallocated.
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Destroy(
      Reader : in out List_Reader_Handle );

   End_Of_List  : exception ;
   Invalid_List : exception ;

   function Child (
      Reader : in List_Reader_Handle ) return Handle;

   procedure Child(
      Reader : in List_Reader_Handle;
      List   : in Handle );

   ---------------------------------------------------------------------------
   -- Description:
   --    Set the reader on the first element 
   -- Preconditions:
   --    C.1 - Listreader is valid
   -- Postconditions:
   --    P.1 - the function returns the first element
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   function First(
      Reader : in List_Reader_Handle ) return Item_Type;

   ---------------------------------------------------------------------------
   -- Description:
   --    Set the reader on the first element 
   -- Preconditions:
   --    C.1 - Listreader is valid
   -- Postconditions:
   --    P.1 - the function returns the first element
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   function Next(
      Reader : in List_Reader_Handle ) return Item_Type;

   ---------------------------------------------------------------------------
   -- Description:
   -- Preconditions:
   --    C.1 - Listreader is valid
   -- Postconditions:
   --    P.1 - the function returns the first element
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Append(
      Reader : in List_Reader_Handle;
      Data   : in Item_Type );

   ---------------------------------------------------------------------------
   -- Description:
   --    Get the item where the current readpoint points to.
   -- Preconditions:
   --    C.1 - Listreader is valid
   -- Postconditions:
   --    P.1 - Nothing is changed.
   --    P.2 - Current element is returned.
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   function Current(
      Reader : in List_Reader_Handle ) return Item_Type;

   ---------------------------------------------------------------------------
   -- Description:
   --    Check for list end
   -- Preconditions:
   --    C.1 - Listreader is valid
   -- Postconditions:
   --    P.1 - Returns true if end of list is reached, else false.
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   function List_End (
      Reader : in List_Reader_Handle ) return Boolean;

private
  type List_Data;
  type Handle is access List_Data;
  Null_Handle : constant Handle := null;

  type List_Reader_Data;
  type List_Reader_Handle is access List_Reader_Data;

  Null_List_Reader_Handle : constant List_Reader_Handle := null;

   type Iterator is abstract tagged record
         Stop : Boolean := False;
      end record;

end UTIL.List;
