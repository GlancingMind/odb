-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-attribute_dictionary.ads,v $
--  Description     : Attribute Table                                        --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 19-Aug-2002                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/10/08 04:43:55 $                           --
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
--  This package provides a dictionary of attribute/value pairs.             --
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
with Ada.Strings.Unbounded;                  use Ada.Strings.Unbounded;

package ODB.Attribute_Dictionary is

   type Object( Size : Natural ) is private;
   type Handle is access all Object;

   procedure Add( 
      This  : in out Object;
      Name  : in String;      
      Value : in String );

   function Get(
      This  : in Object;
      Name  : in String ) return String;

   procedure Clear(
      This  : in out Object );

private

   type Object_Data_Type( Size : Natural );
   type Object_Data_Access is access all Object_Data_Type ;

   function Initialize(
      Size : in Natural ) return Object_Data_Access ;

   type Object( Size : Natural ) is record
         Data : Object_Data_Access := Initialize( Size );
      end record; 


end ODB.Attribute_Dictionary;
