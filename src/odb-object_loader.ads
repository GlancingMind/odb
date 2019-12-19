-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-object_loader.ads,v $
--  Description     : XML Reader                                             --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 19-Aug-2002                                            --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/10/06 04:27:57 $                           --
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
--  This package allows to read in an XML document and to read the contents  --
--  of the document sequentially.                                            --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Streams;                       	use Ada.Streams;
with Ada.Streams.Stream_IO;             	use Ada.Streams.Stream_IO;

with Sax.Exceptions;
with Sax.Locators;
with Sax.Readers;
with Sax.Attributes;
with Sax.Models;
with Unicode.CES;

with ODB.Storage_Header;			use ODB.Storage_Header;
use  ODB;

package ODB.Object_Loader is

   type Object is private;
   type Handle is access Object;

   ---------------------------------------------------------------------------
   -- Description: 
   --    Read the named object from the application pool
   -- Preconditions:
   --    C.1 - The stream has to be valid
   -- Postconditions:
   -- Exceptions:
   -- Notes:
   ---------------------------------------------------------------------------  
   procedure Read(
      This  : in out Object;
      Name  : in String );

   function Header(
      This : in Object ) return Storage_Header.Object;

   function Stream( 
      This : in Object ) return Stream_Access ;

   function InstanceOf(
      This : in Object ) return String;

   procedure Path(
      This   : in out Object;
      Value  : in String );


   Invalid_Object : exception ;

private
   type Object_Data_Type;
   type Object_Data_Access is access Object_Data_Type;

   function Initialize return Object_Data_Access;

   type Object is new Sax.Readers.Reader with record
         Data : Object_Data_Access := Initialize;
      end record;

   -- these methods are required by the SAX interface
   procedure Warning
     (This : in out Object;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class);

   procedure Error
     (This : in out Object;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);

   procedure Fatal_Error
     (This : in out Object;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);

   procedure Start_Element
     (This       : in out Object;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);

   procedure End_Element
     (This : in out Object;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");

   procedure Start_Cdata (
      This : in out Object);

   procedure End_Cdata (
      This : in out Object);

   procedure Characters(
      This : in out Object;
      Ch      : Unicode.CES.Byte_Sequence);

   procedure Set_Document_Locator
     (This : in out Object;
      Loc     : access Sax.Locators.Locator'Class);

   procedure Start_Prefix_Mapping
     (This    : in out Object;
      Prefix  : Unicode.CES.Byte_Sequence;
      URI     : Unicode.CES.Byte_Sequence);

   procedure End_Prefix_Mapping
     (This    : in out Object;
      Prefix  : Unicode.CES.Byte_Sequence);

end ODB.Object_Loader;
