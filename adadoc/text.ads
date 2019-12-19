-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/adadoc/text.ads,v $
--  Description     : Persistent message qeue handling                       --
--  Author          : Michael Erdmann                                        --
--  Created         : 31.12.2001                                             --
--  Last Modified By: $Author: merdmann $
--  Last Modified On: $Date: 2003/06/23 07:37:04 $
--  Status          : $State: Exp $
--                                                                           --
--  Copyright (C) 2001 Michael Erdmann                                       --
--                                                                           --
--  ASCL  is free software;  you can redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
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
--  This software is implemented to work with GNAT, the GNU Ada compiler.    --
--                                                                           --
--  Functional Description
--  ======================
--
--  Component Data
--  ==============
--
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--                                                                           --
--                                                                           --
--  Contact                                                                  --
--  =======                                                                  --
--  Error reports and suggestions shall be send to the Address:              --
--               Michael.Erdmann@snafu.de                                    --
--                                                                           --
--  General Informations will be found at:                                   --
--               purl:/net/michael.erdmann                                   --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded;                  use Ada.Strings.Unbounded;

package Text is

   type Paragraph_Type is private;

   type Text_Array_Type is array( Positive range <> ) of Unbounded_String;
   type Text_Array_Access is access Text_Array_Type;

   ---------------------------------------------------------------------------
   --| Description    :
   --| Preconditions  :
   --| Postconditions :
   --| Exceptions     :
   --| Note           :
   ---------------------------------------------------------------------------

   procedure Add(
      This : in out Paragraph_Type;
      S    : in String );

   function To_Array(
      This   : in Paragraph_Type ) return Text_Array_Access;

   procedure Clear(
      This : in out Paragraph_Type );

   function Is_Empty(
      This : in Paragraph_Type ) return Boolean;

private

   type Paragraph_Data_Type;
   type Paragraph_Data_Access is access Paragraph_Data_Type;

   type Paragraph_Type is record
         Data : Paragraph_Data_Access := null;
      end record;


end Text;


