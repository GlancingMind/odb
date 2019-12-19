-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /cvsroot/gnade/odb/src/odb-xml.ads,v $
--  Description     : XML Elements of the ODB files                          --
--  Author          : Michael Erdmann <Michael.Erdmann@snafu.de>             --
--  Created On      : 04-Oct-2003                                            --
--  Last Modified By: $Author: merdmann $			             --
--  Last Modified On: $Date: 2003/10/06 04:27:57 $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2003 Michael Erdmann                                       --
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
--  This package contains the XML tag names and the name space specification.--
--  Changes to the tag names should only be done here in order to keep the   --
--  Loader and the Writer symetrical.                                        --
--                                                                           --
--                                                                           --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  Syntax changes need to be done in the Object Loader and Writer.          --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
-------------------------------------------------------------------------------
package ODB.XML is

   NS : constant String := "http://gnade.sourceforge.net/2003/10/03-cls";

   Tag_Class     : constant String := "class" ;
   Tag_Header    : constant String := "header";
   Tag_Attribute : constant String := "attribute" ;
   Tag_Data      : constant String := "data";

private

   Version : constant String := 
       "$Id: odb-xml.ads,v 1.1 2003/10/06 04:27:57 merdmann Exp $";

end ODB.XML;
