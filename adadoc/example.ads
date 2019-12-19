--
-- $Id: example.ads,v 1.1.1.1 2003/06/23 07:37:03 merdmann Exp $
--
---------------------------------------------------------------------------
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Package Information
-- ===================
--
-- This is a module which does nothing else then beeing
-- a test modules for adadoc. Please note, that all CVS
-- information is striped off.
--
-- It performs the following functions:
--
--     Nothing
--
--     Even lesser
--
--     Sometimes causing faults
--
-- This package is using only the header comments method. The
-- header keywords are not used.
--
-- Reuse Information
-- =================
--
-- This is a headlined section, which will be visible in the
-- TOC. The key information is the simple fact, that the headline
-- is underlined.
-------------------------------------------------------------------------

package Example is

   --
   -- this function returns a nice constant value
   --
   function True return Boolean ;

   function True_1 return Boolean ;

   -- End of package.
end Example;


