--------------------------------------------------------------------------------
--                                                                            --
-- Copyright (C) 2004, RISC OS Ada Library (RASCAL) developers.               --
--                                                                            --
-- This library is free software; you can redistribute it and/or              --
-- modify it under the terms of the GNU Lesser General Public                 --
-- License as published by the Free Software Foundation; either               --
-- version 2.1 of the License, or (at your option) any later version.         --
--                                                                            --
-- This library is distributed in the hope that it will be useful,            --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of             --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU           --
-- Lesser General Public License for more details.                            --
--                                                                            --
-- You should have received a copy of the GNU Lesser General Public           --
-- License along with this library; if not, write to the Free Software        --
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA    --
--                                                                            --
--------------------------------------------------------------------------------

-- @brief Misc. conversion subprograms.
-- $Author$
-- $Date$
-- $Revision$

package RASCAL.Convert is

   Invalid_Digit_Error : Exception;
   Result_Size_Error   : Exception;

   --
   -- Returns the string representation of hexadecimal number.
   --
   function Integer_To_Hex (Nr    : in Integer;
                            Width : in Positive := 4) return String;

   --
   -- Coverts a string representaion of a hexadecimal number into an integer.
   --No prefix of postfix is allowed.
   --
   function Hex_To_Integer (Hex : in String) return Integer;

private
end RASCAL.Convert;