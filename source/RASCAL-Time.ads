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

-- @brief Time related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with RASCAL.OS;             use RASCAL.OS;
with System.Unsigned_Types; use System.Unsigned_Types;

package RASCAL.Time is

   type UTC_Time_Type is
   record
   Word      : Unsigned;
   Last_Byte : Byte;
   end record;

   type UTC_Pointer is access UTC_Time_Type;

   pragma Convention (C, UTC_Time_Type);

   --
   -- Returns the time either with or without centiseconds.
   -- {fcode}Hour:Minutes:Seconds:Centiseconds{f}
   --
   function Get_Time (Centi : in boolean := false) return string;

   --
   -- Returns the date in a format specified by 'Format'.
   --
   function Get_Date (Format : in string := "%ce%yr-%mn-%dy") return string;

   --
   -- Returns monotonic time : Number of centiceconds since last hard reset.
   --
   function Read_MonotonicTime return Integer;

end RASCAL.Time;
