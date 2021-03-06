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

-- @brief Hourglass related subprograms.
-- $Author$
-- $Date$
-- $Revision$

package RASCAL.Hourglass is

   --
   --Switch Hourglass on.
   --
   procedure On;

   --
   --Switch Houglass off.
   --
   procedure Off;

   --
   --Schedules the hourglass to appear after the given delay.
   --A delay of 0 wil suppress the hourglass until 'Off' is called.
   --
   procedure Start (Centi : in Natural);

private
end RASCAL.Hourglass;
