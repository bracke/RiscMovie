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

-- @brief Pointer related methods
-- $Author$
-- $Date$
-- $Revision$

with RASCAL.OS; use RASCAL.OS;

package RASCAL.Pointer is

   --
   -- Returns the state of the mouse when the last Wimp_Poll was executed.
   --
   procedure Get_PointerInfo (X_Pos  : out integer;
                              Y_Pos  : out integer;
                              Button : out integer;
                              Window : out Wimp_Handle_Type;
                              Icon   : out Icon_Handle_Type);

   --
   -- Is the SELECT button pressed ?
   --
   function Is_Select return Boolean;

   --
   -- Is the ADJUST button pressed ?
   --
   function Is_Adjust return Boolean;

   --
   -- Is the MENU button pressed ?
   --
   function Is_Menu return Boolean;

   --
   -- Returns the Double click delay in centiseconds.
   --
   function Get_DoubleClick_Delay return Integer;

private
end RASCAL.Pointer;