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

-- @brief Keyboard related methods
-- $Author$
-- $Date$
-- $Revision$

with RASCAL.WimpTask;    use RASCAL.WimpTask;

package RASCAL.Keyboard is

   --
   -- Is the SHIFT key pressed ?
   --
   function Is_Shift return Boolean;

   --
   -- Is the ALT key pressed ?
   --
   function Is_Alt return Boolean;

   --
   -- Is the CTRL key pressed ?
   --
   function Is_Control return Boolean;

   --
   -- Inserts keypresses into the WIMP buffer.
   --
   procedure Process_Key (Key : in Character := ASCII.CR);

   --
   -- Inserts string into the WIMP buffer as a series of keypresses.
   --
   procedure Process_String (Text   : in String;
                             Sender : in out Wimp_Task_Class'Class);

   --
   -- Returns the name of the key with the given inkey code.
   --
   function Get_KeyName (Code : in Positive) return String;

private
end RASCAL.Keyboard;