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

-- @brief Event definitions for handling messages from !ConfiX.
-- $Author$
-- $Date$
-- $Revision$

with RASCAL.OS;    use RASCAL.OS;

with Interfaces.C; use Interfaces.C;

package RASCAL.ConfiX is

   --
   -- Message send by ConfiX to application.
   --The message string contains textual commands (not case sensititve).
   --'GetCaret','Config','ConfigOK'.
   -- 
   type Message_ConfiX is
   record
   Header         : Message_Event_Header;
   Message        : Char_Array(1..236);
   end record;

   pragma Convention (C, Message_ConfiX);

   type Message_ConfiX_Pointer is access Message_ConfiX;

   type AMEL_Message_ConfiX is abstract new
        Message_EventListener(Message_Event_ConfiX) with
   record
   Event : Message_ConfiX_Pointer;
   end record;


private
end RASCAL.ConfiX;