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

-- @brief Mode types and methods.
-- $Author$
-- $Date$
-- $Revision$

with RASCAL.OS;   use RASCAL.OS;

package RASCAL.Mode is

   No_Mode_Variable : Exception;

   type Mode_Unit_Type is (Pixel,OSUnits);
   
   Current : constant Integer := -1;

   --
   -- Wimp_SetMode causes this message to be broadcasted. You should {*}not{*} acnowledge this message.
   --After sending this message, the Wimp sends an Open_Window_Request event for each window.
   --You must not delete any window.
   --If you want to delete a window, then just mark it to be deleted, and do the deed when you receive the Open window request.
   --
   type Message_ModeChange is
   record
   Header         : Message_Event_Header;
   end record;
   pragma Convention (C, Message_ModeChange);

   type Message_ModeChange_Pointer is access Message_ModeChange;

   type AMEL_Message_ModeChange is abstract new
        Message_EventListener(Message_Event_ModeChange) with
   record
   Event : Message_ModeChange_Pointer;
   end record;

   --
   -- This message is broadcasted by the palette utility when the RGB value for one or more physical colours have changed.
   --Message_ModeChange is broadcast {*}instead{*} of this message at a mode change.
   --In 256 colour modes, it is not the physical colour which changes, but only the mapping from logical to physical colour.
   --In that case the palette utility itself forces a redraw of the whole screen.
   --
   type Message_PaletteChange is
   record
   Header         : Message_Event_Header;
   end record;
   pragma Convention (C, Message_PaletteChange);

   type Message_PaletteChange_Pointer is access Message_PaletteChange;

   type AMEL_Message_PaletteChange is abstract new
        Message_EventListener(Message_Event_PaletteChange) with
   record
   Event : Message_PaletteChange_Pointer;
   end record;

   --
   -- Returns the horisontal resolution of the mode.
   --
   function Get_X_Resolution (Unit   : in Mode_Unit_Type := Pixel;
                              ModeNr : in Integer := Current) return Integer;

   --
   -- Returns the vertical resolution of the mode.
   --
   function Get_Y_Resolution (Unit   : in Mode_Unit_Type := Pixel;
                              ModeNr : in Integer := Current) return Integer;

   --
   -- Returns horisontal eigen factor of the mode.
   --
   function Get_X_Eig_Factor (ModeNr : in Integer := Current) return Integer;

   --
   -- Returns vertical eigen factor of the mode.
   --
   function Get_Y_Eig_Factor (ModeNr : in Integer := Current) return Integer;

   procedure Handle (The : in AMEL_Message_ModeChange)    is abstract;
   procedure Handle (The : in AMEL_Message_PaletteChange) is abstract;

end RASCAL.Mode;
