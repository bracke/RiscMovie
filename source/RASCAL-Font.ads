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

-- @brief Event definitions and subprograms for handling outline fonts.
-- $Author$
-- $Date$
-- $Revision$

with RASCAL.OS; use RASCAL.OS;

package RASCAL.Font is

   type Font_Handle_Type is new integer;

   --
   -- This message is bradcasted when the desktop font has changed.
   --
   type Message_FontChanged is
   record
   Header         : Message_Event_Header;
   end record;
   pragma Convention (C, Message_FontChanged);

   type Message_FontChanged_Pointer is access Message_FontChanged;

   type AMEL_Message_Fontchanged is abstract new
        Message_EventListener(Message_Event_FontChanged) with
   record
   Event : Message_FontChanged_Pointer;
   end record;

   --
   -- Converts the coordinates from millipoints to OS units.
   --
   procedure Millipoints_To_OS (X : in out Integer;
                                Y : in out Integer);

   --
   -- Converts the coordinates from OS units to millipoints.
   --
   procedure OS_To_Millipoints (X : in out Integer;
                                Y : in out Integer);

   --
   -- Calculates how wide a string would be under the given limits.
   --
   procedure Get_StringWidth (Text   : in String;
                              Width  : in out Integer;
                              Height : in out Integer;
                              Length : in out Integer);

   --
   -- Truncate the string to the given limits.
   --
   function Truncate (Text   : in String;
                      Width  : in Integer;
                      Height : in Integer;
                      Length : in Integer;
                      Right  : in boolean) return String;

   --
   -- This call can be used to determine which characters are present in a font, and which glyphs in the underlying font file characters map to.
   --This call works only by looking at encoding files -
   --it cannot guarantee that a given character is actually defined in a font file, but it can say which characters definitely aren't, by returning with R2 set to -1.
   --
   procedure Enumerate_Characters (Font : in     Integer;
                                   Char : in out Integer;
                                   Code :    out Integer);


end RASCAL.Font;
