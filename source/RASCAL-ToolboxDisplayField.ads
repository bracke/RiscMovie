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

-- @brief Toolbox DisplayField related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with RASCAL.Toolbox;        use RASCAL.Toolbox;
with RASCAL.OS;             use RASCAL.OS;
with RASCAL.WimpIcon;       use RASCAL.WimpIcon;

with System.Unsigned_Types; use System.Unsigned_Types;

package RASCAL.ToolboxDisplayField is

   --
   -- How is the text in the displayfield aligned ?
   --
   function Get_Alignment (Window    : in Object_ID;
                           Component : in Component_ID) return Alignment_Type;

   --
   -- Returns the text string shown in the display field.
   --
   function Get_Value (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return String;

   --
   -- Sets the font used in the display field. Default is the system font.
   --
   procedure Set_Font (Window       : in Object_ID;
                       Component    : in Component_ID;
                       Font         : in string;
                       Font_Width   : in integer;
                       Font_Height  : in integer;
                       Flags        : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the text string shown in the display field. The screen display is immediately updated if the window is on screen.
   --
   procedure Set_Value (Window    : in Object_ID;
                        Component : in Component_ID;
                        New_Value : in String;
                        Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the text string shown in the display field. The screen display is immediately updated if the window is on screen.
   --The text is truncated to the size of the displayfield and an ellipsis is added.
   --
   procedure Set_TruncatedValue (Window    : in Object_ID;
                                 Component : in Component_ID;
                                 New_Value : in String;
                                 Flags     : in System.Unsigned_Types.Unsigned := 0);
end RASCAL.ToolboxDisplayField;
