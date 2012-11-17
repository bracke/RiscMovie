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

-- $Author$
-- $Date$
-- $Revision$
-- @brief Wimp icon related types and methods.

with Interfaces.C;              use Interfaces.C;
with Kernel;                    use Kernel;
with System;                    use System;

with RASCAL.OS;                 use RASCAL.OS;
with RASCAL.Memory;             use RASCAL.Memory;

package RASCAL.WimpIcon is

   type spriteareatyp is new Integer;
   
   Wimp_SetCaretPosition : constant Interfaces.C.unsigned := 16#400D2#;
   Wimp_GetCaretPosition : constant Interfaces.C.unsigned := 16#400D3#;
   Wimp_CreateIcon       : constant Interfaces.C.unsigned := 16#400C2#;
   Wimp_PlotIcon         : constant Interfaces.C.unsigned := 16#400E2#;
   Wimp_ResizeIcon       : constant Interfaces.C.unsigned := 16#400FC#;
   Wimp_SetIconState     : constant Interfaces.C.unsigned := 16#400CD#;
   Wimp_GetIconState     : constant Interfaces.C.unsigned := 16#400CE#;
   Wimp_WhichIcon        : constant Interfaces.C.unsigned := 16#400D6#;
   Wimp_DeleteIcon       : constant Interfaces.C.unsigned := 16#400C4#;
   
   f_IconHasText            : constant integer := 16#0#;
   m_IconHasText            : constant integer := 16#1#;
   
   f_IconIsSprite           : constant integer := 16#1#;
   m_IconIsSprite           : constant integer := 16#2#;
   
   f_IconHasBorder          : constant integer := 16#2#;
   m_IconHasBorder          : constant integer := 16#4#;
   
   f_IconHorizontalCentred  : constant integer := 16#3#;
   m_IconHorizontalCentred  : constant integer := 16#8#;
   
   f_IconVerticalCentred    : constant integer := 16#4#;
   m_IconVerticalCentred    : constant integer := 16#10#;
   
   f_IconFilled             : constant integer := 16#5#;
   m_IconFilled             : constant integer := 16#20#;
   
   f_IconAntiAliased        : constant integer := 16#6#;
   m_IconAntiAliased        : constant integer := 16#40#;
   
   f_IconNeedsHelp          : constant integer := 16#7#;
   m_IconNeedsHelp          : constant integer := 16#80#;
   
   f_IconIndirected         : constant integer := 16#8#;
   m_IconIndirected         : constant integer := 16#100#;
   
   f_IconRightJustified     : constant integer := 16#9#;
   m_IconRightJustified     : constant integer := 16#200#;
   
   f_IconAdjust             : constant integer := 16#a#;
   m_IconAdjust             : constant integer := 16#400#;
   
   f_IconHalfSprite         : constant integer := 16#b#;
   m_IconHalfSprite         : constant integer := 16#800#;
   
   f_IconSelected           : constant integer := 16#15#;
   m_IconSelected           : constant integer := 16#200000#;
   
   f_IconShaded             : constant integer := 16#16#;
   m_IconShaded             : constant integer := 16#400000#;
   
   f_IconDeleted            : constant integer := 16#17#;
   m_IconDeleted            : constant integer := 16#800000#;
   
   texticonssize        : constant integer := 2;
   
   type iconhandletyp is new Integer;
   type indirecteddataptr is new Integer;
   
   type iconarray_type is array (0..63) of Integer;
   type texticonbuffertyp is array (0..texticonssize-1) of Character;
   
   texticonbuffer      : texticonbuffertyp;
   texticonbufferoffset: Integer := 0; -- derzeitiger Offset zum freien Element

   type Alignment_Type is (Left,Centre,Right);
   
   type Wimp_IconState_Type is
   record
   Window  : Wimp_Handle_Type;
   Icon    : Icon_Handle_Type;
   Min_X   : Integer;
   Min_Y   : Integer;
   Max_X   : Integer;
   Max_Y   : Integer;
   Flags   : Integer;
   Data    : Char_Array(1..12);
   end record;

   type Wimp_CreateIcon_Type is
   record
   Window  : Wimp_Handle_Type;
   Min_X   : Integer := 0;
   Min_Y   : Integer := 0;
   Max_X   : Integer := 0;
   Max_Y   : Integer := 0;
   Flags   : Integer := 0;
   Data    : Char_Array(1..12);

   end record;

   type Wimp_SetIconState_Tyoe is
   record
   Window     : Wimp_Handle_Type;
   Icon       : Icon_Handle_Type;
   EOR_Word   : Integer;
   Clear_Word : Integer;
   end record;

   --
   -- Returns the alignment of the text in the icons.
   --
   function Get_Alignment (Window : in Wimp_Handle_Type;
                           Icon   : in Icon_Handle_Type) return Alignment_Type;

   --
   -- Returns the validation string of the icon.
   --
   function Get_Validation (Window : in Wimp_Handle_Type;
                            Icon   : in Icon_Handle_Type) return String;

   --
   -- Sets the validation string of the icon.
   --
   procedure Set_Validation (Window     : in Wimp_Handle_Type;
                             Icon       : in Icon_Handle_Type;
                             Validation : in String;
                             Redraw     : in Boolean := true);

   --
   -- Returns the icon's text value.
   --
   function Get_Text (Window : in Wimp_Handle_Type;
                      Icon   : in Icon_Handle_Type) return String;

   --
   -- Sets the icon's text value.
   --
   procedure Set_Text (Window : in Wimp_Handle_Type;
                       Icon   : in Icon_Handle_Type;
                       Text   : in String;
                       Redraw : in Boolean := true);

   --
   -- Set the icon's flags.
   --
   procedure Set_State (Window : in Wimp_Handle_Type;
                        Icon   : in Icon_Handle_Type;
                        Flags  : in Integer;
                        Mask   : in Integer);

   --
   -- Changes the icon's button type.
   --
   procedure Change_ButtonType (Window : in Wimp_Handle_Type;
                                Icon   : in Icon_Handle_Type;
                                Button : in Integer);

   --
   -- Forces the icon to be redrawn.
   --
   procedure Redraw_Icon (Window : in Wimp_Handle_Type;
                          Icon   : in Icon_Handle_Type);

   --
   -- Select icon.
   --
   procedure Select_Icon (Window : in Wimp_Handle_Type;
                          Icon   : in Icon_Handle_Type);

   --
   -- Unselect icon.
   --
   procedure UnSelect (Window : in Wimp_Handle_Type;
                       Icon   : in Icon_Handle_Type);

   --
   -- Returns true if icon is selectable.
   --
   function Is_Selectable (Window : in Wimp_Handle_Type;
                           Icon   : in Icon_Handle_Type) return Boolean;

   --
   -- Make icon selectable.
   --
   procedure Make_Selectable (Window : in Wimp_Handle_Type;
                              Icon   : in Icon_Handle_Type);

   --
   -- Make icon unselectable.
   --
   procedure Make_Unselectable (Window : in Wimp_Handle_Type;
                                Icon   : in Icon_Handle_Type);

   --
   -- Return true if icon is selected.
   --
   function Is_Selected (Window : in Wimp_Handle_Type;
                         Icon   : in Icon_Handle_Type) return Boolean;

   --
   -- Deletes the icon.
   --
   procedure Delete_Icon (Window : in Wimp_Handle_Type;
                          Icon   : in Icon_Handle_Type);


   --
   -- Creates a new button.
   --
   function Create_Button (Window : in Wimp_Handle_Type;
                           Min_X  : in Integer;
                           Min_Y  : in Integer;
                           Max_X  : in Integer;
                           Max_Y  : in Integer;
                           Text   : in String;
                           Adresse: in Address) return Icon_Handle_Type;

   --
   -- Create text icon
   --
   function Create_TextIcon (Window       : in Wimp_Handle_Type;
                             Min_X        : in Integer;
                             Min_Y        : in Integer;
                             Max_X        : in Integer;
                             Max_Y        : in Integer;
                             Flags        : in Integer;
                             Text         : in String;
                             bufferlength : in Integer;
                             Adresse      : in Address) return Icon_Handle_Type;

   --
   -- Create text icon.
   --
   function Create_TextIcon (Window       : in Wimp_Handle_Type;
                             Min_X        : in Integer;
                             Min_Y        : in Integer;
                             Max_X        : in Integer;
                             Max_Y        : in Integer;
                             Flags        : in Integer;
                             Text         : in String;
                             bufferlength : in Integer := 0) return Icon_Handle_Type;

   --
   -- Create frame icon.
   --
   procedure Create_FrameIcon (Window : in Wimp_Handle_Type;
                               Min_X  : in Integer;
                               Min_Y  : in Integer;
                               Max_X  : in Integer;
                               Max_Y  : in Integer;
                               Titel  : in String);


   --
   -- Return foreground colour of icon.
   --
   function Get_Foreground (Window : in Wimp_Handle_Type;
                            Icon   : in Icon_Handle_Type) return Wimp_Colour;

   --
   -- Return background colour of icon.
   --
   function Get_Background (Window : in Wimp_Handle_Type;
                            Icon   : in Icon_Handle_Type) return Wimp_Colour;

   --
   -- Set the icons background colour.
   --
   procedure Set_Background (Window : in Wimp_Handle_Type;
                             Icon   : in Icon_Handle_Type;
                             Colour : in Wimp_Colour);

   --
   -- Set the icons foreground colour.
   --
   procedure Set_Foreground (Window : in Wimp_Handle_Type;
                             Icon   : in Icon_Handle_Type;
                             Colour : in Wimp_Colour);


private
end RASCAL.WimpIcon;