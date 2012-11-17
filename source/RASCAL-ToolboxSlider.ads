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

-- @brief Toolbox Slider related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with Interfaces.C;               use Interfaces.C;
with System;                     use System;
with System.Unsigned_Types;      use System.Unsigned_Types;

with RASCAL.Toolbox;             use RASCAL.Toolbox;
with RASCAL.OS;                  use RASCAL.OS;

package RASCAL.ToolboxSlider is

   type Slider_Bound_Type is (Lower,Upper,Step_Size);

   type Slider_Components_Type is (Numeric,Left,Right,Slider);

   --
   -- This event is raised when the value of the slider is changed.
   --
   type Toolbox_Slider_ValueChanged is
   record
   Header     : Toolbox_Event_Header;
   New_Value  : Integer;
   end record;
   pragma Convention (C, Toolbox_Slider_ValueChanged);

   type Toolbox_Slider_ValueChanged_Pointer is access Toolbox_Slider_ValueChanged;

   type ATEL_Toolbox_Slider_ValueChanged is abstract new Toolbox_EventListener(Toolbox_Event_Slider_ValueChanged,-1,-1) with
   record
   Event : Toolbox_Slider_ValueChanged_Pointer;
   end record;

   --
   -- Returns the bounds and step of the slider.
   --
   function Get_Bounds (Window    : in Object_ID;
                        Component : in Component_ID;
                        Bound     : in Slider_Bound_Type) return integer;

   --
   -- Set the bounds and step of the slider.
   --
   procedure Set_Bounds (Window    : in Object_ID;
                         Component : in Component_ID;
                         Bound     : in Slider_Bound_Type;
                         New_Value : in integer);

   --
   -- Returns the colour of the bar and background of the slider.
   --
   procedure Get_Colour (Window    : in Object_ID;
                         Component : in Component_ID;
                         Bar       : out integer;
                         Background: out integer;
                         Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the value of the slider.
   --
   function Get_Value (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return integer;

   --
   -- Sets the value of the slider.
   --
   procedure Set_Value (Window    : in Object_ID;
                        Component : in Component_ID;
                        New_Value : in integer;
                        Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the colour of the bar and background of the slider.
   --
   procedure Set_Colour (Window    : in Object_ID;
                        Component : in Component_ID;
                        Bar       : in integer;
                        Background: in integer;
                        Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_Slider_ValueChanged) is abstract;

end RASCAL.ToolboxSlider;
