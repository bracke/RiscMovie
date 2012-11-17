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

-- @brief Toolbox RadioButton related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with System;                     use System;
with System.Unsigned_Types;      use System.Unsigned_Types;
with Interfaces.C;               use Interfaces.C;

with RASCAL.Toolbox;             use RASCAL.Toolbox;
with RASCAL.OS;                  use RASCAL.OS;

package RASCAL.ToolboxRadioButton is

   type Button_State_Type is (UnSelected,Selected);
   --
   -- This event is raised when the state of the radio button changes.
   --
   type Toolbox_RadioButton_StateChanged is
   record
   Header        : Toolbox_Event_Header;
   State         : Integer;
   Old_On_Button : Component_ID;
   end record;
   pragma Convention (C, Toolbox_RadioButton_StateChanged);

   type Toolbox_RadioButton_StateChanged_Pointer is access Toolbox_RadioButton_StateChanged;

   type ATEL_Toolbox_RadioButton_StateChanged is abstract new Toolbox_EventListener(Toolbox_Event_RadioButton_StateChanged,-1,-1) with
   record
   Event : Toolbox_RadioButton_StateChanged_Pointer;
   end record;

   --
   -- Returns the event which is generated when the radio button state changes.
   --
   function Get_Event (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return ToolBox_Event_Code_Type;

   --
   -- Returns the label of the radio button.
   --
   function Get_Label (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return String;

   --
   -- Returns the state of the radio button.
   --
   function Get_State (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return Button_State_Type;

   --
   -- Sets the event which is generated when the radio button state changes.
   --
   procedure Set_Event (Window    : in Object_ID;
                        Component : in Component_ID;
                        Event     : in ToolBox_Event_Code_Type;
                        Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the font used in the radio button. Default is the system font.
   --
   procedure Set_Font (Window       : in Object_ID;
                       Component    : in Component_ID;
                       Font         : in String;
                       Font_Width   : in integer := 12;
                       Font_Height  : in integer := 12;
                       Flags        : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the label used by the radio button.
   --
   procedure Set_Label (Window    : in Object_ID;
                        Component : in Component_ID;
                        Label     : in String;
                        Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the state of the radio button.
   --
   procedure Set_State (Window    : in Object_ID;
                        Component : in Component_ID;
                        State     : in Button_State_Type;
                        Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_RadioButton_StateChanged) is abstract;

end RASCAL.ToolboxRadioButton;
