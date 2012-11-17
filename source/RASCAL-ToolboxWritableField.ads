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

-- @brief Toolbox WritableField related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with Interfaces.C;               use Interfaces.C;
with System;                     use System;
with System.Unsigned_Types;      use System.Unsigned_Types;

with RASCAL.Toolbox;             use RASCAL.Toolbox;
with RASCAL.OS;                  use RASCAL.OS;

package RASCAL.ToolboxWritableField is

   --
   -- This event is raised when the value of a writable field has been changed by the user.
   --
   type Toolbox_WritableField_ValueChanged is
   record
   Header  : Toolbox_Event_Header;
   Content : Char_Array (1..208);
   end record;
   pragma Convention (C, Toolbox_WritableField_ValueChanged);

   type Toolbox_WritableField_ValueChanged_Pointer is access Toolbox_WritableField_ValueChanged;

   type ATEL_Toolbox_WritableField_ValueChanged is abstract new Toolbox_EventListener(Toolbox_Event_WritableField_ValueChanged,-1,-1) with
   record
   Event : Toolbox_WritableField_ValueChanged_Pointer;
   end record;
   
   --
   -- Returns the value (content) of the writable field.
   --
   function Get_Value (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return String;

   --
   -- Sets the list of allowed characters for the writable field.
   --
   procedure Set_Allowable (Window    : in Object_ID;
                            Component : in Component_ID;
                            Allowable : in string;
                            Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the font to be used in the writable field. Default is the system font.
   --
   procedure Set_Font (Window       : in Object_ID;
                       Component    : in Component_ID;
                       Font         : in String;
                       Font_Width   : in integer := 12;
                       Font_Height  : in integer := 12;
                       Flags        : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the value (content) of the writable field.
   --
   procedure Set_Value (Window    : in Object_ID;
                        Component : in Component_ID;
                        New_Value : in string;
                        Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_WritableField_ValueChanged) is abstract;

end RASCAL.ToolboxWritableField;
