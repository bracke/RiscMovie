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

-- @brief Toolbox StringSet related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with Interfaces.C;               use Interfaces.C;
with System;                     use System;
with System.Unsigned_Types;      use System.Unsigned_Types;

with RASCAL.Toolbox;             use RASCAL.Toolbox;
with RASCAL.OS;                  use RASCAL.OS;

package RASCAL.ToolboxStringSet is

   type StringSet_Components_Type is (Field,Menu);

   --
   -- This event is raised when the value of the StringSet has changed.
   --
   type Toolbox_StringSet_ValueChanged is
   record
   Header  : Toolbox_Event_Header;
   Message : Char_Array (1 .. 208);
   end record;
   pragma Convention (C, Toolbox_StringSet_ValueChanged);

   type Toolbox_StringSet_ValueChanged_Pointer is access Toolbox_StringSet_ValueChanged;

   type ATEL_Toolbox_StringSet_ValueChanged is abstract new Toolbox_EventListener(Toolbox_Event_StringSet_ValueChanged,-1,-1) with
   record
   Event : Toolbox_StringSet_ValueChanged_Pointer;
   end record;

   --
   -- This event is raised just before the stringset's menu is to be shown.
   --
   type Toolbox_StringSet_AboutToBeShown is
   record
   Header  : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_StringSet_AboutToBeShown);

   type Toolbox_StringSet_AboutToBeShown_Pointer is access Toolbox_StringSet_AboutToBeShown;

   type ATEL_Toolbox_StringSet_AboutToBeShown is abstract new Toolbox_EventListener(Toolbox_Event_StringSet_AboutToBeShown,-1,-1) with
   record
   Event : Toolbox_StringSet_AboutToBeShown_Pointer;
   end record;
   

   --
   -- Returs the ComponentIDs of the gadgets that make up the StringSet.
   --
   function Get_Components (Window    : in Object_ID;
                            Component : in Component_ID;
                            Part      : in StringSet_Components_Type) return Icon_Handle_Type;

   --
   -- Returns the name of the string currently in the stringset field.
   --
   function Get_Selected (Window    : in Object_ID;
                          Component : in Component_ID) return String;

   --
   -- Returns the index of the selected item.
   --
   function Get_Selected_Index (Window    : in Object_ID;
                                Component : in Component_ID) return integer;

   --
   -- Sets the list of allowable characters which can be typed into a writable field.
   --
   procedure Set_Allowable (Window    : in Object_ID;
                            Component : in Component_ID;
                            Allowable : in string;
                            Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the list of available strings in the stringset.
   --
   procedure Set_Available (Window    : in Object_ID;
                            Component : in Component_ID;
                            Available : in string;
                            Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets which string in the stringset is selected.
   --
   procedure Set_Selected (Window    : in Object_ID;
                           Component : in Component_ID;
                           Selected  : in string);

   --
   -- Sets which string in the stringset is selected.
   --
   procedure Set_Selected_Index (Window    : in Object_ID;
                                 Component : in Component_ID;
                                 Selected  : in integer);

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_StringSet_ValueChanged) is abstract;

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_StringSet_AboutToBeShown) is abstract;

end RASCAL.ToolboxStringSet;
