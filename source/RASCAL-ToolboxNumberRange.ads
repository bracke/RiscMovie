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

-- @brief ToolboxNumberRange types and methods.
-- $Author$
-- $Date$
-- $Revision$

with System;                     use System;
with System.Unsigned_Types;      use System.Unsigned_Types;
with Interfaces.C;               use Interfaces.C;

with RASCAL.Toolbox;             use RASCAL.Toolbox;
with RASCAL.OS;                  use RASCAL.OS;

package RASCAL.ToolboxNumberRange is

   type NumberRange_Bound_Type is (Lower,Upper, Current_Size, Precision);

   type NumberRange_Components_Type is (Numeric,Left,Right,Slider);

   --
   -- This event is raised when the value of the number range has changed.
   --
   type Toolbox_NumberRange_ValueChanged is
   record
   Header     : Toolbox_Event_Header;
   New_Value  : Integer;
   end record;
   pragma Convention (C, Toolbox_NumberRange_ValueChanged);

   type Toolbox_NumberRange_ValueChanged_Pointer is access Toolbox_NumberRange_ValueChanged;

   type ATEL_Toolbox_NumberRange_ValueChanged is abstract new Toolbox_EventListener(Toolbox_Event_NumberRange_ValueChanged,-1,-1) with
   record
   Event : Toolbox_NumberRange_ValueChanged_Pointer;
   end record;


   --
   -- Returns the lower and upper bounds for the values in a number range.
   --
   function Get_Bounds (Window    : in Object_ID;
                        Component : in Component_ID;
                        Bound     : in NumberRange_Bound_Type) return integer;

   --
   -- Sets the lower and upper bounds for the values in a number range.
   --
   procedure Set_Bounds (Window    : in Object_ID;
                         Component : in Component_ID;
                         Bounds    : in NumberRange_Bound_Type;
                         New_Value : in integer);

   --
   -- Returns the componentids of the objects that make up the numberrange.
   --
   function Get_Components (Window    : in Object_ID;
                            Component : in Component_ID;
                            Part      : in NumberRange_Components_Type) return Icon_Handle_Type;

   --
   -- Returns the current value of the numberrange.
   --
   function Get_Value (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return integer;

   --
   -- Sets the current value of the numberrange.
   --
   procedure Set_Value (Window    : in Object_ID;
                        Component : in Component_ID;
                        New_Value : in integer;
                        Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_NumberRange_ValueChanged) is abstract;

end RASCAL.ToolboxNumberRange;
