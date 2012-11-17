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

with RASCAL.OS;
with RASCAL.Memory;     use RASCAL.Memory;
with RASCAL.Utility;    use RASCAL.Utility;

with Interfaces.C;      use Interfaces.C;
with Kernel;            use Kernel;
with Reporter;

package body RASCAL.ToolboxNumberRange is

   Toolbox_ObjectMiscOp : constant Interfaces.C.unsigned :=16#44EC6#;
   
   --

   function Get_Bounds (Window    : in Object_ID;
                        Component : in Component_ID;
                        Bound     : in NumberRange_Bound_Type) return integer is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(2**NumberRange_Bound_Type'Pos(Bound));
      Register.R(1) := int(Window);
      Register.R(2) := 835;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxNumberRange.Get_Bounds: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return integer(Register.R(NumberRange_Bound_Type'Pos(Bound)));
   end Get_Bounds;

   --

   procedure Set_Bounds (Window   : in Object_ID;
                        Component : in Component_ID;
                        Bounds    : in NumberRange_Bound_Type;
                        New_Value : in integer) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(2**NumberRange_Bound_Type'Pos(Bounds));
      Register.R(1) := int(Window);
      Register.R(2) := 834;
      Register.R(3) := int(Component);
      Register.R(4) := int(New_Value);
      Register.R(5) := int(New_Value);
      Register.R(6) := int(New_Value);
      Register.R(7) := int(New_Value);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxNumberRange.Set_Bounds: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Bounds;

   --

   function Get_Components (Window    : in Object_ID;
                            Component : in Component_ID;
                            Part      : in NumberRange_Components_Type) return Icon_Handle_Type is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(2**NumberRange_Components_Type'Pos(Part));
      Register.R(1) := int(Window);
      Register.R(2) := 836;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxNumberRange.Get_Components: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Icon_Handle_Type(Register.R(NumberRange_Components_Type'Pos(Part)));
   end Get_Components;

   --

   function Get_Value (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return integer is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 833;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxNumberRange.Get_Value: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return integer(Register.R(0));
   end Get_Value;

   --

   procedure Set_Value (Window    : in Object_ID;
                        Component : in Component_ID;
                        New_Value : in integer;
                        Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 832;
      Register.R(3) := int(Component);
      Register.R(4) := int(New_Value);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxNumberRange.Set_Value: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Set_Value;

   --

end RASCAL.ToolboxNumberRange;
