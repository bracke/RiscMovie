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
with RASCAL.Utility;    use RASCAL.Utility;
with RASCAL.Memory;     use RASCAL.Memory;

with Interfaces.C;      use Interfaces.C;
with Kernel;            use Kernel;
with Reporter;

package body RASCAL.ToolboxSlider is

   Toolbox_ObjectMiscOp : constant Interfaces.C.unsigned :=16#44EC6#;

   --
   
   function Get_Bounds (Window    : in Object_ID;
                        Component : in Component_ID;
                        Bound     : in Slider_Bound_Type) return integer is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Result         : integer := 2**(Slider_Bound_Type'Pos(Bound));
   begin
      Register.R(0) := int(Result);
      Register.R(1) := int(Window);
      Register.R(2) := 579;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSlider.Get_Bounds: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return integer(Register.R(Slider_Bound_Type'Pos(Bound)));
   end Get_Bounds;

   --

   procedure Set_Bounds (Window    : in Object_ID;
                         Component : in Component_ID;
                         Bound     : in Slider_Bound_Type;
                         New_Value : in integer) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Result         : integer := 2**(Slider_Bound_Type'Pos(Bound));
   begin
      Register.R(0) := int(Result);
      Register.R(1) := int(Window);
      Register.R(2) := 578;
      Register.R(3) := int(Component);
      Register.R(4) := int(New_Value);
      Register.R(5) := int(New_Value);
      Register.R(6) := int(New_Value);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSlider.Set_Bounds: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Bounds;

   --

   procedure Get_Colour (Window    : in Object_ID;
                         Component : in Component_ID;
                         Bar       : out integer;
                         Background: out integer;
                         Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 581;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSlider.Get_Colour: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Bar := integer(Register.R(0));
      Background := integer(Register.R(1));
   end Get_Colour;

   --

   function Get_Value (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return integer is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 577;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSlider.Get_Value: " & To_Ada(Error.ErrMess)));
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
      Register.R(2) := 576;
      Register.R(3) := int(Component);
      Register.R(4) := int(New_Value);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSlider.Set_Value: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Value;

   --

   procedure Set_Colour (Window   : in Object_ID;
                        Component : in Component_ID;
                        Bar       : in integer;
                        Background: in integer;
                        Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 580;
      Register.R(3) := int(Component);
      Register.R(4) := int(Bar);
      Register.R(5) := int(Background);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSlider.Set_Colour: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Set_Colour;

   --
   
end RASCAL.ToolboxSlider;
