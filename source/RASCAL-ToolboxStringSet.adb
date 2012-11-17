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

package body RASCAL.ToolboxStringSet is

   Toolbox_ObjectMiscOp : constant Interfaces.C.unsigned :=16#44EC6#;

   --

   function Get_Components (Window    : in Object_ID;
                            Component : in Component_ID;
                            Part      : in StringSet_Components_Type) return Icon_Handle_Type is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(2**StringSet_Components_Type'Pos(Part));
      Register.R(1) := int(Window);
      Register.R(2) := 16#386#;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxStringSet.Get_Components: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Icon_Handle_Type(Register.R(StringSet_Components_Type'Pos(Part)));
   end Get_Components;

   --

   function Get_Selected (Window    : in Object_ID;
                          Component : in Component_ID) return String is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Buffer_Size    : integer := 0;
   begin
      Register.R(0) := 0;
      Register.R(1) := int(Window);
      Register.R(2) := 899;
      Register.R(3) := int(Component);
      Register.R(4) := 0;
      Register.R(5) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxStringSet.Get_Selected-1: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Buffer_Size := integer(Register.R(5));
      declare
         Buffer : String(1..Buffer_Size + 1);
      begin
         Register.R(0) := 0;
         Register.R(1) := int(Window);
         Register.R(2) := 899;
         Register.R(3) := int(Component);
         Register.R(4) := Adr_To_Int(Buffer'Address);
         Register.R(5) := int(Buffer_Size + 1);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxStringSet.Get_Selected-2: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_Selected;

   --

   function Get_Selected_Index (Window    : in Object_ID;
                                Component : in Component_ID) return integer is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := 1;
      Register.R(1) := int(Window);
      Register.R(2) := 899;
      Register.R(3) := int(Component);
      Register.R(4) := 0;
      Register.R(5) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxStringSet.Get_Selected: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return integer(Register.R(0));
   end Get_Selected_Index;

   --
   procedure Set_Allowable (Window    : in Object_ID;
                            Component : in Component_ID;
                            Allowable : in string;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Null_Allowable : string := Allowable & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 16#384#;
      Register.R(3) := int(Component);
      Register.R(4) := Adr_To_Int(Null_Allowable'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxStringSet.Set_Allowable: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Set_Allowable;

   --

   procedure Set_Available (Window    : in Object_ID;
                            Component : in Component_ID;
                            Available : in string;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Null_Available : string := Available & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 16#380#;
      Register.R(3) := int(Component);
      Register.R(4) := Adr_To_Int(Null_Available'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxStringSet.Set_Available: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Available;

   --

   procedure Set_Selected (Window    : in Object_ID;
                           Component : in Component_ID;
                           Selected  : in string) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Null_Selected : string := Selected & ASCII.NUL;
   begin
      Register.R(0) := 0;
      Register.R(1) := int(Window);
      Register.R(2) := 898;
      Register.R(3) := int(Component);
      Register.R(4) := Adr_To_Int(Null_Selected'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxStringSet.Set_Selected: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Selected;

   --

   procedure Set_Selected_Index (Window    : in Object_ID;
                                 Component : in Component_ID;
                                 Selected  : in integer) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := 1;
      Register.R(1) := int(Window);
      Register.R(2) := 898;
      Register.R(3) := int(Component);
      Register.R(4) := int(Selected);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxStringSet.Set_Selected: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);         
      end if;
   end Set_Selected_Index;

   --

end RASCAL.ToolboxStringSet;
