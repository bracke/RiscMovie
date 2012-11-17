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

with Kernel;            use Kernel;
with Interfaces.C;      use Interfaces.C;
with Ada.Strings.Unbounded;
with Reporter;

with RASCAL.Memory;            use RASCAL.Memory;
with RASCAL.Utility;           use RASCAL.Utility;
with RASCAL.ToolboxWindow;     use RASCAL.ToolboxWindow;
with RASCAL.OS;

package body RASCAL.ToolboxRadioButton is

   Toolbox_ObjectMiscOp : constant Interfaces.C.unsigned :=16#44EC6#;
   
   --

   function Get_Event (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return ToolBox_Event_Code_Type is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 387;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxRadioButton.Get_Event: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return ToolBox_Event_Code_Type(Register.R(0));
   end Get_Event;

   --

   function Get_Label (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return String is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Buffer_Size    : integer := 0;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 385;
      Register.R(3) := int(Component);
      Register.R(4) := 0;
      Register.R(5) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxRadioButton.Get_Label: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := integer(Register.R(5));

      declare
         Buffer : String(1..Buffer_Size + 1);
      begin
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(Window);
         Register.R(2) := 385;
         Register.R(3) := int(Component);
         Register.R(4) := Adr_To_Int(Buffer'Address);
         Register.R(5) := int(Buffer_Size + 1);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxRadioButton.Get_Label: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_Label;

   --

   function Get_State (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return Button_State_Type is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 389;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxRadioButton.Get_State: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Button_State_Type'Val(Register.R(0));
   end Get_State;
   
   --

   procedure Set_Event (Window    : in Object_ID;
                        Component : in Component_ID;
                        Event     : in ToolBox_Event_Code_Type;
                        Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 386;
      Register.R(3) := int(Component);
      Register.R(4) := int(Event);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxRadioButton.Set_Event: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Event;

   --

   procedure Set_Font (Window       : in Object_ID;
                       Component    : in Component_ID;
                       Font         : in String;
                       Font_Width   : in integer := 12;
                       Font_Height  : in integer := 12;
                       Flags        : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Font_0         : String := Font & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 390;
      Register.R(3) := int(Component);
      Register.R(4) := Adr_To_Int(Font_0'Address);
      Register.R(5) := int(Font_Width);
      Register.R(6) := int(Font_Height);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxRadioButton.Set_Font: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Font;

   --

   procedure Set_Label (Window    : in Object_ID;
                        Component : in Component_ID;
                        Label     : in String;
                        Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register    : aliased Kernel.swi_regs;
      Error       : oserror_access;
      Label_0     : UString := U(Label & ASCII.NUL);
      Buffer_Size : Integer := (Gadget_Get_BufferSize(Window,Component))-1;
   begin
      if Buffer_Size > -1 then
         if Label'Length > Buffer_Size then
            Label_0 := Ada.Strings.Unbounded.Head(Label_0,Buffer_Size);
            Ada.Strings.Unbounded.Append(Label_0,ASCII.NUL);
         end if;
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(Window);
         Register.R(2) := 384;
         Register.R(3) := int(Component);
         Register.R(4) := Adr_To_Int(S(Label_0)'Address);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);
         
         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxRadioButton.Set_Label: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
      end if;
   end Set_Label;

   --

   procedure Set_State (Window    : in Object_ID;
                        Component : in Component_ID;
                        State     : in Button_State_Type;
                        Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 388;
      Register.R(3) := int(Component);
      Register.R(4) := Button_State_Type'Pos(State);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma debug(Reporter.Report("ToolboxRadioButton.Set_State: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_State;

   --

end RASCAL.ToolboxRadioButton;
