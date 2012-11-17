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

with RASCAL.Utility;       use RASCAL.Utility;
with RASCAL.Memory;        use RASCAL.Memory;
with RASCAL.Toolbox;       use RASCAL.Toolbox;
with RASCAL.ToolboxWindow; use RASCAL.ToolboxWindow;
with RASCAL.Font;          use RASCAL.Font;

with Kernel;               use Kernel;
with Interfaces.C;         use Interfaces.C;
with Ada.Strings.Unbounded;
with Reporter;

package body RASCAL.ToolboxDisplayField is

   Toolbox_ObjectMiscOp : constant Interfaces.C.unsigned :=16#44EC6#;
   
   --

   function Get_Value (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return String is

      Register                : aliased Kernel.swi_regs;
      Buffer_Size             : integer := 0;
      Error                   : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 449;
      Register.R(3) := int(Component);
      Register.R(4) := 0;
      Register.R(5) := 0;

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxDisplayField.Get_Value: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := Integer(Register.R(5));

      declare
         Buffer : String(1..Buffer_Size);
      begin
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(Window);
         Register.R(2) := 449;
         Register.R(3) := int(Component);
         Register.R(4) := Adr_To_Int(Buffer'Address);
         Register.R(5) := int(Buffer_Size);

         Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxDisplayField.Get_Value: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_Value;

   --

   procedure Set_Font (Window       : in Object_ID;
                       Component    : in Component_ID;
                       Font         : in string;
                       Font_Width   : in integer;
                       Font_Height  : in integer;
                       Flags        : in System.Unsigned_Types.Unsigned := 0) is

      Register   : aliased Kernel.swi_regs;
      Error      : oserror_access;
      Font_0     : String := Font & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 450;
      Register.R(3) := int(Component);
      Register.R(4) := Adr_To_Int(Font_0'Address);
      Register.R(5) := int(Font_Width);
      Register.R(5) := int(Font_Height);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxDisplayField.Set_Font: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Font;

   --

   procedure Set_Value (Window    : in Object_ID;
                        Component : in Component_ID;
                        New_Value : in String;
                        Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register    : aliased Kernel.swi_regs;
      Error       : oserror_access;
      Value_0     : UString := U(New_Value & ASCII.NUL);
      Buffer_Size : Integer := (Gadget_Get_BufferSize(Window,Component))-1;
   begin
      if Buffer_Size > -1 then
         if New_Value'Length > Buffer_Size then
            Value_0 := Ada.Strings.Unbounded.Head(Value_0,Buffer_Size);
            Ada.Strings.Unbounded.Append(Value_0,ASCII.NUL);
         end if;

         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(Window);
         Register.R(2) := 448;
         Register.R(3) := int(Component);
         Register.R(4) := Adr_To_Int(S(Value_0)'Address);
          
         Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);
         
         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxDisplayField.Set_Value: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
      end if;
   end Set_Value;

   --

   function Get_Alignment (Window    : in Object_ID;
                           Component : in Component_ID) return Alignment_Type is

      W      : Wimp_Handle_Type := ToolboxWindow.Get_Wimp_Handle(Window);
      Icons  : Icon_List_Type   := ToolboxWindow.Get_Icon_List(Window,Component);
      Icon   : Icon_Handle_Type := Icons(Icons'First);
   begin
      return WimpIcon.Get_Alignment(W,Icon);
   end;                        

   --

   procedure Set_TruncatedValue (Window    : in Object_ID;
                                 Component : in Component_ID;
                                 New_Value : in String;
                                 Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Buffer_Size : Integer := (Gadget_Get_BufferSize(Window,Component))-1;
      Right       : Boolean           := Get_Alignment(Window,Component) = Left;
      BBox        : Toolbox_BBox_Type := ToolboxWindow.Gadget_GetBBox (Window,Component);
      Truncated   : String := Font.Truncate(New_Value,BBox.xmax-BBox.xmin,BBox.ymax-BBox.ymin,Buffer_Size,Right);
   begin
      Set_Value (Window,Component,Truncated,Flags);
   end Set_TruncatedValue;

   --

end RASCAL.ToolboxDisplayField;
