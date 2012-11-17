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

package body RASCAL.ToolboxTextArea is

   Toolbox_ObjectMiscOp : constant Interfaces.C.unsigned :=16#44EC6#;

   --
      
   function Get_State (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return integer is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 16408;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxTextArea.Get_State: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return integer(Register.R(0));
   end Get_State;

   --

   procedure Set_State (Window    : in Object_ID;
                         Component : in Component_ID;
                         State     : in integer;
                         Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 16409;
      Register.R(3) := int(Component);
      Register.R(4) := int(State);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxTextArea.Set_State: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_State;

   --

   procedure Set_Text (Window    : in Object_ID;
                       Component : in Component_ID;
                       Text      : in string;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register  : aliased Kernel.swi_regs;
      Error     : oserror_access;
      Text_0    : string := Text & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 16410;
      Register.R(3) := int(Component);
      Register.R(4) := Adr_To_Int(Text_0'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxTextArea.Set_Text: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Text;

   --

   function Get_Text (Window    : in Object_ID;
                      Component : in Component_ID) return String is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Buffer_Size    : integer := 0;
   begin
      Register.R(0) := 0;
      Register.R(1) := int(Window);
      Register.R(2) := 16411;
      Register.R(3) := int(Component);
      Register.R(4) := 0;
      Register.R(5) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxTextArea.Get_Text: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Buffer_Size := integer(Register.R(5));
      declare
         Buffer : Char_Array(1..size_t(Buffer_Size + 1));
      begin
         Register.R(0) := 0;
         Register.R(1) := int(Window);
         Register.R(2) := 16411;
         Register.R(3) := int(Component);
         Register.R(4) := Adr_To_Int(Buffer'Address);
         Register.R(5) := int(Buffer_Size);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxTextArea.Get_Text: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return To_Ada(Buffer);
      end;
   end Get_Text;

   --

   procedure Insert_Text (Window    : in Object_ID;
                          Component : in Component_ID;
                          Text      : in string;
                          Index     : in integer := 0;
                          Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register  : aliased Kernel.swi_regs;
      Error     : oserror_access;
      Null_Text : string := Text & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 16412;
      Register.R(3) := int(Component);
      Register.R(4) := int(Index);
      Register.R(5) := Adr_To_Int(Null_Text'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxTextArea.Insert_Text: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Insert_Text;

   --

   procedure Replace_Text (Window       : in Object_ID;
                           Component    : in Component_ID;
                           Text         : in string;
                           End_Index    : in integer := 0;
                           Start_Index  : in integer := 0;
                           Flags        : in System.Unsigned_Types.Unsigned := 0) is

      Register  : aliased Kernel.swi_regs;
      Error     : oserror_access;
      Null_Text : string := Text & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 16413;
      Register.R(3) := int(Component);
      Register.R(4) := int(Start_Index);
      Register.R(5) := int(End_Index);
      Register.R(6) := Adr_To_Int(Null_Text'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxTextArea.Replace_Text: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Replace_Text;

   --

   function Get_Selection (Window    : in Object_ID;
                          Component : in Component_ID) return String is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Buffer_Size    : integer := 0;
   begin
      Register.R(0) := 1;
      Register.R(1) := int(Window);
      Register.R(2) := 16414;
      Register.R(3) := int(Component);
      Register.R(4) := 0;
      Register.R(5) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxTextArea.Get_Selection: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Buffer_Size := integer(Register.R(5));
      declare
         Buffer : String(1..Buffer_Size + 1);
      begin
         Register.R(0) := 1;
         Register.R(1) := int(Window);
         Register.R(2) := 16414;
         Register.R(3) := int(Component);
         Register.R(4) := Adr_To_Int(Buffer'Address);
         Register.R(5) := int(Buffer_Size);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxTextArea.Get_Selection: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_Selection;

   --

   procedure Get_Selection_Index (Window     : in Object_ID;
                                  Component  : in Component_ID;
                                  Start_Index: out integer;
                                  End_Index  : out integer) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := 0;
      Register.R(1) := int(Window);
      Register.R(2) := 16414;
      Register.R(3) := int(Component);
      Register.R(4) := 0;
      Register.R(5) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxTextArea.Get_Selection: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Start_Index := integer(Register.R(0));
      End_Index   := integer(Register.R(1));
   end Get_Selection_Index;

   --

   procedure Set_Selection (Window      : in Object_ID;
                        Component   : in Component_ID;
                        End_Index   : in integer;
                        Start_Index : in integer :=0;                        
                        Flags       : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 16415;
      Register.R(3) := int(Component);
      Register.R(4) := int(Start_Index);
      Register.R(5) := int(End_Index);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxTextArea.Set_Selection: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Selection;

   --

   procedure Set_Font (Window       : in Object_ID;
                       Component    : in Component_ID;
                       Font         : in String;
                       Font_Width   : in integer := 12;
                       Font_Height  : in integer := 12;
                       Flags        : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Null_Font      : string := Font & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 16416;
      Register.R(3) := int(Component);
      Register.R(4) := Adr_To_Int(Null_Font'Address);
      Register.R(5) := int(Font_Width);
      Register.R(6) := int(Font_Height);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxTextArea.Set_Font: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Font;

   --

   procedure Set_Colour (Window     : in Object_ID;
                         Component  : in Component_ID;
                         Foreground : in integer := 12;
                         Background : in integer := 12;
                         Flags      : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 16417;
      Register.R(3) := int(Component);
      Register.R(4) := int(Foreground);
      Register.R(5) := int(Background);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxTextArea.Set_Colour: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Colour;

   --

   procedure Get_Colour (Window    : in Object_ID;
                         Component : in Component_ID;
                         Foreground: out integer;
                         Background: out integer;
                         Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 16418;
      Register.R(3) := int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxOptionButton.Get_Selected: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Foreground := integer(Register.R(0));
      Background := integer(Register.R(1));
   end Get_Colour;

   --

end RASCAL.ToolboxTextArea;
