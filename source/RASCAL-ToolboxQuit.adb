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

with RASCAL.Memory;     use RASCAL.Memory;
with RASCAL.OS;         use RASCAL.OS;
with RASCAL.Utility;    use RASCAL.Utility;

with Kernel;            use Kernel;
with Reporter;

package body RASCAL.ToolboxQuit is

   Toolbox_ObjectMiscOp : constant Interfaces.C.unsigned := 16#44EC6#;

   --

   function Get_Window_ID (Quit : in Object_ID;
                           Flags: in System.Unsigned_Types.Unsigned := 0) return Object_ID is

      Register             : aliased Kernel.swi_regs;
      Error                : OSError_Access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Quit);
      Register.R(2) := 0;
      Error := Kernel.SWI(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         Pragma Debug(Reporter.Report("ToolboxQuit.Get_Window_ID: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
         return -1;
      end if;
      return Object_ID(Register.R(0));
   end Get_Window_ID;

   --

   function Get_Message (Quit : in Object_ID;
                         Flags: in System.Unsigned_Types.Unsigned := 0) return string is

      Register             : aliased Kernel.swi_regs;
      Error                : OSError_Access;
      Buffer_Size          : integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Quit);
      Register.R(2) := 2;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Error := Kernel.SWI(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         Pragma Debug(Reporter.Report("ToolboxQuit.Get_Message: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
         return "";
      end if;
      Buffer_Size := Integer (Register.R(4));
      declare
         Buffer : String (1..Buffer_Size);
      begin
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(Quit);
         Register.R(2) := 2;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := int(Buffer_Size);
         Error := Kernel.SWI(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            Pragma Debug(Reporter.Report("ToolboxQuit.Get_Message: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
            return "";
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_Message;

   --

   function Get_Title (Quit : in Object_ID;
                       Flags: in System.Unsigned_Types.Unsigned := 0) return String is

      Register             : aliased Kernel.swi_regs;
      Error                : OSError_Access;
      Buffer_Size          : Integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Quit);
      Register.R(2) := 3;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Error := Kernel.SWI(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         Pragma Debug(Reporter.Report("ToolboxQuit.Get_Title: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
         return "";
      end if;
      Buffer_Size := integer(Register.R(4));
      declare
         Buffer : string(1..Buffer_Size);
      begin
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(Quit);
         Register.R(2) := 3;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := int(Buffer_Size);
         Error := Kernel.SWI(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            Pragma Debug(Reporter.Report("ToolboxQuit.Get_Title: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
            return "";
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_Title;

   --

   procedure Set_Message (Quit      : in Object_ID;
                          Message   : in String;
                          Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register             : aliased Kernel.swi_regs;
      Error                : OSError_Access;
      Null_Message         : String := Message & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Quit);
      Register.R(2) := 1;
      Register.R(3) := Adr_To_Int(Null_Message'Address);
      Error := Kernel.SWI(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         Pragma Debug(Reporter.Report("ToolboxQuit.Set_Message: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
         null;
      end if;
   end Set_Message;

   --

   procedure Set_Title (Quit      : in Object_ID;
                        Title     : in string;
                        Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register     : aliased Kernel.swi_regs;
      Error        : OSError_Access;
      Null_Title   : String := Title & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Quit);
      Register.R(2) := 3;
      Register.R(3) := Adr_To_Int(Null_Title'Address);
      Error := Kernel.SWI(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         Pragma Debug(Reporter.Report("ToolboxQuit.Set_Title: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Title;

   --

end RASCAL.ToolboxQuit;
