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
with RASCAL.Utility; use RASCAL.Utility;
with RASCAL.Memory;  use RASCAL.Memory;

with Kernel;         use Kernel;
with Interfaces.C;   use Interfaces.C;
with Reporter;

package body RASCAL.ToolboxDCS is

   Toolbox_ObjectMiscOp : constant Interfaces.C.unsigned :=16#44EC6#;

   --

   function Get_Window_ID (DCS   : in Object_ID;
                           Flags : in System.Unsigned_Types.Unsigned := 0) return Object_ID is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(DCS);
      Register.R(2) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxDCS.Get_Window_ID: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Object_ID(Register.R(0));
   end Get_Window_ID;

   --

   procedure Set_Message (DCS       : in Object_ID;
                          Message   : in string;
                          Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
      Null_Message         : string := Message & Character'Val(0);
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(DCS);
      Register.R(2) := 1;
      Register.R(3) := Adr_To_Int(Null_Message'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxDCS.Set_Message: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Set_Message;

   --

   function Get_Message (DCS   : in Object_ID;
                         Flags : in System.Unsigned_Types.Unsigned := 0) return string is

      Register    : aliased Kernel.swi_regs;
      Error       : oserror_access;
      Buffer_Size : integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(DCS);
      Register.R(2) := 2;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxDCS.Get_Message: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := integer(Register.R(4));

      declare
         Buffer : string(1..Buffer_Size);
      begin
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(DCS);
         Register.R(2) := 2;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := int(Buffer_Size);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,register'Access,register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxDCS.Get_Message: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_Message;

   --

   procedure Set_Title (DCS   : in Object_ID;
                        Titel : in string;
                        Flags : in System.Unsigned_Types.Unsigned := 0) is

      Register    : aliased Kernel.swi_regs;
      Error       : oserror_access;
      Null_Title  : string := Titel & Character'Val(0);
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(DCS);
      Register.R(2) := 3;
      Register.R(3) := Adr_To_Int(Null_Title'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxDCS.Set_Title: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Title;

   --

   function Get_Title (DCS   : in Object_ID;
                       Flags : in System.Unsigned_Types.Unsigned := 0) return string is

      Register    : aliased Kernel.swi_regs;
      Error       : oserror_access;
      Buffer_Size : integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(DCS);
      Register.R(2) := 4;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxDCS.Get_Title: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := integer(Register.R(4));

      declare
         Buffer : string(1..Buffer_Size);
      begin
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(DCS);
         Register.R(2) := 4;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := int(Buffer_Size);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,register'Access,register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxDCS.Get_Title: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_Title;

end RASCAL.ToolboxDCS;
