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

with Interfaces.C;      use Interfaces.C;
with Kernel;            use Kernel;
with Reporter;

package body RASCAL.ToolboxSaveAs is

   Toolbox_ObjectMiscOp : constant Interfaces.C.unsigned :=16#44EC6#;

   --

   function Get_Window_ID (SaveAs : in Object_ID;
                           Flags  : in System.Unsigned_Types.Unsigned := 0) return Object_ID is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(SaveAs);
      Register.R(2) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSaveAs.Get_Window_ID: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Object_ID(Register.R(0));
   end Get_Window_ID;

   
   --

   function Get_Title (SaveAs : in Object_ID;
                       Flags  : in System.Unsigned_Types.Unsigned := 0) return string is

      Register     : aliased Kernel.swi_regs;
      Error        : oserror_access;
      Buffer_Size  : integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(SaveAs);
      Register.R(2) := 2;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSaveAs.Get_Title: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Buffer_Size := integer(Register.R(4));
      declare
         Buffer : string(1..Buffer_Size);
      begin
         Register.R(0) := 0;
         Register.R(1) := int(SaveAs);
         Register.R(2) := 2;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := int(Buffer_Size);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxSaveAs.Get_Title: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_Title;

   --

   procedure Set_Title (SaveAs : in Object_ID;
                        Title  : in string;
                        Flags  : in System.Unsigned_Types.Unsigned := 0) is

      Register     : aliased Kernel.swi_regs;
      Error        : oserror_access;
      Null_Title   : string := Title & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(SaveAs);
      Register.R(2) := 3;
      Register.R(3) := Adr_To_Int(Null_Title'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSaveAs.Set_Title: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Title;

   --

   function Get_FileName (SaveAs : in Object_ID;
                         Flags   : in System.Unsigned_Types.Unsigned := 0) return string is

      Register    : aliased Kernel.swi_regs;
      Error       : oserror_access;
      Buffer_Size : integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(SaveAs);
      Register.R(2) := 4;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSaveAs.Get_FileName: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Buffer_Size := integer(Register.R(4));
      declare
         Buffer : string(1..Buffer_Size);
      begin
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(SaveAs);
         Register.R(2) := 4;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := int(Buffer_Size);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxSaveAs.Get_FileName: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_FileName;

   --

   procedure Set_FileName (SaveAs   : in Object_ID;
                           FileName : in string;
                           Flags    : in System.Unsigned_Types.Unsigned := 0) is

      Register      : aliased Kernel.swi_regs;
      Error         : oserror_access;
      Null_FileName : string := FileName & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(SaveAs);
      Register.R(2) := 3;
      Register.R(3) := Adr_To_Int(Null_FileName'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSaveAs.Set_FileName: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_FileName;

   --

   function Get_File_Size (SaveAs : in Object_ID;
                           Flags  : in System.Unsigned_Types.Unsigned := 0) return integer is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(SaveAs);
      Register.R(2) := 8;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma debug(Reporter.Report("ToolboxSaveAs.Get_File_Size: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return integer(Register.R(0));
   end Get_File_Size;

   --

   procedure Set_File_Size (SaveAs    : in Object_ID;
                            File_Size : in integer;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(SaveAs);
      Register.R(2) := 7;
      Register.R(3) := int(File_Size);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSaveAs.Set_File_Size: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_File_Size;

   --

   function Get_File_Type (SaveAs : in Object_ID;
                           Flags  : in System.Unsigned_Types.Unsigned := 0) return integer is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(SaveAs);
      Register.R(2) := 5;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSaveAs.Get_File_Type: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return integer(Register.R(0));
   end Get_File_Type;

   --

   procedure Set_File_Type (SaveAs    : in Object_ID;
                            File_Type : in integer;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(SaveAs);
      Register.R(2) := 5;
      Register.R(3) := int(File_Type);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSaveAs.Set_File_Type: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_File_Type;

   --

   procedure Save_Completed (SaveAs   : in Object_ID;
                             FileName : in string;
                             Flags    : in System.Unsigned_Types.Unsigned := 1) is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
      Null_FileName        : string := FileName & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(SaveAs);
      Register.R(2) := 12;
      Register.R(3) := Adr_To_Int(Null_FileName'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSaveAs.Save_Completed: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Save_Completed;

   --

   procedure Set_Data_Address (SaveAs         : in Object_ID;
                               Data           : in Address;
                               Data_Size      : in integer;
                               Selection      : in Address;
                               Selection_Size : in integer;
                               Flags          : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(SaveAs);
      Register.R(2) := 10;
      Register.R(3) := Adr_To_Int(Data);
      Register.R(4) := int(Data_Size);
      Register.R(5) := Adr_To_Int(Selection);
      Register.R(6) := int(Selection_Size);

      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSaveAs.Save_Completed: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Data_Address;

   --

   procedure Buffer_Filled  (SaveAs      : in Object_ID;
                             Buffer      : in Address;
                             Buffer_Size : in integer;
                             Flags       : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(SaveAs);
      Register.R(2) := 11;
      Register.R(3) := Adr_To_Int(Buffer);
      Register.R(4) := int(Buffer_Size);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSaveAs.Buffer_Filled: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Buffer_Filled;

   --

   procedure Selection_Available  (SaveAs      : in Object_ID;
                                   Selection   : in boolean;
                                   Flags       : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(SaveAs);
      Register.R(2) := 9;
      Register.R(3) := boolean'Pos(Selection);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxSaveAs.Selection_Available: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Selection_Available;

   --
   
end RASCAL.ToolboxSaveAs;
