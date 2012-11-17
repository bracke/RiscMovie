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

with RASCAL.Time;       use RASCAL.Time;
with RASCAL.Memory;     use RASCAL.Memory;
with RASCAL.OS;         use RASCAL.OS;
with RASCAL.Utility;    use RASCAL.Utility;

with Kernel;            use Kernel;
with Interfaces.C;      use Interfaces.C;
with Reporter;

package body RASCAL.ToolboxFileInfo is

   Toolbox_ObjectMiscOp : constant Interfaces.C.unsigned := 16#44EC6#;

   --

   function Get_Date (FileInfo : Object_ID;
                      Flags    : in System.Unsigned_Types.Unsigned := 0) return Address is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
      Buffer               : UTC_Time_Type;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(FileInfo);
      Register.R(2) := 10;
      Register.R(3) := Adr_To_Int(Buffer'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxFileInfo.Get_Date: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      --      return UTC_Time_Type(Register.R(0));
      return Buffer'Address;
   end Get_Date;

   --

   function Get_FileName (FileInfo : Object_ID;
                          Flags    : in System.Unsigned_Types.Unsigned := 0) return string is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
      Buffer_Size          : integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(FileInfo);
      Register.R(2) := 6;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxFileInfo.Get_FileName: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := integer(Register.R(4));

      declare
         Buffer : string(1..Buffer_Size);
      begin
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(FileInfo);
         Register.R(2) := 6;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := int(Buffer_Size);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxFileInfo.Get_FileName: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_FileName;

   --

   
   function Get_FileSize (FileInfo : Object_ID;
                           Flags    : in System.Unsigned_Types.Unsigned := 0) return integer is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(FileInfo);
      Register.R(2) := 8;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxFileInfo.Get_FileSize: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return integer(Register.R(0));

   end Get_FileSize;

   --

   
   function Get_FileType (FileInfo : in Object_ID;
                          Flags    : in System.Unsigned_Types.Unsigned := 0) return integer is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(FileInfo);
      Register.R(2) := 4;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxFileInfo.Get_FileType: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return integer(Register.R(0));

   end Get_FileType;

   --

   function Get_Modified (FileInfo : Object_ID;
                          Flags      : in System.Unsigned_Types.Unsigned := 0) return boolean is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(FileInfo);
      Register.R(2) := 2;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxFileInfo.Get_Modified: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      return "And"(1,int_to_unsigned(integer(Register.R(0))))=1;

   end Get_Modified;

   --

   function Get_Title (FileInfo : Object_ID;
                       Flags    : in System.Unsigned_Types.Unsigned := 0) return string is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
      Buffer_Size          : integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(FileInfo);
      Register.R(2) := 12;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma DEbug(Reporter.Report("ToolboxFileInfo.Get_Title: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := integer(Register.R(4));

      declare
         Buffer : string(1..Buffer_Size);
      begin
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(FileInfo);
         Register.R(2) := 12;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := int(Buffer_Size);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxFileInfo.Get_Title: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_Title;

   --

   function Get_Window_ID (FileInfo : Object_ID;
                           Flags    : in System.Unsigned_Types.Unsigned := 0) return Object_ID is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(FileInfo);
      Register.R(2) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxFileInfo.Get_Window_ID: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Object_ID(Register.R(0));
   end Get_Window_ID;

   --

   procedure Set_Date (FileInfo  : in Object_ID;
                       Date      : in UTC_Time_Type;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(FileInfo);
      Register.R(2) := 9;
      Register.R(3) := Adr_To_Int(Date'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxFileInfo.Set_Date: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Date;

   --

   procedure Set_FileName (FileInfo  : in Object_ID;
                           FileName  : in string;
                           Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register        : aliased Kernel.swi_regs;
      Error           : oserror_access;
      Null_FileName   : string := FileName & Character'Val(0);
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(FileInfo);
      Register.R(2) := 5;
      Register.R(3) := Adr_To_Int(Null_FileName'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxFileInfo.Set_FileName: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_FileName;

   --

   procedure Set_FileType (FileInfo : in Object_ID;
                           FileType : in integer;
                           Flags    : in System.Unsigned_Types.Unsigned := 0) is

      Register      : aliased Kernel.swi_regs;
      Error         : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(FileInfo);
      Register.R(2) := 3;
      Register.R(3) := int(FileType);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxFileInfo.Set_FileType: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Set_FileType;

   --

   procedure Set_FileSize (FileInfo : in Object_ID;
                           FileSize : in integer;
                           Flags    : in System.Unsigned_Types.Unsigned := 0) is

      Register      : aliased Kernel.swi_regs;
      Error         : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(FileInfo);
      Register.R(2) := 7;
      Register.R(3) := int(FileSize);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxFileInfo.Set_FileSize: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_FileSize;

   --

   procedure Set_Modified (FileInfo   : in Object_ID;
                           Modified   : in boolean;
                           Flags      : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(FileInfo);
      Register.R(2) := 1;
      Register.R(3) := boolean'Pos(Modified);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxFileInfo.Set_Modified: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Modified;

   --

   procedure Set_Title (FileInfo  : in Object_ID;
                        Title     : in string;
                        Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register     : aliased Kernel.swi_regs;
      Error        : oserror_access;
      Null_Title   : string := Title & Character'Val(0);
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(FileInfo);
      Register.R(2) := 11;
      Register.R(3) := Adr_To_Int(Null_Title'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxFileInfo.Set_Title: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Set_Title;

   --

   
end RASCAL.ToolboxFileInfo;
