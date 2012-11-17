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
with Interfaces.C;      use Interfaces.C;
with Reporter;

package body RASCAL.ToolboxProgInfo is

   Toolbox_ObjectMiscOp : constant := 16#44EC6#;
   
   --

   function Get_Window_ID (ProgInfo : Object_ID;
                           Flags    : in System.Unsigned_Types.Unsigned := 0) return Object_ID is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(ProgInfo);
      Register.R(2) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxProgInfo.Get_Window_ID: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Object_ID(Register.R(0));
   end Get_Window_ID;

   --

   function Get_URI (ProgInfo : Object_ID;
                     Flags: in System.Unsigned_Types.Unsigned := 0) return string is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
      Buffer_Size          : integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(ProgInfo);
      Register.R(2) := 8;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxProgInfo.Get_URI: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := integer(Register.R(4));

      declare
         Buffer : string(1..Buffer_Size);
      begin
         Register.R(0) := int(Unsigned_to_Int(Flags));
         Register.R(1) := int(ProgInfo);
         Register.R(2) := 8;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := int(Buffer_Size);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxProgInfo.Get_URI: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_URI;

   --

   function Get_Title (ProgInfo : Object_ID;
                       Flags    : in System.Unsigned_Types.Unsigned := 0) return string is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
      Buffer_Size          : integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(ProgInfo);
      Register.R(2) := 6;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxProgInfo.Get_Title: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := integer(Register.R(4));

      declare
         Buffer : string(1..Buffer_Size);
      begin
         Register.R(0) := 0;
         Register.R(1) := int(ProgInfo);
         Register.R(2) := 6;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := int(Buffer_Size);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxProgInfo.Get_Title: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_Title;

   --

   procedure Set_URI (ProgInfo      : in Object_ID;
                      URI           : in string;
                      Flags         : in System.Unsigned_Types.Unsigned := 0) is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
      Null_URI             : string := URI & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(ProgInfo);
      Register.R(2) := 7;
      Register.R(3) := Adr_To_Int(Null_URI'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxProgInfo.Set_URI: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_URI;

   --

   procedure Set_Title (ProgInfo      : in Object_ID;
                        Title     : in string;
                        Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register     : aliased Kernel.swi_regs;
      Error        : oserror_access;
      Null_Title   : string := Title & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(ProgInfo);
      Register.R(2) := 5;
      Register.R(3) := Adr_To_Int(Null_Title'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxProgInfo.Set_Title: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Title;

   --

   function Get_License_Type (ProgInfo : Object_ID;
                              Flags    : in System.Unsigned_Types.Unsigned := 0) return License_Type is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(ProgInfo);
      Register.R(2) := 4;

      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxProgInfo.Get_License_Type: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return License_Type'Val(integer(Register.R(0)));
   end Get_License_Type;

   --

   procedure Set_License_Type (ProgInfo : in Object_ID;
                               License  : in License_Type;
                               Flags    : in System.Unsigned_Types.Unsigned := 0) is

      Register     : aliased Kernel.swi_regs;
      Error        : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(ProgInfo);
      Register.R(2) := 3;
      Register.R(3) := int(License_Type'Pos(License));
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxProgInfo.Set_License_Type: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_License_Type;

   --

   function Get_Version (ProgInfo : Object_ID;
                         Flags    : in System.Unsigned_Types.Unsigned := 0) return string is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
      Buffer_Size          : integer;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(ProgInfo);
      Register.R(2) := 2;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxProgInfo.Get_Version: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Buffer_Size := integer(Register.R(4));
      declare
         Buffer : string(1..Buffer_Size);
      begin
         Register.R(0) := 0;
         Register.R(1) := int(ProgInfo);
         Register.R(2) := 2;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := int(Buffer_Size);
         Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxProgInfo.Get_Version: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return MemoryToString(Buffer'Address);
      end;
   end Get_Version;

   --

   procedure Set_Version (ProgInfo      : in Object_ID;
                          Version     : in string;
                          Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register     : aliased Kernel.swi_regs;
      Error        : oserror_access;
      Null_Version : string := Version & ASCII.NUL;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(ProgInfo);
      Register.R(2) := 1;
      Register.R(3) := Adr_To_Int(Null_Version'Address);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxProgInfo.Set_Version: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Version;

   --

   function Get_Web_Event (ProgInfo : Object_ID;
                           Flags    : in System.Unsigned_Types.Unsigned := 0) return Toolbox_Event_Code_Type is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(ProgInfo);
      Register.R(2) := 9;
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxProgInfo.Get_Web_Event: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Toolbox_Event_Code_Type(integer(Register.R(0)));
   end Get_Web_Event;

   --

   procedure Set_Web_Event (ProgInfo : in Object_ID;
                            Event    : in Toolbox_Event_Code_Type;
                            Flags    : in System.Unsigned_Types.Unsigned := 0) is

      Register     : aliased Kernel.swi_regs;
      Error        : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(ProgInfo);
      Register.R(2) := 10;
      Register.R(3) := int(integer(Event));
      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxProgInfo.Set_Web_Event: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Web_Event;

   --
   
end RASCAL.ToolboxProgInfo;
