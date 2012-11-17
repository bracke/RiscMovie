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
with Reporter;

with RASCAL.Utility;    use RASCAL.Utility;
with RASCAL.Memory;     use RASCAL.Memory;
with RASCAL.Convert;    use RASCAL.Convert;
with RASCAL.OS;         use RASCAL.OS;

package body RASCAL.SystemInfo is

   OS_Byte             : constant Interfaces.C.unsigned :=16#6#;
   OS_Memory           : constant Interfaces.C.unsigned :=16#68#;
   OS_ReadSysInfo      : constant Interfaces.C.unsigned :=16#58#;
   OS_PlatformFeatures : constant Interfaces.C.unsigned :=16#6D#;
   OS_MMUControl       : constant Interfaces.C.unsigned :=16#6b#;

   --

   function Get_CacheStatus return Cache_Status_Type is

     Register : aliased Kernel.swi_regs;
     Error    : oserror_access; 
   begin
     Register.R(0) := 0;
     Register.R(1) := 0;
     Register.R(2) := -1;
     Error := Kernel.swi(OS_MMUControl,Register'Access,Register'Access);
     if Error /= null then
        raise unknown_cache_state;
     end if;

     case Register.R(1) is
     when 4733 => return On;
     when 4729 => return IW;
     when 633  => return W;
     when 625  => return Off;
     when others => raise unknown_cache_state;
     end case;

   end Get_CacheStatus;
   
   --

   function Get_WindowManager_Version return Integer is

     Register : aliased Kernel.swi_regs;
     Error    : oserror_access;
     OS_Ident : Integer;
   begin
     Register.R(0) := 129;
     Register.R(1) := 0;
     Register.R(2) := 16#ff#;

     Error := Kernel.swi(OS_Byte,Register'Access,Register'Access);
  
     if Error /= null then
        pragma Debug(Reporter.Report("Wimp.Get_WindowManager_Version - " & To_Ada(Error.ErrMess)));
        OS.Raise_Error(Error);
     end if;

     OS_Ident := Integer(Register.R(1));

     -- RISC OS 2 or worse
     if OS_Ident <= 16#a3# then
        return 200;
     end if;

     Register.R(0) := 7;
     Error := Kernel.swi(OS_ReadSysInfo,Register'Access,Register'Access);
  
     if Error /= null then
        pragma Debug(Reporter.Report("Wimp.Get_WindowManager_Version II - " & To_Ada(Error.ErrMess)));
        OS.Raise_Error(Error);
     end if;
     return Integer(Register.R(0));
     
   end Get_WindowManager_Version;

   --

   function Get_OS_Declaration return String is

     Register : aliased Kernel.swi_regs;
     Error    : oserror_access;
   begin
     Register.R(0) := 0;
     Register.R(1) := 0;
     Register.R(2) := 16#ff#;
     Error := Kernel.swi(OS_Byte,Register'Access,Register'Access);
     return To_Ada(Error.ErrMess);
   end Get_OS_Declaration;

   --

   function Get_OS_Info (Info : in Version_Info_Type := Version_Name) return String is
   
     Register : aliased Kernel.swi_regs;
     Error    : oserror_access;
   begin
     Register.R(0) := 9;
     Register.R(1) := int(Version_Info_Type'Pos(Info));
     Error := Kernel.swi(OS_ReadSysInfo,Register'Access,Register'Access);
  
     if Error = null and then
        Register.R(0) /=0 then

        return MemoryToString (Int_To_Adr(Register.R(0)));
     end if;
     return "";
   end Get_OS_Info;

   --

   function Get_Memory_Size (Info : in Memory_Info_Type) return Integer is
   
     Register : aliased Kernel.swi_regs;
     Error    : oserror_access;
   begin
     Register.R(0) := int(Utility."or" ((Memory_Info_Type'Pos(Info)+1) * 2**8,8));
     Error := Kernel.swi(OS_Memory,Register'Access,Register'Access);
  
     if Error /= null then
        return 0;
     end if;
     return Integer(Register.R(1)) * Integer(Register.R(2));
   end Get_Memory_Size;

   --

   procedure Get_Machine_ID (Low  : out Integer;
                             High : out Integer) is

     Register : aliased Kernel.swi_regs;
     Error    : oserror_access;
   begin
     Register.R(0) := 2;
     Error := Kernel.swi(OS_ReadSysInfo,Register'Access,Register'Access);

     if Error = null then
        Low  := Integer(Register.R(3));
        High := Integer(Register.R(4));
     end if;
   end Get_Machine_ID;

   --

   function Get_Machine_ID return String is

      Low,High : Integer := 0;
   begin
     Get_Machine_ID (Low,High);
     return Convert.Integer_To_Hex (High,8) & " " & Convert.Integer_To_Hex (Low,8);
   end Get_Machine_ID;

   --

   procedure Get_Ethernet_Address (Low  : out Integer;
                                   High : out Integer) is

     Register : aliased Kernel.swi_regs;
     Error    : oserror_access;
   begin
     Register.R(0) := 4;
     Error := Kernel.swi(OS_ReadSysInfo,Register'Access,Register'Access);

     if Error = null then
        Low  := Integer(Register.R(0));
        High := Integer(Register.R(1));
     end if;
   end Get_Ethernet_Address;

   --

   function Get_Ethernet_Address return String is

      Low,High : Integer := 0;
   begin
     Get_Ethernet_Address (Low,High);
     return Convert.Integer_To_Hex (High,8) & " " & Convert.Integer_To_Hex (Low,8);
   end Get_Ethernet_Address;

   --

   function Get_CodeFeatures return Integer is
   
     Register : aliased Kernel.swi_regs;
     Error    : oserror_access;
   begin
     Register.R(0) := 0;
     Error := Kernel.swi(OS_PlatformFeatures,Register'Access,Register'Access);
  
     if Error /= null then
        pragma Debug(Reporter.Report("SystemInfo.Get_CodeFeatures - " & To_Ada(Error.ErrMess)));
        OS.Raise_Error(Error);
     end if;
     return Integer(Register.R(0));
   end Get_CodeFeatures;

   --

   function Get_CPU return CPU_Type is
   
     Register : aliased Kernel.swi_regs;
     Error    : oserror_access;
     Flags    : Integer;
   begin
     Register.R(0) := 0;
     Error := Kernel.swi(OS_PlatformFeatures,Register'Access,Register'Access);
  
     if Error /= null then
        return Old;
     end if;
     Flags := Integer(Register.R(0));
     if utility."and"(Flags,Code_Feature_No26bit) = integer(Code_Feature_No26bit) then
        return New_32bitonly;
     end if;
        
     if utility."and"(Flags,Code_Feature_Syncronise) = integer(Code_Feature_Syncronise) then
        return StrongARM;
     else
        return ARM6_7;
     end if;
   end Get_CPU;

   --

   procedure Report is
   begin
      Reporter.Text("\R Machine information");
      Reporter.Text("\b CPU: " & CPU_Type'Image(Get_CPU));
      Reporter.Text("\b OS Declaration: " & Get_OS_Declaration);
      
      for i in Version_Info_Type'Range loop
         Reporter.Text("\b " & Version_Info_Type'Image(i) & ": " & Get_OS_Info(i));
      end loop;
      for i in Memory_Info_Type'Range loop
         Reporter.Text("\b " & Memory_Info_Type'Image(i) & ": " & intstr(Get_Memory_Size(i)/(1024*1024)) & "MB");
      end loop;
      Reporter.Text("\b Machine ID: " & Get_Machine_ID);
      Reporter.Text("\b Ethernet Address: " & Get_Ethernet_Address);
   end Report;

   --

end RASCAL.SystemInfo;
