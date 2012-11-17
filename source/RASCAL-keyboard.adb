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
with RASCAL.WimpTask;   use RASCAL.WimpTask;
with RASCAL.SystemInfo;

with Kernel;            use Kernel;
with Interfaces.C;      use Interfaces.C;
with Reporter;

package body RASCAL.Keyboard is

   OS_Byte         : constant Interfaces.C.unsigned :=16#06#;
   Wimp_ProcessKey : constant Interfaces.C.unsigned :=16#400DC#;

   --

   function Is_Shift return Boolean is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := 121;
      Register.R(1) := 16#80#;

      Error := Kernel.swi(OS_Byte,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Keyboard.Is_Shift: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Register.R(1) = 16#ff#;
   end Is_Shift;

   --

   function Is_Alt return Boolean is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := 121;
      Register.R(1) := int(Utility."xor"(2,16#80#));

      Error := Kernel.swi(OS_Byte,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Keyboard.Is_Alt: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Register.R(1) = 16#ff#;
   end Is_Alt;

   --

   function Is_Control return Boolean is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
   begin
      Register.R(0) := 121;
      Register.R(1) := int(Utility."xor"(1,16#80#));

      Error := Kernel.swi(OS_Byte,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Keyboard.Is_Alt: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Register.R(1) = 16#ff#;
   end Is_Control;

   --

   procedure Process_Key (Key : in Character := ASCII.CR) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Character'Pos(Key));
      Error := Kernel.swi(Wimp_ProcessKey,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Keyboard.Process_Key: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Process_Key;

   --

   procedure Process_String (Text   : in String;
                             Sender : in out Wimp_Task_Class'Class) is

      Poll : Boolean := SystemInfo.Get_WindowManager_Version < 400;
   begin
      for i in Text'Range loop
         Process_Key(Text(i));
         if Poll then
            WimpTask.Single_Poll(Sender);
         end if;
      end loop;
      WimpTask.Single_Poll(Sender);
   end Process_String;

   --

   function Get_KeyName (Code : in Positive) return String is
   begin
      case Code is
      when 16#1#..16#1a#    => return '^' & Character'Val(16#40#+Code);
      when 16#1b#..16#60#   => return "" & Character'Val(Code);
      when 16#181#..16#189# => return 'F' & Character'Val(Character'Pos('0')+Code-16#180#);
      when 16#191#..16#199# => return "‹F" & Character'Val(Character'Pos('0')+Code-16#190#);
      when 16#1a1#..16#1a9# => return "^F" & Character'Val(Character'Pos('0')+Code-16#1a0#);
      when 16#1b1#..16#1b9# => return "^‹F" & Character'Val(Character'Pos('0')+Code-16#1b0#);      
      when 16#1ca#..16#1cc# => return "F1" & Character'Val(Character'Pos('0')+Code-16#1c9#);
      when 16#1ea#..16#1ec# => return "‹F1" & Character'Val(Character'Pos('0')+Code-16#1e9#);
      when 16#1da#..16#1dc# => return "^F1" & Character'Val(Character'Pos('0')+Code-16#1d9#);
      when 16#1fa#..16#1fc# => return "^‹F1" & Character'Val(Character'Pos('0')+Code-16#1f9#);
      when others           => return "";
      end case;
   end Get_KeyName;

   --

end RASCAL.Keyboard;
