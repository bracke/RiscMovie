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

with RASCAL.OS;        use RASCAL.OS;
with RASCAL.Memory;    use RASCAL.Memory;
with RASCAL.Utility;   use RASCAL.Utility;

with Interfaces.C;     use Interfaces.C;
with Kernel;           use Kernel;
with Reporter;         use Reporter;

package body RASCAL.UserMessages is

   --

   procedure SendMessage(Message    : in Address;
                         Reason     : in Reason_Event_Code_Type;
                         Event_Code : in Message_Event_Code_Type;
                         Window     : in Wimp_Handle_Type := 0;
                         Icon       : in Icon_handle_Type := 0;
                         Length     : in Integer := 256) is
   
      Register          : aliased Kernel.swi_regs;
      Wimp_SendMessage  : constant := 16#400E7#;
      Error             : oserror_access;
   begin
      Memory.PutWord (Align (Length),Message);
      Memory.PutWord (0,Message,3*4);
      Memory.PutWord (Integer (Event_Code),Message,4*4);

      Register.R(0) := Int (Reason);
      Register.R(1) := Adr_To_Int (Message);
      Register.R(2) := Int (Window);
      Register.R(3) := Int (Icon);

      Error := Kernel.SWI (Wimp_SendMessage,Register'Access, Register'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("UserMessages.SendMessage: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end SendMessage;

   --

   procedure Acknowledge(Message : in Address;
                         Window  : in integer := 0;
                         Icon    : in integer := 0) is

      Register       : aliased Kernel.swi_regs;
      Handle         : integer := Window;

      Wimp_SendMessage  : constant := 16#400E7#;
      Error             : oserror_access;
   begin
      Memory.PutWord(Memory.GetWord(Message,2*4),Message,3*4);
      if Handle = 0 then
         Handle :=  Memory.GetWord(Message,1*4);
      end if;
      Register.R(0) := 19;
      Register.R(1) := Adr_To_Int(Message);
      Register.R(2) := int(Handle);
      Register.R(3) := int(Icon);
      Error := Kernel.swi (Wimp_SendMessage,Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("UserMessages.Acknowledge: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Acknowledge;

   --
   
end RASCAL.UserMessages;
