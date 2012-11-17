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
with RASCAL.memory;              use RASCAL.memory;
with RASCAL.Utility;             use RASCAL.Utility;

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with System.Storage_Elements;    use System.Storage_Elements;
with kernel;                     use kernel;
with Reporter;

package body RASCAL.TaskManager is

   TaskManager_EnumerateTasks           : constant Interfaces.C.unsigned := 16#42681#;
   TaskManager_TaskNameFromHandle       : constant Interfaces.C.unsigned := 16#42680#;
   Wimp_StartTask                       : constant Interfaces.C.unsigned := 16#400DE#;
   Wimp_SendMessage                     : constant Interfaces.C.unsigned := 16#400E7#;

   --

   function Start_Task (Command : in String) return Integer is
   
      Command_0    : String := Command & ASCII.NUL;
      Error        : oserror_access;
      Register     : aliased Kernel.SWI_Regs;
   begin
      Register.R(0) := Adr_To_Int (Command_0'Address);
      Error := Kernel.SWI (Wimp_StartTask, Register'Access, Register'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("TaskManager.Start_Task: " &
                                      Interfaces.C.To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Integer (Register.R(0));
   end Start_Task;

   --

   function Enumerate_Task (Buffer : in Task_Type;
                            Index  : in integer := 0) return integer is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Index);
      Register.R(1) := int(To_Integer(Buffer.Handle'Address));
      Register.R(2) := 16;
      Error := Kernel.SWI (TaskManager_EnumerateTasks,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("TaskManager.Enumerate_Task: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
         return -2;
      else
         if integer(Register.R(2)) = 0 then
            return Integer(Register.R(0));
         else
            return -2;
         end if;
      end if;
   end Enumerate_Task;

   --

   function Get_TaskName (Handle : in integer) return String is

      Register            : aliased Kernel.swi_regs;
      Error               : oserror_access;
   begin
      Register.R(0) := int (Handle);
      Error := Kernel.SWI (TaskManager_TaskNameFromHandle,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("TaskManager.Get_TaskName: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
         return "";
      else
         return MemoryToString (Int_To_Adr (Register.R (0)));
      end if;
   end Get_Taskname;

   --

   function Get_TaskHandle (Name : in String) return Integer is

      Buffer  : Task_Type;
      i       : integer   := Enumerate_Task (Buffer);
   begin
      loop
        if Name = MemoryToString (Buffer.Name) then
           return Buffer.Handle;
        end if;
        exit when i < 0;

        i := Enumerate_Task (Buffer,i);
      end loop;
      raise Unknown_Task;
   end Get_TaskHandle;

   --

   function Is_Task (Name : in String) return boolean is

      Buffer  : Task_Type;
      i       : integer   := Enumerate_Task (Buffer);
   begin
      loop
        if Name = MemoryToString (Buffer.Name) then
           return true;
        end if;
        exit when i < 0;

        i := Enumerate_Task (Buffer,i);
      end loop;
      return false;
   end is_Task;

   --

   function Nr_Of_Tasks return Natural is

      Buffer  : Task_Type;
      i       : integer   := 0;
      Nr      : Natural   := 0;
   begin
      loop
         exit when i < 0;
         i  := Enumerate_Task (Buffer,i);
         if i /=-2 then
            Nr := Nr + 1;
         end if;
      end loop;
      return Nr;
   end Nr_Of_Tasks;

   --

   function Get_Task_List return Task_List_Type is

      Tasks     : Natural                   := Nr_Of_Tasks;
      Task_List : Task_List_Type (1..Tasks);
      i         : Integer                   := 0;
   begin
      for x in Task_List'Range loop
         i := Enumerate_Task (Task_List(x),i);
      end loop;
      return Task_List;
   end Get_Task_List;

   --

   function Get_WindowOwner (Window : Wimp_Handle_Type;
                             Icon   : Icon_Handle_Type) return Integer is
   
      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
      Buffer   : array (0..6) of integer;
   begin
      Buffer(0) := 20;
      Buffer(2) := 0;
      Buffer(4) := 0;

      Register.R(0) := 19;
      Register.R(1) := Adr_To_Int(Buffer'Address);
      Register.R(2) := Int(Window);
      Register.R(3) := Int(Icon);
      Error := Kernel.SWI (Wimp_SendMessage,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("TaskManager.Get_WindowOwner: " &
                                      Interfaces.C.To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Integer(Register.R(2));
   end Get_WindowOwner;

   --

end RASCAL.TaskManager;
