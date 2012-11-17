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
with RASCAL.TaskManager;
with RASCAL.Variable;
with RASCAL.UserMessages;        Use RASCAL.UserMessages;
with RASCAL.Utility;             use RASCAL.Utility;
with RASCAL.Memory;              use RASCAL.Memory;
with RASCAL.FileExternal;
with Kernel;                     use Kernel;
with Interfaces.C;               use Interfaces.C;
with Reporter;

package body RASCAL.InteractiveHelp is

   package FileExternal renames RASCAL.FileExternal;

   --

   procedure Run is

      Wimp_ProcessKey : constant := 16#400DC#;
      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Sys_Variable   : String := Variable.Get_Value("Help$Dir");
      Message        : Message_HelpEnable;
   begin
      if TaskManager.is_Task("Help") or else TaskManager.is_Task("Info") then

         SendMessage(Message'Address,17,Message_Event_HelpEnable,0,0,24);

         Register.R(0):=385;
         Error := Kernel.swi (Wimp_ProcessKey,register'Access, register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("InteractiveHelp.Run: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
      else
         if Sys_Variable'Length > 0 then
            Call_OS_CLI("Filer_Run <Help$Dir>");
         else
            if FileExternal.Exists ("<Boot$Dir>.^.Apps.!Help") then
               Call_OS_CLI("Filer_Run <Boot$Dir>.^.Apps.!Help");
            elsif FileExternal.Exists ("<Boot$Dir>.^.Apps.!Info") then
               Call_OS_CLI("Filer_Run <Boot$Dir>.^.Apps.!Info");
            elsif FileExternal.Exists ("Resources:$.Apps.!Help") then
               Call_OS_CLI("Filer_Run Resources:$.Apps.!Help");
            elsif FileExternal.Exists ("Resources:$.Apps.!Info") then
               Call_OS_CLI("Filer_Run Resources:$.Apps.!Info");
            else
               raise Help_Not_Found;
            end if;
         end if;
      end if;
   end Run;

   --

end RASCAL.InteractiveHelp;
