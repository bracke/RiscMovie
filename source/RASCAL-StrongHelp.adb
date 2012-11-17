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

with RASCAL.Utility;      use RASCAL.Utility;
with RASCAL.Module;
with RASCAL.Variable;

package body RASCAL.StrongHelp is

   package Variable renames RASCAL.Variable;
   
   --

   function Run return boolean is

      sys_variable   : string := Variable.Get_Value("StrongHelp$Dir");
   begin
      if Module.is_Module("StrongHelp") then
         null;
      else
         if sys_variable'Length > 0 then
            Call_OS_CLI("Filer_Run <StrongHelp$Dir>");
         else
            return false;
         end if;
      end if;
      return true;
   end Run;

   --
    
end RASCAL.StrongHelp;
