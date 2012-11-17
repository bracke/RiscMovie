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

with Kernel;          use Kernel;
with Interfaces.C;    use Interfaces.C;

with RASCAL.Utility;  use RASCAL.Utility;

package body RASCAL.Module is

   OS_Module   : constant := 16#1E#;

   --

   function Is_Module (Name : in string) return boolean is

      Register       : aliased Kernel.swi_regs;
      Error          : oserror_access;
      Zero_Name      : string := Name & ASCII.NUL;
   begin
      Register.R(0):=18;
      Register.R(1):=Adr_To_Int(Zero_Name'Address);
      Error := Kernel.swi (OS_Module,register'Access, register'Access);

      if Error /= null then
         return false;
      end if;
      return true;
         
   end is_Module;

   --
    
end RASCAL.Module;
