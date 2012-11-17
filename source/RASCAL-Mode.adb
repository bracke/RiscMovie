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

with RASCAL.Memory;     use RASCAL.Memory;
with RASCAL.Utility;    use RASCAL.Utility;
with RASCAL.OS;

package body RASCAL.Mode is

   OS_ReadModeVariable : constant := 16#35#;

   --

   function Get_X_Resolution (Unit   : in Mode_Unit_Type := Pixel;
                              ModeNr : in Integer := Current) return Integer is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int (ModeNr);
      Register.R(1) := 11;

      Error := Kernel.SWI (OS_ReadModeVariable,register'Access,register'Access);
      if Error = null then
         if Unit = OSUnits then
            return (Integer(Register.R(2))+1) * (2**Get_X_Eig_Factor);
         else
            return Integer(Register.R(2))+1;
         end if;
      end if;
      raise  No_Mode_Variable;
   end Get_X_Resolution;

   --

   function Get_Y_Resolution (Unit   : in Mode_Unit_Type := Pixel;
                              ModeNr : in Integer := Current) return Integer is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int (ModeNr);
      Register.R(1) := 12;

      Error := Kernel.SWI (OS_ReadModeVariable,register'Access,register'Access);
      if Error = null then
         if Unit = OSUnits then
            return (Integer(Register.R(2))+1) * (2**Get_Y_Eig_Factor);
         else
            return Integer(Register.R(2))+1;
         end if;
      end if;
      raise  No_Mode_Variable;      
   end Get_Y_Resolution;

   --

   function Get_Y_Eig_Factor (ModeNr : in Integer := Current) return Integer is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int (ModeNr);
      Register.R(1) := 5;

      Error := Kernel.SWI (OS_ReadModeVariable,register'Access,register'Access);
      if Error = null then
         return Integer(Register.R(2));
      end if;
      raise  No_Mode_Variable;
   end Get_Y_Eig_Factor;

   --

   function Get_X_Eig_Factor (ModeNr : in Integer := Current) return Integer is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int (ModeNr);
      Register.R(1) := 4;

      Error := Kernel.SWI (OS_ReadModeVariable,register'Access,register'Access);
      if Error = null then
         return Integer(Register.R(2));
      end if;
      raise  No_Mode_Variable;
   end Get_X_Eig_Factor;

   --

   
end RASCAL.Mode;