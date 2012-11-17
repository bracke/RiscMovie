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

with Kernel;           use Kernel;
with Interfaces.C;     use Interfaces.C;
with Reporter;

with RASCAL.Memory;    use RASCAL.Memory;
with RASCAL.Utility;   use RASCAL.Utility;
with RASCAL.OS;        use RASCAL.OS;

package body RASCAL.Hourglass is

   Hourglass_On    : Constant := 16#406c0#;
   Hourglass_Off   : Constant := 16#406c1#;
   Hourglass_Start : Constant := 16#406c3#;

   --

   procedure On is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Error := Kernel.SWI (Hourglass_On,register'Access,register'Access);
      if error /= null then
         pragma Debug(Reporter.Report("Hourglass.On: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end On;

   --

   procedure Off is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Error := Kernel.SWI (Hourglass_Off,register'Access,register'Access);
      if error /= null then
         pragma Debug(Reporter.Report("Hourglass.Off: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Off;

   --

   procedure Start (Centi : in Natural) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Centi);
      Error := Kernel.SWI (Hourglass_Start,register'Access,register'Access);
      if error /= null then
         pragma Debug(Reporter.Report("Hourglass.Start: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Start;

   --

end RASCAL.Hourglass;
