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

with Kernel;                     use Kernel;
with Interfaces.C;               use Interfaces.C;
with System.Storage_Elements;    use System.Storage_Elements;
with Reporter;

with RASCAL.Memory;              use RASCAL.Memory;
with RASCAL.Utility;             use RASCAL.Utility;
with RASCAL.OS;

package body RASCAL.Variable is

   OS_ReadVarVal  : constant := 16#23#;
   OS_SetVarVal   : constant := 16#24#;
   OS_GSTrans     : constant := 16#27#;

   --

   function Exists (Name : in String) return Boolean is

      Name_0    : String := Name & ASCII.NUL;
      Register  : aliased Kernel.SWI_regs;
      Error     : OSError_Access;
   begin
      Register.R(0) := Adr_To_Int(Name_0'address);
      Register.R(1) := 0;
      Register.R(2) := -1;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Error := Kernel.SWI (OS_ReadVarVal,register'Access,register'Access);
      return Integer(Register.R(2)) /=0;
   end Exists;

   --

   procedure Set_Value (Name      : string;
                        New_Value : string;
                        Var_Type  : Variable_Type := Literal) is

      Register       : aliased Kernel.swi_regs;
      Name_0         : string := Name & ASCII.NUL;
      Error          : OSError_Access;
   begin
      Register.R(0) := Adr_To_Int(Name_0'Address);
      Register.R(1) := Adr_To_Int(New_Value'Address);
      Register.R(2) := New_Value'Length;
      Register.R(3) := 0;
      Register.R(4) := Variable_Type'Pos(Var_Type);
      Error := Kernel.SWI (OS_SetVarVal,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Variable.Set_Variable: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Value;

   --

   procedure UnSet (Name : string) is

      Register : aliased Kernel.SWI_regs;
      Name_0   : string := Name & ASCII.NUL;
      Error    : OSError_Access;
   begin
      Register.R(0) := Adr_To_Int(Name_0'Address);
      Register.R(1) := 0;
      Register.R(2) := -1;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Error := Kernel.SWI (OS_SetVarVal,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Variable.UnSet_Variable: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end UnSet;


   --

   function Get_Value (Variable : in String) return String is

      variable_0  : String := Variable & ASCII.NUL;
      Register    : aliased Kernel.SWI_regs;
      Error       : OSError_Access;
      Bytes       : Integer;
   begin
      Register.R(0) := int(To_Integer(Variable_0'address));
      Register.R(1) := 0;
      Register.R(2) := -1;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Error := Kernel.SWI (OS_ReadVarVal,Register'Access,Register'Access);

      Bytes := -1*Integer(Register.R(2));
      if Bytes = 0 then
         return "";
      end if;

      declare
         Buffer : String (1..Bytes);
      begin
         Register.R(0) := int(To_Integer(Variable_0'address));
         Register.R(1) := Adr_To_Int(Buffer'Address);
         Register.R(2) := int(Bytes);
         Register.R(3) := 0;
         Register.R(4) := 0;
         Error := Kernel.SWI (OS_ReadVarVal,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("Variable.Get_Variable: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return Buffer(1..Bytes-1);
      end;
   end Get_Value;

   --

   function Translate (Str : in String) return String is

      Str_0    : String := Str & ASCII.NUL;
      Register : aliased Kernel.SWI_regs;
      Error    : OSError_Access;
      Result   : String(1..1024);
   begin
      Register.R(0) := int(To_Integer(Str_0'address));
      Register.R(1) := int(To_Integer(Result'Address));
      Register.R(2) := 1024;

      Error := Kernel.SWI (OS_GSTrans,Register'Access,Register'Access);
      if Error /= null then
         return "";
      else
         return Result (1..Integer(Register.R(2)));
      end if;
   end Translate;

   --

end RASCAL.Variable;