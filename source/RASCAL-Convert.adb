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

with RASCAL.Utility;             use RASCAL.Utility;

with Interfaces.C;               use Interfaces.C;
with System.Storage_Elements;    use System.Storage_Elements;
with System.Unsigned_Types;      use System.Unsigned_Types;
with Kernel;                     use Kernel;
with Reporter;

package body RASCAL.Convert is

   --

   function Integer_To_Hex (Nr    : in Integer;
                            Width : in Positive := 4) return String is

      OS_ConvertHex : Interfaces.C.unsigned;
      Register      : aliased Kernel.swi_regs;
      Error         : oserror_access;
      Buffer_Size   : constant Positive := 9;
      Buffer        : String(1..Buffer_Size);
      Nibbles       : Interfaces.C.unsigned := Interfaces.C.Unsigned(Width);
   begin
      if Nibbles > 1 and (Nibbles mod 2 > 0) then
         Nibbles := Nibbles + 1;
      end if;
      if Nibbles > 8 then
         Nibbles := 8;
      end if;
      Register.R(0) := Interfaces.C.int(Nr);
      Register.R(1) := Adr_To_Int(Buffer'Address);
      Register.R(2) := int(Buffer_Size);
      OS_ConvertHex := 16#D0# + Nibbles / 2;      
      Error := Kernel.SWI (OS_ConvertHex,register'Access,register'Access);
      if Error = null and then
         (Buffer_Size-Integer(Register.R(2))) >= 0 then
         return Buffer(1..Buffer_Size-Integer(Register.R(2)));
      end if;
      return "";
   end Integer_To_Hex;

   --

   function Hex_To_Integer (Hex : in String) return Integer is

      Digit_Character  : Character;
      Digit_Value      : Integer range 0..15;
      Result           : Integer := 0;

      Zero_Position      : constant := Character'Pos('0');
      Capital_A_Position : constant := Character'Pos('A');
      Small_a_Position   : constant := Character'Pos('a');
   begin
      for I in Hex'Range loop

         Digit_Character := Hex (I);
         case Digit_Character is
         when '0'..'9' => Digit_Value := Character'Pos(Digit_Character) - Zero_Position;
         when 'A'..'F' => Digit_Value := Character'Pos(Digit_Character) - Capital_A_Position+10;
         when 'a'..'f' => Digit_Value := Character'Pos(Digit_Character) - Small_a_Position+10;
         when others => raise Invalid_Digit_Error;
         end case;

         begin
            Result := 16*Result + Digit_Value;
         exception
            when Constraint_Error => raise Result_Size_Error;
         end;
      end loop;

      return Result;
   end Hex_To_Integer;

   --

end RASCAL.Convert;