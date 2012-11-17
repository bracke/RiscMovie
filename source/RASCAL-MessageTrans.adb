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

with System.Unsigned_Types;    use System.Unsigned_Types;     
with Interfaces.C;             use Interfaces.C;
with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;
with Kernel;                   use Kernel;
with Reporter;

with RASCAL.Utility;           use RASCAL.Utility;
with RASCAL.Memory;            use RASCAL.Memory;
with RASCAL.OS;                use RASCAL.OS;

package body RASCAL.MessageTrans is

   MessageTrans_FileInfo  : constant := 16#41500#;
   MessageTrans_OpenFile  : constant := 16#41501#;
   MessageTrans_Lookup    : constant := 16#41502#;
   MessageTrans_CloseFile : constant := 16#41504#;
   OS_Module              : constant := 16#1E#;

   --

   function Open_File (Filename : in string) return Messages_Handle_Type is

      File          : String := Filename & Character'Val (0);

      Error         : oserror_access;
      Register      : aliased Kernel.swi_regs;
      MCB           : Messages_Handle_Type := new Messages_Control_Block_Type;
      Buffer_Size   : integer := 0;
      Buffer : mem_adr_type;
   begin
      Register.R (1) := int (To_Integer (File'Address));
      Error := Kernel.swi (MessageTrans_FileInfo, Register'Access, Register'Access);

      if Error /= null then
         OS.Raise_Error(Error);
      end if;
      Buffer_Size := integer(Register.R(2));
      Buffer := AllocateFixed(Buffer_Size);
      Register.R (0) := int (To_Integer (MCB.all'Address));
      Register.R (1) := int (To_Integer (File'Address));
      Register.R (2) := Adr_To_Int (Buffer);
      Error := Kernel.swi (MessageTrans_OpenFile, Register'Access, Register'Access);
      
      if Error /= null then
         OS.Raise_Error(Error);
         raise Program_Error;
      else
         MCB.all(5) := 1;
         MCB.all(6) := Int_To_Unsigned(Adr_To_Integer (Buffer));
         return MCB;
      end if;
   end Open_File;

   --
   
   function Lookup (Token      : in string;
                    MCB        : in Messages_Handle_Type;
                    Parameter1 : in String := "";
                    Parameter2 : in String := "";
                    Parameter3 : in String := "";
                    Parameter4 : in String := "") return String is
   

      Token_0      : String := Token & ASCII.NUL;
      Parameter1_0 : String := Parameter1 & ASCII.NUL;
      Parameter2_0 : String := Parameter2 & ASCII.NUL;
      Parameter3_0 : String := Parameter3 & ASCII.NUL;
      Parameter4_0 : String := Parameter4 & ASCII.NUL;
      Register     : aliased Kernel.swi_regs;
      Buffer       : array (1 .. 64) of integer;
      Error        : oserror_access;
   begin
      if (MCB.all(5)=1) then
         Register.R(0) := int (To_Integer (MCB.all'Address));
         Register.R(1) := int (To_Integer (Token_0'Address));
         Register.R(2) := int (To_Integer (Buffer'Address));
         Register.R(3) := int (64 * 4);
         Register.R(4) := int (To_Integer (Parameter1_0'Address));
         Register.R(5) := int (To_Integer (Parameter2_0'Address));
         Register.R(6) := int (To_Integer (Parameter3_0'Address));
         Register.R(7) := int (To_Integer (Parameter4_0'Address));
         Error := Kernel.SWI (MessageTrans_Lookup, Register'Access, Register'Access);
         if Error /= null then
            OS.Raise_Error(Error);
            return "";
         else
            return Memory.MemoryToString (To_Address(Integer_Address (Register.R (2))),0,Integer(Register.R(3)));
         end if;
      else
         raise Messages_File_Is_Closed;
      end if;
   end Lookup;

   --

   procedure Close_File (MCB : Messages_Handle_Type) is

      Register     : aliased Kernel.swi_regs;
      Error        : oserror_access;
   begin
      if (MCB.all(5)=1) then
         Register.R (0) := int (To_Integer (MCB'Address));
         Error := Kernel.SWI (MessageTrans_CloseFile, Register'Access, Register'Access);
         
         if Error /= null then
            OS.Raise_Error(Error);
         end if;
         if MCB.all(6) /= 0 then
            null;
            -- Deallocate(Int_To_Adr(int(MCB.all(6))));
         end if;
         MCB.all(5) := 0;
         MCB.all(6) := 0;
      end if;
   end Close_File;

   --

   procedure Read_Boolean (Token : in String;
                           Value : in out Boolean;
                           MCB   : in Messages_Handle_Type) is

      Val : Unbounded_String;
   begin
      Val := U(MessageTrans.Lookup(Token,MCB));
      if Ada.Strings.Unbounded.Length(Val) > 0 then
         Value := Boolean'Value(S(Val));
      end if;
   exception
      when others => null;
   end Read_Boolean;

   --

   procedure Read_Integer (Token : in String;
                           Value : in out Integer;
                           MCB   : in Messages_Handle_Type) is

      Val : Unbounded_String;
   begin
      Val := U(MessageTrans.Lookup(Token,MCB));
      if Ada.Strings.Unbounded.Length(Val) > 0 then
         Value := Integer'Value(S(Val));
      end if;
   exception
      when others => null;
   end Read_Integer;

   --

   procedure Read_String (Token : in String;
                          Value : in out Unbounded_String;
                          MCB   : in Messages_Handle_Type) is

      Val : Unbounded_String;
   begin
      Val := U(MessageTrans.Lookup(Token,MCB));
      if Ada.Strings.Unbounded.Length(Val) > 0 then
         Value := Val;
      end if;
   exception
      when others => null;
   end Read_String;

   --

end RASCAL.MessageTrans;
