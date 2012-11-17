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

with Ada.Strings;                          use Ada.Strings;
with Ada.Strings.Fixed;                    use Ada.Strings.Fixed;
with System.Storage_Elements;              use System.Storage_Elements;
with System.Address_To_Access_Conversions;
with Interfaces.C;
with Reporter;

with RASCAL.OS;                            use RASCAL.OS;

package body RASCAL.Utility is

   --

   Linebufferlength : constant := 1024;

   -- Methods

   procedure Get_Line (File : in File_Type;
                       Item : out Unbounded_String) is
   
     function More_Input return Unbounded_String is
       Input : String (1..Linebufferlength);
       Last  : Natural;
     begin
       Get_Line (File, Input, Last);
       if Last < Input'Last then
         return To_Unbounded_String (Input(1..Last));
       else
         return To_Unbounded_String (Input(1..Last)) & More_Input;
       end if;
     end More_Input;

   begin
     Item := More_Input;
   end Get_Line;

   --

   function Get_Line (File : in File_Type) return String is
   
   dummy : Ustring;
   
   begin
     Get_Line (File,dummy);
     return S(dummy);
   end Get_Line;

   --

   procedure Put (File : in File_type;
                  Item : in Unbounded_String) is
   begin
     Put(File, To_String(Item));
   end Put;

   --

   procedure Put_Line (File : in File_Type;
                       Item : in Unbounded_String) is
   begin
     Put(File, To_String(Item));
     New_Line(File);
   end Put_Line;

   --

   function "and" (left : in Integer; right : in Unsigned) return Integer is
   begin
     return Unsigned_To_Int(Int_To_Unsigned(left) and right);
   end "and";

   --

   function "or" (left : in Integer; right : in Unsigned) return Integer is
   begin
     return Unsigned_To_Int(Int_To_Unsigned(left) or right);
   end "or";

   --

   function "xor" (left : in Integer; right : in Unsigned) return Integer is
   begin
     return Unsigned_To_Int(Int_To_Unsigned(left) xor right);
   end "xor";

   --

   function bic (left : in Integer; right : in Unsigned) return Integer is
   begin
      return "and"(left,not right);
   end bic;

   --

   function charbool (char : in Character) return Boolean is
   
   begin
     if char = '0' then
       return false;
     else
       return true;
     end if;
   exception
     when others => Reporter.Report("Exception raised in Utility.charbool!");
                    raise;
   end charbool;

   --

   function boolstr (bool : in Boolean) return String is
   
   begin
     if bool then
       return "1";
     else
       return "0";
     end if;
     exception
       when others => Reporter.Report("Exception raised in Utility.boolstr!");
                      raise;
   end boolstr;

   --

   function intstr (int : in Integer) return String is
   begin
     return Trim(Integer'Image(int),left);
     exception
       when others => Reporter.Report("Exception raised in Utility.intstr!");
                      raise;
   end intstr;

   --

   function strint (str : in String) return Integer is
   begin
     return Integer'Value(str);
     exception
       when others => Reporter.Report("Exception raised in Utility.strint!");
                      raise;
   end strint;

   --

   function Adr_To_Int (adr : in Address) return Interfaces.C.int is
   begin
     return Interfaces.C.int(To_Integer(adr));
   end Adr_To_Int;

   --

   function Int_To_Adr (cint : in Interfaces.C.int) return Address is
   begin
     return To_Address(Integer_Address(cint));
   end Int_To_Adr;

   --

   function Adr_To_Integer (adr : in Address) return Integer is
   begin
     return Integer(To_Integer(adr));
   end Adr_To_Integer;

   --

   function Integer_To_Adr (aint : in Integer) return Address is
   begin
     return To_Address(Integer_Address(aint));
   end Integer_To_Adr;

   --

   function StripLeadingSpaces (Str : in String) return String is
   
   
   begin
     return Trim(Str,Left);
     exception
       when others => Reporter.Report("Exception raised in Utility.StripLeadingSpaces!");
                      raise;
   end StripLeadingSpaces;

   --

   function StripTrailingSpaces (Str : in String) return String is
   
   
   begin
     return Trim(Str,Right);
     exception
       when others => Reporter.Report("Exception raised in Utility.StripLeadingSpaces!");
                      raise;
   end StripTrailingSpaces;

   --

   function StripTrailingZeroes (Str : in String) return String is
   
   Ende : Integer;
   
   begin
      Ende:=Str'Last;
      while (Ende > Str'First and Str(Ende) = ASCII.NUL) loop
         Ende:=Ende-1;
      end loop;
      return Str(Str'First..Ende);
      exception
        when others => Reporter.Report("Exception raised in Utility.StripTrailingZeroes!");
                       raise;
   end StripTrailingZeroes;

   --

   function Align(nr : Integer) return Integer is
   aligned : integer := nr;
   begin
   
       while aligned mod 4 /= 0 loop
             aligned := aligned + 1;
       end loop;
       return aligned;   
   end Align;

   --

   procedure Call_OS_CLI (Command : in String) is
   
      Local_String : String := Command & ASCII.NUL;
      Error        : oserror_access;
      OS_CLI       : constant Interfaces.C.Unsigned := 16#5#;
      Regs         : aliased Kernel.SWI_Regs;
   begin
      Regs.R(0) := Adr_To_Int (Local_String'Address);
      Error := Kernel.SWI (OS_CLI, Regs'Access, Regs'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("Utility.Call_OS_CLI: " & Interfaces.C.To_Ada(Error.ErrMess)));         
         OS.Raise_Error(Error);
      end if;
   exception
      when others => null;
   end Call_OS_CLI;

   --

end RASCAL.Utility;
