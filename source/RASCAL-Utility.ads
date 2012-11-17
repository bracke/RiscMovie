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

-- @brief Misc. types and methods.
-- $Author$
-- $Date$
-- $Revision$

with Kernel;                     use Kernel;
with Text_IO;                    use Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with System;                     use System;
with System.Unsigned_Types;      use System.Unsigned_Types;
with Unchecked_Conversion;
with Interfaces.C;

package RASCAL.Utility is

   subtype Ustring is Unbounded_String;

   function U (Source : String) return Unbounded_String
                                renames To_Unbounded_String;

   function S (Source : Unbounded_String) return String
                                          renames To_String;

   --
   -- Reads a line from file.
   --
   procedure Get_Line (File : in File_Type;
                       Item : out Unbounded_String);

   --
   -- Reads a line from file.
   --
   function Get_Line (File : in File_Type) return String;

   --
   -- Writes a string to file.
   --
   procedure Put (File : in File_Type;
                  Item : in Unbounded_String);

   --
   -- Writes a string to file together with a linefeed.
   --
   procedure Put_Line (File : in File_Type;
                       Item : in Unbounded_String);

   function Unsigned_To_Int is new Unchecked_Conversion (source => Unsigned,
                                              target => Integer);

   function Int_To_Unsigned is new Unchecked_Conversion (source => Integer,
                                              target => Unsigned);

   --
   --
   --
   function "and" (left : in Integer; right : in Unsigned) return Integer;

   --
   --
   --
   function "or" (left : in Integer; right : in Unsigned) return Integer;

   --
   --
   --
   function "xor" (left : in Integer; right : in Unsigned) return Integer;

   --
   -- Bit clear. A AND (NOT B)
   --
   function bic (left : in Integer; right : in Unsigned) return Integer;

   --
   -- Returns false if char = '0', true otherwise.
   --
   function charbool (char : in Character) return Boolean;

   --
   -- Returns '0' if bool = false, '1' otherwise.
   --
   function boolstr (bool : in Boolean) return String;

   --
   -- Converts an integer to string.
   --
   function intstr (int : in Integer) return String;

   --
   -- Converts a string to an integer.
   --
   function strint (str : in String) return Integer;

   --
   -- Converts an Addres to an Interfaces.C.Int.
   --
   function Adr_To_Int (adr : in Address) return Interfaces.C.int;

   --
   -- Converts an Interfaces.C.Int to an Address.
   --
   function Int_To_Adr (cint : in Interfaces.C.int) return Address;

   --
   -- Converts an Address to an Integer.
   --
   function Adr_To_Integer (adr : in Address) return Integer;

   --
   -- Converts an Integer to an Address.
   --
   function Integer_To_Adr (aint : in Integer) return Address;

   --
   -- Strips leading spaces.
   --
   function StripLeadingSpaces (Str : in String) return String;

   --
   -- Strips trailing spaces.
   --
   function StripTrailingSpaces (Str : in String) return String;

   --
   -- Strips trailing zeroes.
   --
   function StripTrailingZeroes (Str : in String) return String;

   --
   -- Aligns the value to word boundaries.
   --
   function Align (nr : Integer) return Integer;

   --
   -- Execute a CLI - command.
   --
   procedure Call_OS_CLI (Command : in String);
    
end RASCAL.Utility;
