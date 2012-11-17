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

-- @brief Memory types and methods.
-- $Author$
-- $Date$
-- $Revision$

with System;               use System;

package RASCAL.Memory is

   subtype mem_adr_type is Address;

   function Allocate (amount : in Integer) return mem_adr_type;
   
   function AllocateFixed (amount : in Integer) return mem_adr_type;
   
   procedure Deallocate (pointer : in mem_adr_type);

   --
   -- Write 'Byte' to address 'Adr' with 'Offset'.
   --
   procedure PutByte (Byte   : in Integer;
                      Adr    : in Address;
                      Offset : in Integer := 0);

   --
   -- Write 'Word' to address 'Adr' with 'Offset'.
   --
   procedure PutWord (Word   : in Integer;
                      Adr    : in Address;
                      Offset : in Integer := 0);

   --
   -- Read byte at address 'Adr' with 'Offset'.
   --
   function GetByte (Adr    : in Address;
                     Offset : in Integer := 0) return Integer;

   --
   -- Read word at address 'Adr' with 'Offset'.
   --
   function GetWord (Adr    : in Address;
                     Offset : in Integer := 0) return Integer;

   --
   -- Read word at address 'Adr' with 'Offset'.
   --
   function GetWordBig (Adr    : in Address;
                        Offset : in Integer := 0) return Integer;

   --
   -- Read string at address 'Adr' with 'Offset' and length 'Amount'.
   --
   function MemoryToString (Adr        : in Address;
                            Offset     : in Integer := 0;
                            Amount     : in Integer) return String;

   --
   -- Read string at address 'Adr' with 'Offset' and end-charactor <='Terminator'
   --
   function MemoryToString (Adr        : in Address;
                            Offset     : in Integer := 0;
                            Terminator : in Character := Character'Val (31))
                            return String;

   --
   -- Read string at address 'Adr' with 'Offset' and end-charactor ='Terminator'
   --
   function Read_String (Adr        : in Address;
                         Offset     : in Integer := 0;
                         Terminator : in Character := Character'Val(31))
                         return String;

   --
   -- Reads string at address 'Adr' with 'Offset' until end of line (ASCII.LF)
   --
   function Get_Line (Adr        : in Address;
                      Offset     : in Integer := 0) return String;

   --
   -- Writes string to address 'Adr' with 'Offset'.
   --
   procedure StringToMemory (Str        : in String;
                             Adr        : in Address;
                             Offset     : in Integer := 0;
                             padlength  : in Integer := 0;
                             Terminator : in Character := Character'Val (0));

   --
   -- Copies memory block from 'Sourceadr' to 'destadr'.
   --
   procedure MemCopy (Sourceadr   : in Address;
                      Destadr     : in Address;
                      Dest_offset : in Integer := 0;
                      Length      : in Integer);

end RASCAL.Memory;
