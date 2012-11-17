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

with Kernel;                    use Kernel;
with System.Storage_Elements;   use System.Storage_Elements;
with System.Unsigned_Types;     use System.Unsigned_Types;
with System.Address_To_Access_Conversions;
with Unchecked_Deallocation;
with Reporter;

with RASCAL.Utility;            use RASCAL.Utility;
with RASCAL.OS;

package body RASCAL.Memory is

   type MemoryBlock;
   type Ptr_MemoryBlock is access MemoryBlock;
   type MemoryBlock is array (Integer range <>) of Integer;

   type bytearray is array(0..3) of Short_Short_Unsigned;
   type halfwordarray is array(0..1) of Short_Short_Unsigned;
   type lonely_byte is array(0..0) of Short_Short_Unsigned;
   type PString is new String(1..1024);
    
   --

   package AddressToStrAccess is new System.Address_To_Access_Conversions
                                        (object => PString); 

   --

   package AddressToIntAccess is new System.Address_To_Access_Conversions
                                        (object => bytearray); 

   --

   package AddressToHWAccess is new System.Address_To_Access_Conversions
                                        (object => halfwordarray); 

   --

   package AddressToByteAccess is new System.Address_To_Access_Conversions
                                        (object => lonely_byte); 

   type MemoryBlockStructure;
   type MemoryBlockStructurePtr is access MemoryBlockStructure;
   type MemoryBlockStructure is
     record
       object_ptr : Ptr_MemoryBlock;
       object_adr : mem_adr_type;
       next       : MemoryBlockStructurePtr := null;
     end record;
   
   Anker,ptr : MemoryBlockStructurePtr;
   firsttime : Boolean := true;
   times : Integer := 0;
   times2: Integer := 0;

   procedure FreeMemoryBlock is new Unchecked_Deallocation
                                (MemoryBlock, Ptr_MemoryBlock);
   
   -- now a hack to address bug in GNAT 3.03
   
   procedure FreeMemoryBlock (Mem : in out Ptr_MemoryBlock) is
   procedure free (Addr: in System.Address);
   pragma Import (C,free);
   use type System.Storage_Elements.Storage_Offset;
   begin
     if Mem /= null then
       free (Mem.all'Address - 8);
       Mem := null;
     end if;  
   end FreeMemoryBlock;
   
   --end hack
   
   procedure FreeListElement is new Unchecked_Deallocation
                                (MemoryBlockStructure, MemoryBlockStructurePtr);
   
   function Allocate (amount : in Integer) return mem_adr_type is
   
   pointer      : Ptr_MemoryBlock;
   returnintptr : mem_adr_type;
   iteration    : Integer := 0;
   byte_amount   : Integer;
   
   begin
     times := times + 1;
   --  Debug.ErrorLog("Allocate Aufruf Nr. "&intstr(times));
     if (amount rem 4) = 0 then
       byte_amount := amount / 4;
     else
       byte_amount := (amount / 4) + 1;
     end if;
     pointer := new MemoryBlock (0..byte_amount-1);
     pointer := new MemoryBlock (0..byte_amount-1);
     returnintptr := pointer(0)'Address;
     if firsttime then
       Anker := new MemoryBlockStructure;
       firsttime := false;
     end if;
     ptr := Anker;
     while ptr.next /= null loop
       ptr := ptr.next;
       iteration := iteration + 1;
   --    Debug.ErrorLog("Iteration: "&intstr(iteration));
     end loop;
     ptr.next := new MemoryBlockStructure;
     ptr := ptr.next;
     ptr.object_ptr := pointer;
     ptr.object_adr := returnintptr;
     return returnintptr;
   end Allocate;
   
   function AllocateFixed (amount : in Integer) return mem_adr_type is
   
   pointer      : Ptr_MemoryBlock;
   byte_amount   : Integer;
   
   begin
     if (amount rem 4) = 0 then
       byte_amount := amount / 4;
     else
       byte_amount := (amount / 4) + 1;
     end if;
     pointer := new MemoryBlock (0..byte_amount-1);
     return pointer(0)'Address;
   end AllocateFixed;
   
   procedure Deallocate (pointer : in mem_adr_type) is
   found : Boolean := false;
   index : Integer := 1;
   prev  : MemoryBlockStructurePtr;
   iteration    : Integer := 0;
   
   begin
     times2 := times2 + 1;
   --  Debug.ErrorLog("Deallocate Aufruf Nr. "&intstr(times2));
     ptr := Anker;
     while not found and ptr.next /= null loop
       prev := ptr;
       ptr := ptr.next;
       iteration := iteration + 1;
   --    Debug.ErrorLog("Iteration: "&intstr(iteration));
       if ptr.object_adr = pointer then
         found := true;
       end if;
     end loop;
     if found then
       prev.next := ptr.next;
   --    Debug.ErrorLog("Freeing memory...");
   --    Debug.ErrorLog("Ptr is: " & intstr(Integer(ptr.object_adr)));
       FreeMemoryBlock(ptr.object_ptr);
   --    Debug.ErrorLog("Freeing structure...");
       FreeListElement(ptr);
     end if;
   end Deallocate;

   --
   
   function GetByte (Adr    : in Address;
                     Offset : in Integer := 0) return Integer is
   
      Ptr      : AddressToByteAccess.Object_Pointer;
      realword : System.Unsigned_Types.Unsigned := 0;
   
   begin
     Ptr := AddressToByteAccess.To_Pointer(Adr+Storage_Offset(Offset));
     realword := System.Unsigned_Types.Unsigned(Ptr(0));
     return Unsigned_To_Int(realword);
   exception
     when others => pragma Debug(Reporter.Report("Exception raised in Memory.GetByte"));
                    raise;
   end GetByte;
   
   --
   
   function GetWordBig (Adr    : in Address;
                     Offset : in Integer := 0) return Integer is
   
      Ptr      : AddressToIntAccess.Object_Pointer;
      realword : System.Unsigned_Types.Unsigned := 0;
   
   begin
   
     Ptr := AddressToIntAccess.To_Pointer(Adr+Storage_Offset(Offset));
     realword := System.Unsigned_Types.Unsigned(Ptr(3)) + Shift_Left(System.Unsigned_Types.Unsigned(Ptr(2)),8)
                 + Shift_Left(System.Unsigned_Types.Unsigned(Ptr(1)),16)
                 + Shift_Left(System.Unsigned_Types.Unsigned(Ptr(0)),24);
     return Unsigned_To_Int(realword);

   exception
     when others => pragma Debug(Reporter.Report("Exception raised in Memory.GetWordBig"));
                    raise;
   end GetWordBig;
   
   --
   
   function GetWord (Adr    : in Address;
                     Offset : in Integer := 0) return Integer is
   
      Ptr : AddressToIntAccess.Object_Pointer;
      realword : Unsigned := 0;
   
   begin

     Ptr := AddressToIntAccess.To_Pointer(Adr+Storage_Offset(Offset));
     realword := Unsigned(Ptr(0)) + Shift_Left(Unsigned(Ptr(1)),8)
                 + Shift_Left(Unsigned(Ptr(2)),16)
                 + Shift_Left(Unsigned(Ptr(3)),24);
     return Unsigned_To_Int(realword);

   exception
     when others => pragma Debug(Reporter.Report("Exception raised in Memory.GetWord"));
                    raise;
   end GetWord;
   
   --
   
   procedure PutByte (Byte   : in Integer;
                      Adr    : in Address;
                      Offset : in Integer := 0) is
   
      Ptr : AddressToByteAccess.Object_Pointer;
      realword : Unsigned;
   
   begin

     Ptr := AddressToByteAccess.To_Pointer(Adr+Storage_Offset(Offset));
     realword := Int_To_Unsigned (Byte);
     Ptr(0) :=  Short_Short_Unsigned(realword and 16#000000FF#);

   exception
     when others => pragma Debug(Reporter.Report("Exception raised in Memory.PutByte!"));
                    raise;
   end PutByte;
   
   --
   
   procedure PutWord (Word   : in Integer;
                      Adr    : in Address;
                      Offset : in Integer := 0) is
   
      Ptr : AddressToIntAccess.Object_Pointer;
      realword : Unsigned;
   
   begin

     Ptr := AddressToIntAccess.To_Pointer(Adr+Storage_Offset(Offset));
     realword := Int_To_Unsigned (Word);
     Ptr(0) := Short_Short_Unsigned(realword and 16#000000FF#);
     Ptr(1) := Short_Short_Unsigned(Shift_Right(realword and 16#0000FF00#,8));
     Ptr(2) := Short_Short_Unsigned(Shift_Right(realword and 16#00FF0000#,16));
     Ptr(3) := Short_Short_Unsigned(Shift_Right(realword and 16#FF000000#,24));

   exception
     when others => pragma Debug(Reporter.Report("Exception raised in Memory.PutWord!"));
                    raise;
   end PutWord;
   
   --
   
   function MemoryToString (Adr        : in Address;
                            Offset     : in Integer := 0;
                            Amount     : in Integer) return String is
                             
   
   Str : String(1..1024);
   i   : Integer := 1;
   j   : Integer := 1;
   Ptr : AddressToStrAccess.Object_Pointer;
   Chr : Character;
   
   begin
     Ptr := AddressToStrAccess.To_Pointer(Adr+Storage_Offset(Offset));
     i := Ptr'First;
     Chr := Ptr(i);
     while j <= Amount loop
       Str(j) := Chr;
       j := j + 1;
       i := i + 1;
       Chr := Ptr(i);    
     end loop;
     return Str(1..j-1);
   exception
     when others => pragma Debug(Reporter.Report("Exception raised in Memory.MemoryToString!"));
                    raise;
   end MemoryToString;

   --

   function MemoryToString (Adr        : in Address;
                            Offset     : in Integer := 0;
                            Terminator : in Character := Character'Val(31))
                            return String is
   
      Str : String(1..1024);
      i   : Integer := 1;
      j   : Integer := 1;
      Ptr : AddressToStrAccess.Object_Pointer;
      Chr : Character;
   begin
     Ptr := AddressToStrAccess.To_Pointer(Adr+Storage_Offset(Offset));
     i := Ptr'First;
     Chr := Ptr(i);
     while Chr > Terminator loop
       Str(j) := Chr;
       j := j + 1;
       i := i + 1;
       Chr := Ptr(i);
     end loop;
     return Str(1..j-1);

   exception
     when others => pragma Debug(Reporter.Report("Exception raised in Memory.MemoryToString!"));
                    raise;
   end MemoryToString;

   --

   function Read_String (Adr        : in Address;
                         Offset     : in Integer := 0;
                         Terminator : in Character := Character'Val(31))
                         return String is
   
      Str : String(1..1024);
      i   : Integer := 1;
      j   : Integer := 1;
      Ptr : AddressToStrAccess.Object_Pointer;
      Chr : Character;
   begin
     Ptr := AddressToStrAccess.To_Pointer(Adr+Storage_Offset(Offset));
     i := Ptr'First;
     Chr := Ptr(i);

     while Chr /= Terminator loop
       Str(j) := Chr;
       j := j + 1;
       i := i + 1;
       Chr := Ptr(i);
     end loop;
     return Str(1..j-1);
   exception
     when others => pragma Debug(Reporter.Report("Exception raised in Memory.MemoryToString!"));
                    raise;
   end Read_String;

   --

   function Get_Line (Adr        : in Address;
                      Offset     : in Integer := 0) return String is
   
      Str : String(1..1024);
      i   : Integer := 1;
      j   : Integer := 1;
      Ptr : AddressToStrAccess.Object_Pointer;
      Chr : Character;
   begin

     Ptr := AddressToStrAccess.To_Pointer(Adr+Storage_Offset(Offset));
     i := Ptr'First;
     Chr := Ptr(i);
     
     while Chr /= ASCII.LF loop
       Str(j) := Chr;
       j := j + 1;
       i := i + 1;
       Chr := Ptr(i);    
     end loop;
     return Str(1..j-1);

   exception
     when others => pragma Debug(Reporter.Report("Exception raised in Memory.Get_line"));
                    raise;
   end Get_Line;

   --

   procedure StringToMemory (Str        : in String;
                             Adr        : in Address;
                             Offset     : in Integer := 0;
                             padlength  : in Integer := 0;
                             Terminator : in Character := Character'Val(0)) is
   
      Ptr : AddressToStrAccess.Object_Pointer;
      i,j : Integer;
   
   begin

     Ptr := AddressToStrAccess.To_Pointer(Adr+Storage_Offset(Offset));
     i := Str'First;
     j := 1;

     while i <= Str'Last loop
       Ptr(j) := Str(i);
       i := i + 1;
       j := j + 1;
     end loop;

     Ptr(j) := Terminator;
     for j in Str'Length+1..padlength loop
       Ptr(j) := Terminator;
     end loop;

   exception
     when others => pragma Debug(Reporter.Report("Exception raised in Memory.StringToMemory!"));
                    raise;
   end StringToMemory;
   
   --

   procedure MemCopy (Sourceadr : in Address;
                      Destadr   : in Address;
                      Dest_offset : in Integer := 0;
                      Length    : in Integer) is
   begin
     for i in 0..Length-1 loop
       PutByte (GetByte(Sourceadr,i),Destadr,i+Dest_offset);
     end loop;
   end MemCopy;

   --
   
end RASCAL.Memory;
