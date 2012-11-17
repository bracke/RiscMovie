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

with RASCAL.Utility;   use RASCAL.Utility;
with RASCAL.Memory;    use RASCAL.Memory;
with RASCAL.MessageTrans;  use RASCAL.MessageTrans;
with RASCAL.OS;            use RASCAL.OS;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C;         use Interfaces.C;

with Reporter;

package body RASCAL.Heap is

   --

   procedure Set_Name (Name : in String) is
   begin
      if not Initialised then
         if Name'Length > 32 then
            Heap_Name(1..32) := Name(Name'First..Name'First+31);
         else
            Heap_Name(1..Name'Length) := Name;
         end if;
      end if;
   end Set_Name;

   --

   procedure Set_MaxSize (Extent : in Natural :=0) is
   begin
      if not Initialised then
         DA_Max_Size := Extent;
      end if;
   end Set_MaxSize;

   --

   procedure Set_MessagesFile (Messages : in Messages_Handle_Type) is
   begin
      Errors := Messages;
   end Set_MessagesFile;

   --

   function Get_Address (The : in Heap_Block_Type) return Address is
   begin
      return The.Anchor;
   end Get_Address;

   --

   function Get_Size (The : in Heap_Block_Type) return Integer is
   begin
      return Get_Size (The.Anchor'Address);
   end Get_Size;

   --

   procedure Extend (The       : in out Heap_Block_Type;
                     New_Size  : in Natural) is

      Result : Integer;
   begin
      Result := Extend (The.Anchor'Address,New_Size);
      if Result = 1 then
         -- 0 = failure, 1 = success.
         The.Extent := New_Size;
      else
         raise Unable_To_Extend_Block;
      end if;
   end Extend;

   --

   procedure Mid_Extend (The      : in out Heap_Block_Type;
                         Location : in Integer;
                         Extent   : in Integer) is

      Result : Integer;
   begin
      Result := Mid_Extend (The.Anchor'Address,Location,Extent);
      if Result = 1 then
         -- 0 == failure, 1 == success
         The.Extent := Get_Size (The);
      else
         raise Unable_To_Extend_Block;
      end if;
   end Mid_Extend;

   --

   procedure Re_Anchor (The : in out Heap_Block_Type;
                        To  : in out Heap_Block_Type) is
      Result : Integer;
   begin
      Result := Re_Anchor (To.Anchor'Address,The.Anchor'Address);
      The.Extent := 0;
      To.Extent  := Get_Size (To);
   end Re_Anchor;

   --

   procedure Set_Budge (Budge : in Boolean) is

      Result    : Integer;
      New_State : Integer := 0;
   begin
      if Budge then
         New_State := 1;
      end if;
      Result := Set_Budge (New_State);
   end Set_Budge;

   --

   procedure Save_HeapInfo (Filename : in String) is

   begin
      Save_FlexHeapInfo (Filename);
   end Save_HeapInfo;

   --

   function Compact return Boolean is

      Result : Integer;
   begin
      Result := Compact;
      return Result = 0;
   end Compact;

   --

   procedure Set_Deferred_Compaction (The   : in out Heap_Block_Type;
                                      Defer : in Boolean) is
      Result    : Integer;
      New_State : Integer := 0;
   begin
      if Defer then
         New_State := 1;
      end if;
      Result := Set_Deferred_Compaction(New_State);
   end Set_Deferred_Compaction;

   --

   procedure Free (The : in out Heap_Block_Type) is
   begin
      if Adr_To_Integer (The.Anchor) /= 0 then         
         Free (The.Anchor'Address);
      end if;
   end Free;

   --

   procedure Initialize(The : in out Heap_Block_Type) is

      Result : Integer;
   begin
      -- Initialise heap if necessary
      if not Initialised then
         Init (Heap_Name,Errors,DA_Max_Size);
         Set_Budge(true);
         Initialised := true;
      end if;
      -- Allocate block
      Result := Alloc (The.Anchor'Address,The.Extent);
      if Result = 0 then
         -- 0 = failure, 1 = success
         raise Unable_To_Allocate_Block;
      end if;
      if Get_Size (The) < The.Extent then
         Extend (The,The.Extent);
      end if;
   end Initialize;

   --

   procedure Adjust(The : in out Heap_Block_Type) is
   begin
      null;
   end Adjust;

   --

   procedure Finalize(The : in out Heap_Block_Type) is
   begin
      if Adr_To_Integer (The.Anchor) /= 0 then
         Free (The.Anchor'Address);
      end if;
   end Finalize;

   --

   --

   function flex_alloc (anchor : flex_ptr; n : int) return int;

   pragma Import (C,flex_alloc);

   function Alloc (Anchor : in Flex_Ptr;
                   Bytes  : in Integer) return Integer is

      C_Bytes : Int;
      C_Return: Int;
   begin
      C_Bytes  := Int(Bytes);
      C_Return := flex_alloc (Anchor,C_Bytes);
      return Integer(C_Return);
   end Alloc;

   --

   procedure flex_free (anchor : flex_ptr);

   pragma Import (C,flex_free);

   procedure Free (Anchor : in Flex_Ptr) is
   begin
      flex_free(Anchor);
   end Free;

   --

   function flex_size (anchor : flex_ptr) return int;

   pragma Import (C,flex_size);

   function Get_Size (Anchor : in Flex_Ptr) return Integer is

      C_Return : Int;
   begin
      C_Return := flex_size (Anchor);
      return Integer(C_Return);
   end Get_Size;

   --

   function flex_extend (anchor : flex_ptr; newsize : int) return int;

   pragma Import (C,flex_extend);

   function Extend (Anchor   : in Flex_Ptr;
                    New_Size : in Integer) return Integer is

      C_New_Size : Int;
      C_Result   : Int;
   begin
      C_New_Size := Int(New_Size);
      C_Result   := flex_extend (Anchor,C_New_Size);
      return Integer(C_Result);
   end Extend;

   --

   function flex_midextend (anchor : flex_ptr; att : int; by : int) return int;

   pragma Import (C,flex_midextend);

   function Mid_Extend (Anchor   : in Flex_Ptr;
                        Location : in Integer;
                        Extent   : in Integer) return Integer is

      C_Location : Int;
      C_Extent   : Int;
      C_Return   : Int;
   begin
      C_Location := Int(Location);
      C_Extent   := Int(Extent);
      C_Return   := flex_midextend (Anchor, C_Location, C_Extent);
      return Integer(C_Return);
   end Mid_Extend;

   --

   function flex_reanchor (to : flex_ptr; from : flex_ptr) return int;

   pragma Import (C,flex_reanchor);

   function Re_Anchor (To   : in Flex_Ptr;
                       From : in Flex_Ptr) return Integer is
   
      C_Return : Int;
   begin
      C_Return := flex_reanchor (To,From);
      return Integer(C_Return);
   end Re_Anchor;

   --

   function flex_set_budge (newstate : int) return int;

   pragma Import (C,flex_set_budge);

   function Set_Budge (New_State : Integer) return Integer is

      C_New_State : Int;
      C_Previous_State : Int;
   begin
      C_New_State := Int(New_State);
      C_Previous_State := flex_set_budge (C_New_State);
      return Integer(C_Previous_State);
   end Set_Budge;

   --

   procedure flex_init(program_name : chars_ptr ; errors_fd : int; dynamic_size : int);
   
   pragma Import (C,flex_init);

   procedure Init (Program_Name : in String;
                   Errors       : in Messages_Handle_Type;
                   Max_DA_Size  : in Integer := 0) is

      C_Program_Name : Chars_Ptr;
      C_Max_DA_Size  : Int;
   begin
      C_Max_DA_Size  := Int(Max_DA_Size);
      C_Program_Name := New_String (Program_Name);
      if Errors = null then
         flex_init (C_Program_Name,0,C_Max_DA_Size);
      else
         flex_init (C_Program_Name,Adr_To_Int(Errors.all'Address),C_Max_DA_Size);
      end if;
      Free (C_Program_Name);
   end Init;

   --

   procedure flex_save_heap_info (filename : chars_ptr);

   pragma Import (C,flex_save_heap_info);

   procedure Save_FlexHeapInfo (Filename : in String) is

      C_Filename : Chars_Ptr;
   begin
      C_Filename := New_String (Filename);
      flex_save_heap_info (C_Filename);
      Free (C_Filename);
   end Save_FlexHeapInfo;

   --

   function flex_compact return int;

   pragma Import (C,flex_compact);

   function Compact return Integer is

      C_Int : Int;
   begin
      C_Int := flex_compact;
      return Integer(C_Int);
   end Compact;

   --

   function flex_set_deferred_compaction (newstate : int) return int;

   pragma Import (C,flex_set_deferred_compaction);

   function Set_Deferred_Compaction (New_State : Integer) return Integer is

      C_New_State : Int;
      C_Previous_State : Int;
   begin
      C_New_State := Int (New_State);
      C_Previous_State := flex_set_deferred_compaction (C_New_State);
      return Integer(C_Previous_State);
   end Set_Deferred_Compaction;

   --

end RASCAL.Heap;
