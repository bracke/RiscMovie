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

with RASCAL.OS;

with Ada.Strings;              use Ada.Strings;
with Kernel;                   use Kernel;
with System.Storage_Elements;  use System.Storage_Elements;
with Interfaces.C;             use Interfaces.C;
with Reporter;

package body RASCAL.FileInternal is

   -- Constants
   OS_Find      : constant := 16#0D#;
   OS_BGet      : constant := 16#0A#;
   OS_BPut      : constant := 16#0B#;
   OS_Args      : constant := 16#09#;
   OS_File      : constant := 16#08#;
   OS_FSControl : constant := 16#29#;
   OS_GBPB      : constant := 16#0C#;

   -- Methods

   procedure Skip_EmptyLines (File : in FileHandle_Type) is
      str   : Unbounded_String;
      Start : Integer;
   begin
      start := FileInternal.Get_Ptr(File);
      str   := U(Read_Line (File,true));
      loop
         exit when Length(str)>0;
         exit when Is_EOF (File);
         start := FileInternal.Get_Ptr(File);
         str   := U(Read_Line (File,true));
      end loop;
      if Length(str)>0 then
         FileInternal.Set_Ptr(File,Start);
      end if;
   end Skip_EmptyLines;

   --

   procedure Close (File : in FileHandle_Type) is
      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;   begin
      Register.R(0) := 0;
      Register.R(1) := int(File.Handle);
      Error := Kernel.swi (OS_Find,Register'access,Register'access);
   end Close;

   --

   procedure Goto_End (File : in FileHandle_Type) is
   begin
      Set_Ptr(File,Get_Extent(File));
   end Goto_End;

   --

   function Is_EOF (File : in FileHandle_Type) return boolean is
      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin      Register.R(0) := 5;
      Register.R(1) := int(File.Handle);
      Error := Kernel.SWI (OS_Args,Register'access,Register'access);
      if Error /= null then
         pragma Debug(Reporter.Report("FileInternal.Is_EOF: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      if Register.R(2) = 0 then
        return false;
      else
        return true;
      end if;
   end Is_EOF;

   --

   function Get_Extent (File : in FileHandle_Type) return natural is
      Register  : aliased Kernel.swi_regs;
      Error     : oserror_access;
   begin
      Register.R(0) := 2;
      Register.R(1) := int(File.Handle);
      Error := Kernel.SWI (OS_Args,Register'access,Register'access);
      if Error /= null then
         pragma Debug(Reporter.Report("FileInternal.Get_Extent: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return natural(Register.R(2));
   end Get_Extent;

   --

   procedure Goto_Start (File : in FileHandle_Type) is
   begin
      Set_Ptr(File,0);
   end Goto_Start;

   --

   function Get_Ptr (File : in FileHandle_Type) return Integer is
      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := 0;
      Register.R(1) := int(File.Handle);
      Error := Kernel.SWI (OS_Args,Register'access,Register'access);
      if Error /= null then
         pragma Debug(Reporter.Report("FileInternal.Get_Ptr: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Integer(Register.R(2));
   end Get_Ptr;

   --

   procedure Set_Ptr (File : in FileHandle_Type;
                      Ptr  : in integer) is
      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
     Register.R(0) := 1;
     Register.R(1) := int(File.Handle);
     Register.R(2) := int(Ptr);
     Error := Kernel.SWI (OS_Args,Register'access,Register'access);
     if Error /= null then
        pragma Debug(Reporter.Report("FileInternal.Set_Ptr: " & To_Ada(Error.ErrMess)));
        OS.Raise_Error(Error);
     end if;
   end Set_Ptr;

   --

   function Get_Byte (File : in FileHandle_Type) return Integer is
      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      if Is_EOF(File) then
         return -1;
      else
         Register.R(1) := int(File.Handle);
         Error := Kernel.SWI (OS_BGet,Register'access,Register'access);
         if Error /= null then
            pragma Debug(Reporter.Report("FileInternal.Get_Byte: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return Integer(Register.R(0));
      end if;
   end Get_Byte;

   --

   procedure Put_Byte (File : in FileHandle_Type;
                       Byte : in Integer) is
      Register  : aliased Kernel.swi_regs;
      Error     : oserror_access;
   begin
      Register.R(0) := int(Byte);
      Register.R(1) := int(File.Handle);
      Error := Kernel.SWI (OS_BPut,Register'access,Register'access);
      if Error /= null then
         pragma Debug(Reporter.Report("FileInternal.Put_Byte: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Put_Byte;

   --

   procedure Skip_Bytes (File : in FileHandle_Type;
                         Nr   : in Integer) is
      Current_ptr : Integer;
      Register    : aliased Kernel.swi_regs;
      Error       : oserror_access;
   begin
      Current_ptr := Get_Ptr(File);
      Register.R(0) := 1;
      Register.R(1) := int(File.Handle);
      Register.R(2) := int(Current_ptr + Nr);
      Error := Kernel.SWI (OS_Args,Register'access,Register'access);
      if Error /= null then
         pragma Debug(Reporter.Report("FileInternal.Skip_Bytes: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Skip_Bytes;

   --

   procedure Put_String (File      : in FileHandle_Type;
                         Line      : in String;
                         Attach_LF : in Boolean := true) is

      Register  : aliased Kernel.swi_regs;
      Error     : oserror_access;
      Line_0    : String := Line & Character'Val(10);
   begin
      Register.R(0) := 2;
      Register.R(1) := int(File.Handle);
      Register.R(2) := Adr_To_Int(Line_0'Address);
      if Attach_LF then
         Register.R(3) := int(Line_0'length);
      else
         Register.R(3) := int(Line_0'length-1);
      end if;
      Error := Kernel.swi (OS_GBPB,register'Access,register'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("FileInternal.Put_String: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Put_String;

   --

   function Get_Lines (File : in FileHandle_Type;
                       Trim : in boolean := true) return integer is
      Lines : integer            := 0;
      Point : integer            := 0;
      Str   : Unbounded_String;
   begin
      Goto_Start(File);
      Point := Get_Ptr(File);
      while not Is_EOF(File) loop
         Str := U(Read_Line(File));
         if Trim then
            Ada.Strings.Unbounded.Trim(Str,Both);
         end if;
         if Length(Str)>0 then
            Lines := Lines + 1;
         end if;
      end loop;
      Set_Ptr(File,Point);
      return Lines;
   end Get_Lines;

   --

   function Read_Line (File : in FileHandle_Type;
                       Trim : in boolean := false) return String is
      Register : aliased Kernel.SWI_regs;
      Error    : oserror_access;
      C        : character;
      Line     : UString := U("");
      Index    : integer := 0;
   begin
      Read_Until_EOL:
      loop
         Register.R(0) := 4;
         Register.R(1) := int(File.Handle);
         Register.R(2) := Adr_To_Int(C'address);
         Register.R(3) := 1;
         Register.R(4) := int(Index);
         Error := Kernel.SWI (OS_GBPB,Register'access,Register'access);
         if Error /= null then
            pragma Debug(Reporter.Report("FileInternal.Read_Line: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         if Register.R(3) /= 0 then
            exit Read_Until_EOL;
         end if;
         if c /=Character'Val(10) then
            Line := Line & C;
         else
            exit Read_Until_EOL;
         end if;
         index := Integer(Register.R(4));
      end loop Read_Until_EOL;
      if Trim then
         Ada.Strings.Unbounded.Trim(Line,both);
      end if;
      return S(Line);
   end Read_Line;

   --

   procedure Ensure_Size (File : in FileHandle_Type;
                          Size : in Integer) is
      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := 6;
      Register.R(1) := int(File.Handle);
      Register.R(2) := int(Size);
      Error := Kernel.SWI (OS_Args,Register'access,Register'access);
      if Error /= null then
         pragma Debug(Reporter.Report("FileInternal.Ensure_Size: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Ensure_Size;

   --

   procedure Load_File (FileName : in String;
                        Buffer   : in Address) is

      Register   : aliased Kernel.swi_regs;
      Error      : oserror_access;
      FileName_0 : string := FileName & ASCII.NUL;
   begin
      Register.R(0) := 16;
      Register.R(1) := Adr_To_Int(FileName_0'address);
      Register.R(2) := Adr_To_Int(Buffer);
      Register.R(3) := 0;
      Error := Kernel.SWI (OS_File,Register'access,Register'access);
      if Error /= null then
         pragma Debug(Reporter.Report("FileInternal.Load_File: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Load_File;

   --

   procedure Save_File (FileName  : in String;
                        Buffer    : in Address;
                        Bufferend : in Address;
                        FileType  : in Integer) is

      Register     : aliased Kernel.swi_regs;
      Error        : oserror_access;
      FileName_0   : String := FileName & ASCII.NUL;
   begin
      Register.R(0) := 10;
      Register.R(1) := Adr_To_Int(FileName_0'address);
      Register.R(2) := int(FileType);
      Register.R(3) := 0;
      Register.R(4) := Adr_To_Int(Buffer);
      Register.R(5) := Adr_To_Int(Bufferend);
      Error := Kernel.SWI (OS_File,Register'access,Register'access);
      if Error /= null then
         pragma Debug(Reporter.Report("FileInternal.Save_File: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Save_File;

   --

   procedure Get_Bytes (File   : in FileHandle_Type;
                        Buffer : in Address;
                        Length : in Integer) is
      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := 4;
      Register.R(1) := int(File.Handle);
      Register.R(2) := Adr_To_Int(Buffer);
      Register.R(3) := int(Length);
      Error := Kernel.SWI (OS_GBPB,register'Access,register'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("FileInternal.Get_Bytes: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Get_Bytes;

   --

   procedure Put_Bytes (File   : in FileHandle_Type;
                        Buffer : in Address;
                        Length : in Integer) is
      Register  : aliased Kernel.swi_regs;
      Error     : oserror_access;
   begin
      Register.R(0) := 2;
      Register.R(1) := int(File.Handle);
      Register.R(2) := Adr_To_Int(Buffer);
      Register.R(3) := int(Length);
      Error := Kernel.swi (OS_GBPB,Register'access,Register'access);
      if Error /= null then
         pragma Debug(Reporter.Report("FileInternal.Put_Bytes: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Put_Bytes;

   --

   function Get_Real_FileHandle (File : in FileHandle_Type) return Real_FileHandle_Type is   begin
      return File.Handle;
   end Get_Real_FileHandle;

   -- Private methods

   function OpenIn (FileName : in String) return Real_FileHandle_Type is

      Error      : oserror_access;
      Register   : aliased Kernel.swi_regs;
      Reasoncode : constant := 16#4B#;
      FileName_0 : String   := FileName & ASCII.NUL;
   begin
      Register.R(0) := Reasoncode;
      Register.R(1) := int(To_Integer(FileName_0'address));
      Error := Kernel.swi (OS_Find,register'Access,register'Access);
      if Error /= NULL then
         pragma Debug(Reporter.Report("FileInternal.OpenIn: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      else
         return Real_FileHandle_Type(Register.R(0));
      end if;
   end OpenIn;

   --

   function OpenOut (FileName : in String) return Real_FileHandle_Type is
      Error      : oserror_access;
      Register   : aliased Kernel.swi_regs;
      Reasoncode : constant := 16#8B#;
      FileName_0 : string   := FileName & ASCII.NUL;
   begin
     Register.R(0) := Reasoncode;
     Register.R(1) := Adr_To_Int(FileName_0'address);
     Error := Kernel.SWI (OS_Find,Register'access,Register'access);
     if Error /= null then
        pragma Debug(Reporter.Report("FileInternal.OpenOut: " & To_Ada(Error.ErrMess)));
        OS.Raise_Error(Error);
     else
        return Real_FileHandle_Type(Register.R(0));
     end if;
   end OpenOut;

   --

   function OpenUp (FileName : in String) return Real_FileHandle_Type is

      Error      : oserror_access;
      Register   : aliased Kernel.swi_regs;
      Reasoncode : constant := 16#CB#;
      FileName_0 : string   := FileName & ASCII.NUL;
   begin
      Register.R(0) := Reasoncode;
      Register.R(1) := Adr_To_Int(FileName_0'address);
      Error := Kernel.SWI (OS_Find,Register'access,Register'access);
      if Error /= NULL then
         return OpenOut (FileName);
      else
         return Real_FileHandle_Type(Register.R(0));
      end if;
   end OpenUp;

   -- Finalization methods

   procedure Initialize (The : in out FileHandle_Type) is
   begin
      case The.Access_Type is
      when Read         => The.Handle := OpenIn(S(The.Path));
      when Write        => The.Handle := OpenOut(S(The.Path));
      when ReadWrite    => The.Handle := OpenUp(S(The.Path));
      end case;
   end Initialize;

   --

   procedure Finalize (The : in out FileHandle_Type) is
   begin
      if The.Handle /= -1 then
         Close (The);
         The.Handle := -1;
      end if;
   end Finalize;

   --

end RASCAL.FileInternal;
