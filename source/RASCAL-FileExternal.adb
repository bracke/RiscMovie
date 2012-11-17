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

with Kernel;                  use Kernel;
with Interfaces.C;            use Interfaces.C;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Reporter;

with RASCAL.Memory;           use RASCAL.Memory;
with RASCAL.Utility;          use RASCAL.Utility;
with RASCAL.OS;               use RASCAL.OS;
with RASCAL.FileName;

package body RASCAL.FileExternal is

   OS_File      : constant := 16#08#;
   OS_FSControl : constant := 16#29#;
   OS_GBPB      : constant := 16#0C#;
   OS_Find      : constant := 16#0D#;
   OS_Args      : constant := 16#09#;

   --

   procedure Close_AllInPath (path : in String)is

      buffer   : String(1..1024);
      register : aliased Kernel.swi_regs;
      carry    : aliased int;
      handle   : int                      := 255;
      index    : integer                  := 0;
      str      : unbounded_string;
      Error    : oserror_access;
   begin
      while handle >= 0 loop
         Register.r(0) := 7;
         Register.r(1) := handle;
         Register.r(2) := int(To_Integer(buffer'Address));
         Register.r(5) := 1024;
         Error := Kernel.swi_c (OS_Args,register'Access,register'Access,carry'access);

         if carry /= 0 then
            str := U(Memory.MemoryToString(buffer'Address));
            index := Ada.Strings.Fixed.
                     index(S(str),path);
            if index > 0 then
               register.r(0) := 0;
               register.r(1) := handle;
               Error := Kernel.SWI (OS_Find,register'Access,register'Access);
            end if;
         end if;
         handle := handle - 1;
      end loop ;
   end Close_AllInPath;

   --

   function Get_UnUsed_FileName (Path : in String) return String is

      dummynr   : Positive          := 1;
      dummypath : Unbounded_String;
   begin
      dummypath := U(Path & "." & intstr(dummynr));
      while Exists(S(dummypath)) loop
         dummynr   := dummynr + 1;
         dummypath := U(Path & "." & intstr(dummynr));
      end loop;
      return intstr(dummynr);
   end Get_UnUsed_FileName;

   --

   function Exists (Filename : in string) return boolean is

      Filename_0 : string := Filename & ASCII.NUL;
      Error      : OSError_Access;
      Register   : aliased Kernel.SWI_Regs;
   begin
      Register.R(0) := 23;
      Register.R(1) := int(To_Integer(Filename_0'Address));
      Error := Kernel.swi(OS_File,Register'Access,Register'Access);

      if Error /= null then
        return false;
      end if;
      if Register.R(0) = 0 then
        return false;
      else
        return true;
      end if;

   end Exists;

   --

   function Is_Valid (Path     : in String;
                      FileSize : in Natural) return Boolean is

      Register   : aliased Kernel.swi_regs;
      Error      : oserror_access;
      FileName_0 : String := Path & ASCII.NUL;
   begin
      Register.R(0) := 11;
      Register.R(1) := Adr_To_Int(FileName_0'Address);
      Register.R(2) := 16#fff#;
      --Register.R(3) := 0;
      Register.R(4) := 0;
      Register.R(5) := Int(FileSize);

      Error := Kernel.SWI (OS_File,Register'access,Register'access);

      if Error /= null then
         pragma Debug(Reporter.Report("FileExternal.Is_Valid: " & To_Ada(Error.ErrMess)));
         return false;
      end if;
      Delete_File (Path);
      return true;
   end Is_Valid;

   --

   procedure Delete_File (Filename : in string) is

      Filename_0 : string := Filename & ASCII.NUL;
      Register   : aliased Kernel.swi_regs;
      Error      : oserror_access;
   begin
      Register.R(0) := 6;
      Register.R(1) := Adr_To_Int(Filename_0'address);
      Error := Kernel.SWI (OS_File,Register'access,Register'access);

      if Error /= null then
         pragma Debug(Reporter.Report("FileExternal.Delete_File: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Delete_File;

   --

   procedure Rename (Source : in string;
                     Target : in string) is

      Source_0 : string := Source & ASCII.NUL;
      Target_0 : string := Target & ASCII.NUL;
      Register : aliased Kernel.SWI_Regs;
      Error    : OSError_Access;
   begin
      Register.R(0) := 25;
      Register.R(1) := Adr_To_Int(Source_0'address);
      Register.R(2) := Adr_To_Int(Target_0'address);
      Error := Kernel.SWI (OS_FSControl,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("FileExternal.Rename: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Rename;

   --

   procedure Copy (Source : in string; Target : in string;
                   Flags  : in System.Unsigned_Types.Unsigned := 0) is

      Source_0 : string := Source & ASCII.NUL;
      Target_0 : string := Target & ASCII.NUL;
      Register : aliased Kernel.SWI_Regs;
      Error    : OSError_Access;
   begin
      Register.R(0) := 26;
      Register.R(1) := Adr_To_Int(Source_0'address);
      Register.R(2) := Adr_To_Int(Target_0'address);
      Register.R(3) := int(Unsigned_to_Int(Flags));
      Error := Kernel.SWI (OS_FSControl,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("FileExternal.Copy: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Copy;

   --

   procedure Move (Source : in string; Target : in string;
                   Flags  : in System.Unsigned_Types.Unsigned := Copy_Option_Delete) is
   begin
      Copy (Source,Target,Flags);
   end Move;

   --

   procedure Wipe (Path : in string) is

      Path_0   : string := Path & ASCII.NUL;
      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(27);
      Register.R(1) := Adr_To_Int(Path_0'address);
      Register.R(2) := 0;
      Register.R(3) := int(1+2);
      Register.R(4) := int(0);
      Register.R(5) := int(0);
      Register.R(6) := int(0);
      Register.R(7) := int(0);
      Register.R(8) := int(0);

      Error := Kernel.swi (OS_FSControl,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("FileExternal.Wipe: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Wipe;

   --

   procedure Create_File (Filename : in string;
                          Length   : in integer := 0;
                          Filetype : in integer := 16#FFD#) is

      Filename_0 : string := Filename & ASCII.NUL;
      Register   : aliased Kernel.swi_regs;
      Error      : oserror_access;
   begin
      Register.R(0) := 11;
      Register.R(1) := Adr_To_Int(Filename_0'address);
      Register.R(2) := int(Filetype);
      Register.R(4) := 0;
      Register.R(5) := int(Length);
      Error := Kernel.SWI (OS_File,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("FileExternal.Create_File: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Create_File;

   --

   procedure Create_Directory (Dirname : in string) is

      Dirname_0 : string := Dirname & ASCII.NUL;
      Register  : aliased Kernel.swi_regs;
      Error     : oserror_access;
   begin
      Register.R(0) := 8;
      Register.R(1) := Adr_To_Int(Dirname_0'address);
      Register.R(4) := 0;
      Error := Kernel.SWI (OS_File,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("FileExternal.Create_Directory: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Create_Directory;

   --

   function Get_FileNames (Path : in string) return string is

      index : integer := 0;
      name  : unbounded_string := U("");
      names : unbounded_string := U("");
   begin
      while index /= -1 loop
         name := U("");
         Read_Dir(Path,index,name);
         if Length(name) > 0 then
            if Length(names) > 0 then
               names := names & ",";
            end if;
            names := names & name;
         end if;
      end loop;
      return S(names);

   end Get_FileNames;

   --

   procedure Read_Dir (Path  : in  string;
                       Index : in out integer;
                       Name  : out Unbounded_String) is

      Path_0   : string := Path & ASCII.NUL;
      Name_0   : string := "*" & ASCII.NUL;
      Buffer   : array(0..63) of integer;
      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
      Nr       : integer;
   begin
      loop
         Register.R(0) := 11;
         Register.R(1) := Adr_To_Int(Path_0'Address);
         Register.R(2) := Adr_To_Int(Buffer'Address);
         Register.R(3) := 1;
         Register.R(4) := int(Index);
         Register.R(5) := 255;
         Register.R(6) := Adr_To_Int(Name_0'Address);
         Error := Kernel.SWI (OS_GBPB,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("FileExternal.Read_Dir: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;

         Index := integer(Register.R(4));
         Nr    := integer(Register.R(3));
         exit when (Index = -1 or Nr = 1);
      end loop;

      if Nr = 1 then
         Name := To_Unbounded_String(Memory.MemoryToString(Buffer'Address,29));
      else
         Name := U("");
      end if;

   end Read_Dir;

   --

   function Nr_Of_Files (Path      : in string;
                         Recursive : in boolean :=false;
                         Count_Dirs: in boolean :=false) return integer is

      Nr         : integer := 0;
      Index      : integer := 0;
      Directory  : string  := path;
      Itemnr     : integer := 0;
      Itemname   : Ustring;
      Loadadr    : integer;
      Execadr    : integer;
      Length     : integer;
      Attributes : integer;
      Itemtype   : integer;
   begin
      Scan_Dir:
      loop
         Get_Directory_Entry (Directory,Itemnr,Itemname,Loadadr,Execadr,
                              Length,Attributes,Itemtype);

         exit Scan_Dir when Itemnr = -2;

         if Itemtype = 1 then
            -- File
            Nr := Nr + 1;
         else
            -- Dir or image
            if Count_Dirs then
               Nr := Nr + 1;
            end if;
            if Recursive then
               Nr:= Nr + Nr_Of_Files(Path & "." &
                                     to_String(Itemname),Recursive,Count_Dirs);
            end if;
         end if;
      end loop Scan_Dir;
      return Nr;

   end Nr_Of_Files;

   --

   function Get_Directory_List (Path       : in String;
                                Count_Dirs : in Boolean := false) return Directory_Type is

      Files     : Natural          := Nr_Of_Files(Path,false,Count_Dirs);
      Directory : Directory_Type(1..Files);
      Index     : Integer          := 0;
      Name      : Unbounded_String := U("");
      i         : integer          := 1;
      Object_Type : File_Object_Type;
   begin
      if Path'Length > 0 then
         while Index /= -1 loop
            Name := U("");
            Read_Dir(Path,Index,Name);
            if Ada.Strings.Unbounded.Length(Name) > 0 then
               Object_Type := Get_Object_Type(S(Path & U(".") & Name));
               if Count_Dirs or Object_Type = File_Object then
                  Directory(i) := Name;
                  i := i +1;
               end if;
            end if;
         end loop;
      end if;
      return Directory;

   end Get_Directory_List;

   --

   procedure Get_Directory_Entry (Directory  : in string;
                                  Itemnr     : in out integer;
                                  Itemname   : out Unbounded_String;
                                  Loadadr    : out integer;
                                  Execadr    : out integer;
                                  Length     : out integer;
                                  Attributes : out integer;
                                  Itemtype   : out integer) is

      Directory_0 : string := Directory & ASCII.NUL;
      Buffer      : array(0..63) of integer;
      Register    : aliased Kernel.swi_regs;
      Error       : oserror_access;
   begin
      Register.R(0) := 11;
      Register.R(1) := Adr_To_Int(Directory_0'address);
      Register.R(2) := Adr_To_Int(Buffer'address);
      Register.R(3) := 1;
      Register.R(4) := int(Itemnr);
      Register.R(5) := 255;
      Register.R(6) := 0;
      Error := Kernel.SWI (OS_GBPB,Register'access,Register'access);

      if Error /= null then
         pragma Debug(Reporter.Report("FileExternal.Get_Directory_Entry: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Itemnr      := integer(Register.R(4));
      Itemname    := U(Memory.MemoryToString(Buffer'Address,29));
      Loadadr     := Buffer(0);
      Execadr     := Buffer(1);
      Length      := Buffer(2);
      Attributes  := Buffer(3);
      Itemtype    := Buffer(4);

      if Register.R(3) = 0 then
         Itemnr := -2;
      end if;

   end Get_Directory_Entry;

   --

   procedure Get_File_Information (Filepath   : in string;
                                   Loadadr    : out integer;
                                   Execadr    : out integer;
                                   Length     : out integer;
                                   Attributes : out integer;
                                   Itemtype   : out File_Object_Type) is

      Error      : oserror_access;
      Register   : aliased Kernel.swi_regs;
      Filename_0 : string := Filepath & ASCII.NUL;
   begin
      Register.R(0) := 13;
      Register.R(1) := Adr_To_Int(Filename_0'Address);
      Error := Kernel.SWI (OS_File,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("FileExternal.Get_File_Information: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Loadadr    := Integer(Register.R(2));
      Execadr    := Integer(Register.R(3));
      Length     := Integer(Register.R(4));
      Attributes := Integer(Register.R(5));
      Itemtype   := File_Object_Type'Val(Integer(Register.R(0)));
   end Get_File_Information;

   --

   procedure Set_File_Type (Filename : in string;
                            Filetype : in integer) is

      Filename_0 : string := Filename & ASCII.NUL;
      Register   : aliased Kernel.swi_regs;
      Error      : oserror_access;
   begin
      Register.R(0) := 18;
      Register.R(1) := Adr_To_Int(Filename_0'Address);
      Register.R(2) := int(Filetype);
      Error := Kernel.SWI (OS_File,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("FileExternal.Set_File_Type: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   exception
       when others => null; pragma Debug(Reporter.Report("Error in FileExternal.Set_File_Type"));
   end Set_File_Type;

   --

   function Get_Size (Filename : in string) return integer is

      Filename_0 : string := Filename & ASCII.NUL;
      Register   : aliased Kernel.swi_regs;
      Error      : oserror_access;

   begin
      Register.R(0) := 17;
      Register.R(1) := Adr_To_Int(Filename_0'address);
      Error := Kernel.swi (OS_File,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("FileExternal.Get_Size: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return integer(Register.R(4));
   end Get_Size;

   --

   procedure Get_Directory_Stamp (Directory : in string;
                                  Loadadr   : out integer;
                                  Execadr   : out integer) is

      Directory_0 : string := Directory & ASCII.NUL;
      Register    : aliased Kernel.swi_regs;
      Error       : oserror_access;
   begin
     Register.R(0) := 21;
     Register.R(1) := Adr_To_Int(Directory_0'address);
     Error := Kernel.SWI (OS_File,Register'access,Register'access);

     if Error /= null then
        pragma Debug(Reporter.Report("FileExternal.Get_Directory_Stamp: " & To_Ada(Error.ErrMess)));
        OS.Raise_Error(Error);
     end if;
     Loadadr := integer(Register.R(2));
     Execadr := integer(Register.R(3));
   end Get_Directory_Stamp;

   --

   function Get_Object_Type (Filename : in string) return File_Object_Type is

      Filename_0 : string := Filename & ASCII.NUL;
      Register   : aliased Kernel.swi_regs;
      Error      : oserror_access;
   begin
      Register.R(0) := 23;
      Register.R(1) := Adr_To_Int(Filename_0'address);
      Error := Kernel.SWI(OS_File,Register'access,Register'access);
      if Error /= null then
         pragma Debug(Reporter.Report("FileExternal.Get_Object_Type: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return File_Object_Type'Val(Integer(Register.R(0)));
   end Get_Object_Type;

   --

   function Get_File_Type (Filename : in string) return integer is

      Filename_0 : string := Filename & ASCII.NUL;
      Register   : aliased Kernel.swi_regs;
      Error      : oserror_access;
   begin
      Register.R(0) := 23;
      Register.R(1) := Adr_To_Int(Filename_0'address);
      Error := Kernel.swi(OS_File,Register'access,Register'access);

      if Error /= null then
         pragma Debug(Reporter.Report("FileExternal.Get_Filetype: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      if Register.R(0) = 0 then
        return -1;
      else
        return integer(Register.R(6));
      end if;
   end Get_File_Type;

   --

   function Filetype_To_Hex (Loadadr : in integer) return string is

      Result : string(1..3);
      Masked : System.Unsigned_Types.Unsigned;
      Nibble : System.Unsigned_Types.Unsigned;
      Load   : System.Unsigned_Types.Unsigned;
   begin
      if Loadadr = -1 then
         return "";
      end if;
      Load := Int_To_Unsigned (Loadadr);
      if (Load and 16#FFF00000#) = 16#FFF00000# then
         Masked := System.Unsigned_Types.Shift_Right((Load and 16#000FFF00#),8);

         for i in 1..3 loop
            Nibble := (System.Unsigned_Types.Shift_Right(Masked,(3-i)*4)) and 16#F#;
            case Nibble is
            when 0  => Result(i) := '0';
            when 1  => Result(i) := '1';
            when 2  => Result(i) := '2';
            when 3  => Result(i) := '3';
            when 4  => Result(i) := '4';
            when 5  => Result(i) := '5';
            when 6  => Result(i) := '6';
            when 7  => Result(i) := '7';
            when 8  => Result(i) := '8';
            when 9  => Result(i) := '9';
            when 10 => Result(i) := 'A';
            when 11 => Result(i) := 'B';
            when 12 => Result(i) := 'C';
            when 13 => Result(i) := 'D';
            when 14 => Result(i) := 'E';
            when 15 => Result(i) := 'F';
            when others => Result(i) := 'x';
            end case;
         end loop;
         return Result;
      else
         return "xxx";
      end if;
   end Filetype_To_Hex;

   --

   function Filetype_To_Number (FileType : in String) return Integer is

      FileType_0 : string := FileType & ASCII.NUL;
      Register   : aliased Kernel.swi_regs;
      Error      : oserror_access;
   begin
      Register.R(0) := 31;
      Register.R(1) := Adr_To_Int(FileType_0'address);
      Error := Kernel.SWI(OS_FSControl,Register'access,Register'access);
      if Error /= null then
        return -1;
      end if;
      return Integer(Register.R(2));
   end Filetype_To_Number;

   --

   function Get_Attributes (Path : in string) return integer is

      Path_0   : String := Path & ASCII.NUL;
      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := 17;
      Register.R(1) := Adr_To_Int(Path_0'Address);
      Error := Kernel.SWI (OS_File,Register'Access,Register'Access);

      if Error = null then
         return integer(Register.r(5));
      else
         OS.Raise_Error(Error);
         pragma Debug(Reporter.Report("FileExternal.Get_Attributes: " & To_Ada(Error.ErrMess)));
         return 0;
      end if;
   end Get_Attributes;

   --

   procedure Set_Attributes (Path : in string; Attributes : in integer) is

      Path_0   : String := Path & ASCII.NUL;
      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin

      Register.R(0) := 4;
      Register.R(1) := Adr_To_Int(Path_0'Address);
      Register.R(5) := Int(Attributes);
      Error := Kernel.SWI (OS_File,Register'Access,Register'Access);

      if Error /= null then
         OS.Raise_Error(Error);
         pragma Debug(Reporter.Report("FileExternal.Set_Attributes: " & To_Ada(Error.ErrMess)));
      end if;
   end Set_Attributes;

   --

   function Get_Stamp (Path : in String) return UTC_Pointer is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
      Path_0   : String      := Path & ASCII.NUL;
      Stamp    : UTC_Pointer := new UTC_Time_Type;
      Result,Word : System.Unsigned_Types.Unsigned;
   begin
      Register.R(0) := 17;
      Register.R(1) := Adr_To_Int(Path_0'Address);
      Error := Kernel.SWI (OS_File,Register'Access,Register'Access);
      if Error /= null then
         OS.Raise_Error(Error);
         pragma Debug(Reporter.Report("FileExternal.Get_Stamp: " & To_Ada(Error.ErrMess)));
         return null;
      end if;
      Result := Int_To_Unsigned(integer(Register.R(2)));
      Stamp.all.Word := Int_To_Unsigned(integer(Register.R(3)));
      Word  := "and"(Result,16#ff#);
      Stamp.all.Last_Byte := Byte(Word);

      return Stamp;
   end Get_Stamp;

   --

end RASCAL.FileExternal;
