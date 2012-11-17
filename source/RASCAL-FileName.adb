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

with Interfaces.C;                     use Interfaces.C;
with System.Storage_Elements;          use System.Storage_Elements;
with Kernel;                           use Kernel;

with Ada.Strings.Fixed;                use Ada.Strings.Fixed;
with Ada.Strings;                      use Ada.Strings;
with Ada.Strings.Maps;                 use Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;          use Ada.Characters.Handling;
with Reporter;

with RASCAL.OS;                        use RASCAL.OS;
with RASCAL.Utility;                   use RASCAL.Utility;
with RASCAL.Memory;                    use RASCAL.Memory;

package body RASCAL.FileName is

   use Ada.Strings.Unbounded;

   OS_FSControl : constant Interfaces.C.unsigned := 16#29#;

   Illegal_Letters : constant String := " $&%@\^:.,#*|" & '"';
   Legal_Letters   : constant String := "______________";
   Legalise        : Character_Mapping := To_Mapping(Illegal_Letters,Legal_Letters);

   --

   function Make_FileName (Name : String) return String is
   begin
      return Translate (Name,Legalise);
   end Make_FileName;

   -- Converts names with wildcards into path
   function Convert_Path (Path : in String) return String is

      Register        : aliased Kernel.swi_regs;
      Error           : oserror_access;
      Buffer_Size     : integer;
      Path_0          : string := Path & ASCII.NUL;
   begin
      if Path'Length = 0 then
         return Path;
      end if;
      -- Get needed buffer size
      Register.R(0) := 37;
      Register.R(1) := Adr_To_Int(Path_0'Address);
      Register.R(2) := 0;
      Register.R(4) := 0;
      Register.R(5) := 0;
      Error := Kernel.swi ( OS_FSControl, register'Access, register'Access );
      Buffer_Size := (- Integer(Register.R(5))) + 1;

      if Error /= Null then
         return Path;
      end if;

      declare
         Buffer : String(1..Buffer_Size);
      begin
         -- Convert path
         Register.R(0) := 37;
         Register.R(1) := Adr_To_Int(Path_0'Address);
         Register.R(2) := Adr_To_Int(Buffer'Address);
         Register.R(4) := 0;
         Register.R(5) := int(Buffer_Size);

         Error := Kernel.swi ( OS_FSControl, register'Access, register'Access);
         if Error /= Null then
            return Path;
         end if;
         return Buffer(1..Integer(Register.R(5))-1);
      end;

   end Convert_Path;

   --

   function Get_FS (Path : in String) return string is

      Pos : Natural;
   begin
      Pos := Index(Path,"::",Forward);
      if Pos = 0 or Pos = Path'First then
         return "";
      end if;
      return Path (Path'First..Pos-1);
   end Get_FS;

   --

   function Get_FS_And_Drive (Path : in string) return string is

      Pos : Natural;
   begin
      Pos := Index (Path,".$");
      if Pos = 0 or Pos = Path'First then
         return "";
      end if;
      return Path (Path'First..Pos-1);
   end Get_FS_And_Drive;

   --

   function Get_Leaf (Path : in String) return String is

      Dot,Colon,Variable,Pos : Natural;
   begin
      Dot      := Index(Path,".",backward);
      Colon    := Index(Path,":",backward);
      Variable := Index(Path,"$Path>",backward);
      if Dot+Colon+Variable = 0 then
         return Path;
      end if;
      Pos := Dot;
      if Colon > Pos then
         Pos := Colon;
      end if;
      if Variable > Pos then
         Pos := Variable+5;
      end if;
      if Pos = Path'Last then
         return "";
      end if;
      return Path(Pos+1..Path'Last);
   end Get_Leaf;

   --

   function Get_Path (Path : in String) return String is

      Dot,Colon,Variable,Pos : Natural;
   begin
      Dot      := Index(Path,".",backward);
      Colon    := Index(Path,":",backward);
      Variable := Index(Path,"$Path>",backward);
      if Dot+Colon+Variable = 0 then
         return "";
      end if;
      Pos := Dot;
      if Colon > Pos then
         Pos := Colon;
      end if;
      if Variable > Pos then
         Pos := Variable+5;
      end if;
      return Path(Path'First..Pos);
   end Get_Path;

   --

   function Web_To_RISCOS (Path          : in String;
                           No_Extensions : in Boolean := true;
                           Mixed_Case    : in Boolean := true) return String is

      index : integer := 0;
      Text  : Unbounded_String := U(Path);
   begin
      index := Ada.Strings.Unbounded.Index(Text,"..");
      while index > 0 loop
         Ada.Strings.Unbounded.Replace_Element(Text,index,'^');
         Ada.Strings.Unbounded.Delete(Text,index+1,index+1);
         index := Ada.Strings.Unbounded.Index(Text,"..");
      end loop;
      Ada.Strings.Unbounded.Trim(Text,Both);
      declare
         Str : String := S(Text);
      begin
         for i in Str'Range loop
            case Str(i) is
            when '.'    => Str(i) := '/';
            when '/'    => Str(i) := '.';
            when others => null;
            end case;
         end loop;
         Text := U(Str);
      end;

      if No_Extensions then
         Index := Ada.Strings.Unbounded.Index(Text,"/",backward);
         while Index > 1 loop
            Text := Ada.Strings.Unbounded.Head(Text,Index-1);
            Index := Ada.Strings.Unbounded.Index(Text,"/",backward);
         end loop;
      end if;

      if Mixed_Case then
         declare
            Result : string := S(Text);
            Set    : Character_Set := To_Set("-_ !([{~'#,.:;§$%&?*+^°" & '"');
         begin
            for i in Result'Range loop
               if i = Result'First or else Is_In(Result(i-1),Set) then
                  Result(i) := To_Upper(Result(i));
               else
                  Result(i) := To_Lower(Result(i));
               end if;
            end loop;
            Text := U(Result);
         end;
      end if;
      return S(text);

   end Web_TO_RISCOS;

   --

   function RISCOS_To_web (Path : in String) return String is

      index : integer := 0;
      Text  : Unbounded_String;
      Str   : String := Path;
   begin
      for i in Str'Range loop
         case Str(i) is
         when '/'    => Str(i) := '.';
         when '.'    => Str(i) := '/';
         when others => null;
         end case;
      end loop;
      Text  := U(Str);
      index := Ada.Strings.Unbounded.Index(Text,"^");
      if Length(Text) > 1 then
         while index > 0 loop

            if Index = 1 then
               Text := U(".." & Slice(Text,2,Length(Text)));
            elsif Index = Length(Text) then
               Text := U(Slice(Text,1,Length(Text)-1) & "..");
            else
               Text := U(Slice(Text,1,Index-1) & ".." & Slice (Text,Index+1,Length(Text)));
            end if;
            index := Ada.Strings.Unbounded.Index(Text,"^");
         end loop;
      else
         if Index > 0 then
            return "..";
         end if;
      end if;
      Ada.Strings.Unbounded.Trim(Text,Both);

      return S(Text);
   end RISCOS_To_Web;

   --

   function DOS_To_RISCOS (Path          : in String;
                           No_Extensions : in Boolean := true;
                           Mixed_Case    : in Boolean := true) return String is

      Index : Natural;
      Text  : Unbounded_String := U(Path);
   begin
      index := Ada.Strings.Unbounded.Index(Text,"..");
      while index > 0 loop
         Text := U(Slice(Text,1,Index-1) & "^" & Slice (Text,Index+2,Length(Text)));
         index := Ada.Strings.Unbounded.Index(Text,"..");
      end loop;

      Ada.Strings.Unbounded.Trim(Text,Both);
      declare
         Str : String := S(Text);
      begin
         for i in Str'Range loop
            case Str(i) is
            when '.'    => Str(i) := '/';
            when '\'    => Str(i) := '.';
            when others => null;
            end case;
         end loop;
         Text := U(str);
      end;

      if No_Extensions then
         Index := Ada.Strings.Unbounded.Index(Text,"/",backward);
         while Index > 1 loop
            Text := Ada.Strings.Unbounded.Head(Text,Index-1);
            Index := Ada.Strings.Unbounded.Index(Text,"/",backward);
         end loop;
      end if;

      if Mixed_Case then
         declare
            Result : string := S(Text);
            Set    : Character_Set := To_Set("-_ !([{~'#,.:;§$%&?*+^°" & '"');
         begin
            for i in Result'Range loop
               if i = Result'First or else Is_In(Result(i-1),Set) then
                  Result(i) := To_Upper(Result(i));
               else
                  Result(i) := To_Lower(Result(i));
               end if;
            end loop;
            return Result;
         end;
      end if;
      return S(text);

   end DOS_To_RISCOS;

   --

   function App_To_Dir (Original : in String) return String is

      Name : ustring := U(Original);
   begin
      while Ada.Strings.Unbounded.Index(Name,"!",forward) = 1 loop
         Name := Ada.Strings.Unbounded.Tail(Name,Ada.Strings.Unbounded.Length(Name)-1);
      end loop;
      return S(Name);
   end App_To_Dir;

   --

   function Seperate_Paths (Path : in String) return Paths_Type is

      Nr          : Natural            := Count(Path,",") + 1;
      Paths       : Paths_Type(1..Nr);
      Path_String : Unbounded_String   := U(Path);
      Len         : Natural            := Length(Path_String);
      i           : Natural            := 0;
      Paths_Index : Positive           := 1;
   begin
      while Len /= 0 loop
         I := Index(Path_String,",");
         if I > 0 then
            Paths(Paths_Index) := U(Slice(Path_String,1,I-1));
            Path_String := U(Slice(Path_String,I+1,Len));
         else
            Paths(Paths_Index) := Path_String;
            Path_STring := U("");
         end if;
         Paths_Index := Paths_Index + 1;
         Len  := Length(Path_String);
      end loop;
      return Paths;
   end Seperate_Paths;

   --

end RASCAL.FileName;
