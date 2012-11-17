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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Kernel;                use Kernel;
with Interfaces.C;          use Interfaces.C;
with Reporter;

with RASCAL.OS;             use RASCAL.OS;
with RASCAL.Memory;         use RASCAL.Memory;
with RASCAL.Utility;        use RASCAL.Utility;
with RASCAL.WimpWindow;     use RASCAL.WimpWindow;

package body RASCAL.WimpIcon is

   --

--   texticonssize        : constant := 2;
--   type texticonbuffertyp is array (0..texticonssize-1) of Character;
--   type iconarray_type is array (0..63) of Integer;

--   texticonbuffer      : texticonbuffertyp;
   -- Pointer fuer Speicher auf indirected icons
   indirecticonmemory : int;
   -- Groesse dieses Speicherbereichs in Bytes
   indirecticonssize  : int;
--   texticonbufferoffset: Integer := 0; -- derzeitiger Offset zum freien Element
   texticonbufferadr   : Integer;
   nullmemadr          : int;
   -- static for text of "FrameIconCreate"
   privatetext : array(0..40) of Character;
   
   validation_r4_str   : String := "R4" & ASCII.NUL;
   validation_r4       : Address:= validation_r4_str'Address;
   validation_r53_str  : String := "R5,3" & ASCII.NUL;
   validation_r53      : Address:= validation_r53_str'Address;
   scrmaxx             : Integer;
   scrmaxy             : Integer;

   --

   procedure Create_FrameIcon (Window : in Wimp_Handle_Type;
                               Min_X  : in Integer;
                               Min_Y  : in Integer;
                               Max_X  : in Integer;
                               Max_Y  : in Integer;
                               Titel  : in String) is
   
      frameFlags      : constant := 2#00010111000000000000000100011101#;
      frametitleFlags : constant := 2#00010111000000000000000100110001#;
   
      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;
      Block    : Wimp_CreateIcon_Type;
   begin
      Block.Window := Window;
      Block.Min_X  := Min_X;
      Block.Min_Y  := Min_Y;
      Block.Max_X  := Max_X;
      Block.Max_Y  := Max_Y;
      Block.Flags  := frameFlags;

      Memory.PutWord(Integer(nullmemadr),Block.Data'Address,0);
      Memory.PutWord(Adr_To_Integer(validation_r4),Block.Data'Address,4);
      Memory.PutWord(1,Block.Data'Address,8);

      Register.r(0) := int(Window);
      Register.R(1) := Adr_To_Int(Block'Address);

      Error := Kernel.SWI (Wimp_CreateIcon,Register'Access, Register'Access);
      if Error /=null then
         pragma Debug(Reporter.Report("WimpIcon.Create_FrameIcon I " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Block.Window := Window;
      Block.Min_X  := Min_X + 24;
      Block.Min_Y  := Max_Y + 16 - 44;                       
      Block.Max_X  := Min_X + 24 + 8 + (Titel'Length * 16);  
      Block.Max_Y  := Max_Y + 16;                            
      Block.Flags  := frametitleFlags;                       

      Memory.StringToMemory(Titel,privateText'Address);

      Memory.PutWord(Adr_To_Integer(privateText'Address),Block.Data'Address,0);
      Memory.PutWord(Integer(nullmemadr),Block.Data'Address,4);
      Memory.PutWord(Titel'Length+1,Block.Data'Address,8);

      Error := Kernel.SWI (Wimp_CreateIcon,Register'Access, Register'Access);
      if Error /=null then
         pragma Debug(Reporter.Report("WimpIcon.Create_FrameIcon II " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Redraw_Icon (Window,Icon_Handle_Type(Register.R(0)));
   end Create_FrameIcon;

   --

   function Create_TextIcon (Window       : in Wimp_Handle_Type;
                             Min_X        : in Integer;
                             Min_Y        : in Integer;
                             Max_X        : in Integer;
                             Max_Y        : in Integer;
                             Flags        : in Integer;
                             Text         : in String;
                             bufferlength : in Integer := 0) return Icon_Handle_Type is
   
      bufflength : Integer := bufferlength;
      Icon       : Icon_Handle_Type;
      
      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;
      Block    : Wimp_CreateIcon_Type;
   begin
      Block.Window := Window;
      Block.Min_X  := Min_X;
      Block.Min_Y  := Min_Y;
      Block.Max_X  := Max_X;
      Block.Max_Y  := Max_Y;
      Block.Flags  := Flags;

      Memory.StringToMemory (Text, Texticonbuffer'Address, Texticonbufferoffset,
                              bufferlength);

      Memory.PutWord(Texticonbufferadr + Texticonbufferoffset,Block.Data'Address,0);
      Memory.PutWord(Integer(nullmemadr),Block.Data'Address,4);
      if bufflength = 0 then
         bufflength := Text'Length + 1;
      end if;
      Memory.PutWord(bufflength,Block.Data'Address,8);

      Texticonbufferoffset := Texticonbufferoffset + bufflength;

      Register.R(0) := Int(Window);
      Register.R(1) := Adr_To_Int(Block'Address);
      Error := Kernel.SWI (Wimp_CreateIcon,Register'Access, Register'Access);
      if Error /=null then
         pragma Debug(Reporter.Report("WimpIcon.Create_TextIcon " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Icon := Icon_Handle_Type(Register.R(0));
      Redraw_Icon (Window,Icon);
      return Icon;  
   end Create_TextIcon;

   --

   function Create_TextIcon (Window       : in Wimp_Handle_Type;
                             Min_X        : in Integer;
                             Min_Y        : in Integer;
                             Max_X        : in Integer;
                             Max_Y        : in Integer;
                             Flags        : in Integer;
                             Text         : in String;
                             bufferlength : in Integer;
                             Adresse      : in Address) return Icon_Handle_Type is
                            
      bufflength : Integer := bufferlength;
      Icon       : Icon_Handle_Type;
      
      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;
      Block    : Wimp_CreateIcon_Type;
   begin
      Block.Window := Window;
      Block.Min_X  := Min_X;
      Block.Min_Y  := Min_Y;
      Block.Max_X  := Max_X;
      Block.Max_Y  := Max_Y;
      Block.Flags  := Flags;
      Memory.PutWord(Text'Length+1,Block.Data'Address,8);
      Memory.StringToMemory(Text,Adresse,0,Text'Length+1);

      if (Unsigned(Flags) and 256) > 0 then
         Memory.StringToMemory(Text,Adresse,0,bufferlength);
         Memory.PutWord(Adr_to_Integer(Adresse),Block.Data'Address,0);
         Memory.PutWord(Integer(nullmemadr),Block.Data'Address,4);
         if bufflength = 0 then
            bufflength := Text'Length + 1;
         end if;
         Memory.PutWord(bufflength,Block.Data'Address,8);
      else
        Memory.StringToMemory (Text, Block.Data'Address, 0, 12);
      end if;
      Register.r(0) := int(Window);
      Register.R(1) := Adr_To_Int(Block'Address);

      Error := Kernel.SWI (Wimp_CreateIcon,Register'Access, Register'Access);
      if Error /=null then
         pragma Debug(Reporter.Report("WimpIcon.Create_TextIcon " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Icon := Icon_Handle_Type(Register.R(0));
      Redraw_Icon (Window,Icon);
      return Icon;
   end Create_TextIcon;

   --

   function Create_Button (Window : in Wimp_Handle_Type;
                           Min_X  : in Integer;
                           Min_Y  : in Integer;
                           Max_X  : in Integer;
                           Max_Y  : in Integer;
                           Text   : in String;
                           Adresse: in Address) return Icon_Handle_Type is
   
      -- Text,border,centered, filled, click-type: release
      Flags    : constant := 2#00010111000000000100000100111101#;
      Icon     : Icon_Handle_Type;
      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;
      Block    : Wimp_CreateIcon_Type;
   begin
      Register.r(0) := int(Window);
      Register.R(1) := Adr_To_Int(Block'Address);

      Block.Window := Window;
      Block.Min_X  := Min_X;
      Block.Min_Y  := Min_Y;
      Block.Max_X  := Max_X;
      Block.Max_Y  := Max_Y;
      Block.Flags  := Flags;

      Memory.PutWord(Adr_To_Integer(Adresse),Block.Data'Address,0);
      Memory.PutWord(Adr_To_Integer(validation_r53),Block.Data'Address,4);
      Memory.PutWord(Text'Length+1,Block.Data'Address,8);
      Memory.StringToMemory(Text,Adresse,0,Text'Length+1);

      Error := Kernel.SWI (Wimp_CreateIcon,Register'Access, Register'Access);
      if Error /=null then
         pragma Debug(Reporter.Report("WimpIcon.Create_Button " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Icon := Icon_Handle_Type(Register.R(0));
      Redraw_Icon (Window,Icon);
      return Icon;
   end Create_Button;

   --

   procedure Delete_Icon (Window : in Wimp_Handle_Type;
                          Icon   : in Icon_Handle_Type) is
   
      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;
      Block    : Wimp_IconState_Type;
   begin
      Block.Window  := Window;
      Block.Icon    := Icon;
      Register.R(1) := Adr_To_Int (Block'Address);
      Error := Kernel.SWI (Wimp_DeleteIcon,Register'Access,Register'Access);
      if Error /=null then
         pragma Debug(Reporter.Report("WimpIcon.Delete_Icon " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Delete_Icon;

   --

   function Is_Selected (Window : in Wimp_Handle_Type;
                         Icon   : in Icon_Handle_Type) return Boolean is
   
      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;
      Block    : Wimp_IconState_Type;
   begin
      Block.Window  := Window;
      Block.Icon    := Icon;
      Register.R(1) := Adr_To_Int (Block'Address);
      Error := Kernel.SWI (Wimp_GetIconState, Register'Access, Register'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("WimpIcon.Is_selected " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return (Block.Flags and 16#200000#)>0;
   end Is_Selected;
   
   --
   
--   function Icon_Data (Window : in Wimp_Handle_Type;
--                       Icon   : in Icon_Handle_Type) return indirecteddataptr is
--
--      Register : aliased Kernel.SWI_regs;
--      Error    : Kernel.oserror_access;
--      Block    : Wimp_IconState_Type;
--   begin
--      Block.Window  := Window;
--      Block.Icon    := Icon;
--      Register.R(1) := Adr_To_Int (Block'Address);
--      Error := Kernel.SWI (Wimp_GetIconState, Register'Access, Register'Access);
--      if Error /= null then
--        return "";
--      else
--        if (Block.Flags and 2#100000000#) > 0 then
--          return Block.Data;
--        else
--          return "";
--        end if;
--      end if;
--   end Icon_Data;
   
   --
   
   procedure Make_Unselectable (Window : in Wimp_Handle_Type;
                                Icon   : in Icon_Handle_Type) is
   begin
      Set_State (Window,Icon,16#400000#,16#400000#);
   end Make_Unselectable;

   --

   procedure Make_Selectable (Window : in Wimp_Handle_Type;
                              Icon   : in Icon_Handle_Type) is
   begin
      Set_State (Window,Icon,0,16#400000#);
   end Make_Selectable;

   --

   function Is_Selectable (Window : in Wimp_Handle_Type;
                          Icon    : in Icon_Handle_Type) return Boolean is
   
      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;
      Block    : Wimp_IconState_Type;
   begin
      Block.Window  := Window;
      Block.Icon    := Icon;
      Register.R(1) := Adr_To_Int (Block'Address);
      Error := Kernel.SWI (Wimp_GetIconState, Register'Access, Register'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("WimpIcon.Is_selectable " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return (Block.Flags and 16#400000#)>0;
   end Is_Selectable;

   --

   procedure UnSelect (Window : in Wimp_Handle_Type;
                       Icon   : in Icon_Handle_Type) is
   begin
      Set_State (Window,Icon,0,16#200000#);
   end UnSelect;

   --

   procedure Select_Icon (Window : in Wimp_Handle_Type;
                          Icon   : in Icon_Handle_Type) is
   begin
      Set_State (Window,Icon,16#200000#,16#200000#);
   end Select_Icon;

   --

   procedure Redraw_Icon (Window : in Wimp_Handle_Type;
                     Icon   : in Icon_Handle_Type) is
   begin
      Set_State(Window,Icon,0,0);
   end Redraw_Icon;

   --

   procedure Change_ButtonType (Window : in Wimp_Handle_Type;
                                Icon   : in Icon_Handle_Type;
                                Button : in Integer) is
   begin
      Set_State (Window,Icon,Button * 2**12,Utility.Unsigned_To_Int(16#0000F000#));
   end Change_ButtonType;

   --

   procedure Set_State (Window : in Wimp_Handle_Type;
                        Icon   : in Icon_Handle_Type;
                        Flags  : in Integer;
                        Mask   : in Integer) is

      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;
      Block    : Wimp_SetIconState_Tyoe;
   begin
      Block.Window     := Window;
      Block.Icon       := Icon;
      Block.EOR_Word   := Flags;
      Block.Clear_Word := Mask;

      Register.R(0) := 0;
      Register.R(1) := Adr_To_Int(Block'Address);
      Error := Kernel.SWI (Wimp_SetIconState, Register'Access, Register'Access);
      if Error /=null then
         pragma Debug(Reporter.Report("WimpIcon.Set_State " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_State;

   --

   procedure Set_Text (Window : in Wimp_Handle_Type;
                       Icon   : in Icon_Handle_Type;
                       Text   : in String;
                       Redraw : in Boolean := true) is
   
      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;
      Block    : Wimp_IconState_Type;
   begin
      Block.Window  := Window;
      Block.Icon    := Icon;
      Register.R(1) := Adr_To_Int (Block'Address);
      Error := Kernel.SWI (Wimp_GetIconState, Register'Access, Register'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("WimpIcon.Set_Text " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Memory.StringToMemory (Text,Integer_to_Adr(Memory.GetWord(Block.Data'Address,4)));
      if Redraw then
         Redraw_Icon (Window,Icon);
      end if;
   end Set_Text;

   --

   function Get_Text (Window : in Wimp_Handle_Type;
                      Icon   : in Icon_Handle_Type) return String is
   
      Register    : aliased Kernel.SWI_regs;
      Error       : Kernel.oserror_access;
      Block       : Wimp_IconState_Type;
      Text        : Unbounded_String;
      Flags       : Integer;
      Buffer_Size : Integer;
      Text_Buffer : Address;
   begin
      Block.Window  := Window;
      Block.Icon    := Icon;
      Register.R(1) := Adr_To_Int (Block'Address);
      Error := Kernel.SWI (Wimp_GetIconState, Register'Access, Register'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("WimpIcon.Get_Text " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Flags := Block.Flags;

      if "and"(Flags,1) = 1 then
         -- Icon has text.

         if "and"(Flags,256) = 256 then
            -- Text buffer is indirected

            Buffer_Size := Memory.GetWord(Block.Data'Address,8);
            if Buffer_Size = 0 then
               return "";
            end if;

            Text_Buffer := Integer_To_Adr(Memory.GetWord(Block.Data'Address,0));
            Text := U(Memory.MemoryToString(Text_Buffer,0,Buffer_Size));
         else
            Text := U(To_Ada(Block.Data));
         end if;
         Text := U(StripTrailingZeroes(S(Text)));
         Text := U(StripTrailingSpaces(S(Text)));
         Text := U(StripLeadingSpaces(S(Text)));
         return S(Text);
      else
         return "";
      end if;
   end Get_Text;

   --

   procedure Set_Validation (Window     : in Wimp_Handle_Type;
                             Icon       : in Icon_Handle_Type;
                             Validation : in String;
                             Redraw     : in Boolean := true) is
   
      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;
      Block    : Wimp_IconState_Type;
   begin
      Block.Window  := Window;
      Block.Icon    := Icon;
      Register.R(1) := Adr_To_Int (Block'Address);
      Error := Kernel.SWI (Wimp_GetIconState, Register'Access, Register'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("WimpIcon.Set_Validation " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Memory.StringToMemory (Validation,Integer_to_Adr(Memory.GetWord(Block.Data'Address,8)));
      if Redraw then
         Redraw_Icon (Window,Icon);
      end if;
   end Set_Validation;
   
   --

   function Get_Validation (Window : in Wimp_Handle_Type;
                            Icon   : in Icon_Handle_Type) return String is
   
      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;
      Block    : Wimp_IconState_Type;
   begin
      Block.Window  := Window;
      Block.Icon    := Icon;
      Register.R(1) := Adr_To_Int (Block'Address);
      Error := Kernel.SWI (Wimp_GetIconState, Register'Access, Register'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("WimpIcon.Get_Validation " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Memory.MemoryToString(Integer_To_Adr(Memory.GetWord(Block.Data'Address,8)));
   end Get_Validation;

   --

   function Get_Alignment (Window : in Wimp_Handle_Type;
                           Icon   : in Icon_Handle_Type) return Alignment_Type is
   
      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;
      Block    : Wimp_IconState_Type;
      Flags    : Integer;
   begin
      Block.Window  := Window;
      Block.Icon    := Icon;
      Register.R(1) := Adr_To_Int (Block'Address);
      Error := Kernel.SWI (Wimp_GetIconState, Register'Access, Register'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("WimpIcon.Get_Alignment " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Flags := Block.Flags;      
      if "and" (Flags,8)=8 then
         return Centre;
      elsif "and" (Flags,512)=512 then
         return Right;
      else
         return Left;
      end if;   
   end Get_Alignment;

   --

   function Get_Foreground (Window : in Wimp_Handle_Type;
                            Icon   : in Icon_Handle_Type) return Wimp_Colour is
   
      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;
      Block    : Wimp_IconState_Type;
      Flags    : Integer;
   begin
      Block.Window  := Window;
      Block.Icon    := Icon;
      Register.R(1) := Adr_To_Int (Block.Window'Address);
      Error := Kernel.SWI (Wimp_GetIconState, Register'Access, Register'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("WimpIcon.Get_Foreground " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Flags := Block.Flags;
      -- divisom args are converted to float to force right rounding
      return Wimp_Colour("and" (integer(float(Flags)/2.0**24),15));
   end Get_Foreground;

   --

   function Get_Background (Window : in Wimp_Handle_Type;
                            Icon   : in Icon_Handle_Type) return Wimp_Colour is
   
      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;
      Block    : Wimp_IconState_Type;
      Flags    : Integer;
   begin
      Block.Window  := Window;
      Block.Icon    := Icon;
      Register.R(1) := Adr_To_Int (Block.Window'Address);
      Error := Kernel.SWI (Wimp_GetIconState, Register'Access, Register'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("WimpIcon.Get_Background " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Flags := Block.Flags;
      -- divisom args are converted to float to force right rounding
      return Wimp_Colour("and" (integer(float(Flags)/2.0**28),15));
   end Get_Background;

   --

   procedure Set_Background (Window : in Wimp_Handle_Type;
                             Icon   : in Icon_Handle_Type;
                             Colour : in Wimp_Colour) is
   begin
      Set_State(Window,Icon,Integer(Colour) * 2**28,Utility.Unsigned_To_Int(16#F0000000#));
   end Set_Background;

   --

   procedure Set_Foreground (Window : in Wimp_Handle_Type;
                             Icon   : in Icon_Handle_Type;
                             Colour : in Wimp_Colour) is
   begin
      Set_State(Window,Icon,Integer(Colour) * 2**24,Utility.Unsigned_To_Int(16#0F000000#));
   end Set_Foreground;

   --

end RASCAL.WimpIcon;