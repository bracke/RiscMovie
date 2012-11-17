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

with Kernel;             use Kernel;
with Interfaces.C;       use Interfaces.C;
with Reporter;

with RASCAL.Utility;     use RASCAL.Utility;
with RASCAL.OS;          use RASCAL.OS;

package body RASCAL.Font is

   --
   
   Font_CacheAddr                      : constant := 16#040080#;
   Font_FindFont                       : constant := 16#040081#;
   Font_LoseFont                       : constant := 16#040082#;
   Font_ReadDefn                       : constant := 16#040083#;
   Font_ReadInfo                       : constant := 16#040084#;
   Font_StringWidth                    : constant := 16#040085#;
   Font_Paint                          : constant := 16#040086#;
   Font_Caret                          : constant := 16#040087#;
   Font_ConverttoOS                    : constant := 16#040088#;
   Font_Converttopoints                : constant := 16#040089#;
   Font_SetFont                        : constant := 16#04008A#;
   Font_CurrentFont                    : constant := 16#04008B#;
   Font_FutureFont                     : constant := 16#04008C#;
   Font_FindCaret                      : constant := 16#04008D#;
   Font_CharBBox                       : constant := 16#04008E#;
   Font_ReadScaleFactor                : constant := 16#04008F#;
   Font_SetScaleFactor                 : constant := 16#040090#;
   Font_ListFonts                      : constant := 16#040091#;
   Font_SetFontColours                 : constant := 16#040092#;
   Font_SetPalette                     : constant := 16#040093#;
   Font_ReadThresholds                 : constant := 16#040094#;
   Font_SetThresholds                  : constant := 16#040095#;
   Font_FindCaretJ                     : constant := 16#040096#;
   Font_StringBBox                     : constant := 16#040097#;
   Font_ReadColourTable                : constant := 16#040098#;
   Font_MakeBitmap                     : constant := 16#040099#;
   Font_UnCacheFile                    : constant := 16#04009A#;
   Font_SetFontMax                     : constant := 16#04009B#;
   Font_ReadFontMax                    : constant := 16#04009C#;
   Font_ReadFontPrefix                 : constant := 16#04009D#;
   Font_SwitchOutputToBuffer           : constant := 16#04009E#;
   Font_ReadFontMetrics                : constant := 16#04009F#;
   Font_DecodeMenu                     : constant := 16#0400A0#;
   Font_ScanString                     : constant := 16#0400A1#;
   Font_SetColourTable                 : constant := 16#0400A2#;
   Font_CurrentRGB                     : constant := 16#0400A3#;
   Font_FutureRGB                      : constant := 16#0400A4#;
   Font_ReadEncodingFilename           : constant := 16#0400A5#;
   Font_FindField                      : constant := 16#0400A6#;
   Font_ApplyFields                    : constant := 16#0400A7#;
   Font_LookupFont                     : constant := 16#0400A8#;
   Font_EnumerateCharacters            : constant := 16#0400A9#;
   
   --

   procedure Millipoints_To_OS (X : in out Integer;
                                Y : in out Integer) is

      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;
   begin
      Register.R(1) := int(X);
      Register.R(2) := int(Y);
      Error := Kernel.SWI (Font_ConverttoOS, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Font.Millipoints_To_OS " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      X := Integer(Register.R(1));
      Y := Integer(Register.R(2));

   end Millipoints_To_OS;

   --

   procedure OS_To_Millipoints (X : in out Integer;
                                Y : in out Integer) is

      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;
   begin
      Register.R(1) := int(X);
      Register.R(2) := int(Y);
      Error := Kernel.SWI (Font_ConverttoPoints, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Font.OS_To_Millipoints " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      X := Integer(Register.R(1));
      Y := Integer(Register.R(2));
            
   end OS_To_Millipoints;

   --

   procedure Get_StringWidth (Text   : in String;
                              Width  : in out Integer;
                              Height : in out Integer;
                              Length : in out Integer) is

      Text_0   : String := Text & ASCII.NUL;
      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;   
   begin
      Register.R(1) := Adr_To_Int(Text_0'Address);
      Register.R(2) := int(Width);
      Register.R(3) := int(Height);
      Register.R(4) := -1;
      Register.R(5) := int(Length);
      Error := Kernel.SWI (Font_StringWidth, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Font.Get_StringWidth " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Width  := Integer(Register.R(2));
      Height := Integer(Register.R(3));
      Length := Integer(Register.R(5));

   end Get_StringWidth;

   --

   function Truncate (Text   : in String;
                      Width  : in Integer;
                      Height : in Integer;
                      Length : in Integer;
                      Right  : in boolean) return String is

      W : Integer := Width;
      H : Integer := Height;
      L : Integer := Length;
      Ellipsis : String := "...";
      w2,h2,l2 : Integer;
   begin
      w2 := 400000;h2:=400000;l2:=200;
      Get_StringWidth (Ellipsis,w2,h2,l2);

      OS_To_Millipoints(W,H);
      W := W - w2*2;
      Get_StringWidth (Text,W,H,L);
      if L < Text'Length then
         if Right then
            return Text(Text'First..L) & Ellipsis;
         else
            return Ellipsis & Text((Text'Last-L)..Text'Last);
         end if;
      else
         return Text;
      end if;
   end Truncate;

   --

   procedure Enumerate_Characters (Font : in     Integer;
                                   Char : in out Integer;
                                   Code :    out Integer) is

      Register : aliased Kernel.SWI_regs;
      Error    : Kernel.oserror_access;   
   begin
      Register.R(0) := int(Font);
      Register.R(1) := int(Char);
      Error := Kernel.SWI (Font_EnumerateCharacters, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Font.Enumerate_Characters " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Char := Integer(Register.R(1));
      Code := Integer(Register.R(2));
   end;

   --

end RASCAL.Font;
