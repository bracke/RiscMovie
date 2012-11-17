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
with RASCAL.Utility;    use RASCAL.Utility;

with Kernel;            use Kernel;
with Interfaces.C;      use Interfaces.C;
with Reporter;

package body RASCAL.Caret is

   Wimp_SetCaretPosition : constant := 16#400D2#;
   Wimp_GetCaretPosition : constant := 16#400D3#;

   --
 
   procedure Remove is
   begin
      Set_Position(-1);
   end Remove;
 
   --
 
   procedure Get_Position (Window   : out Wimp_Handle_Type;
                           Icon     : out Icon_Handle_Type;
                           X_Offset : out Integer;
                           Y_Offset : out Integer;
                           Flags    : out Integer;
                           Index    : out Integer) is
 
      Block    : Caret_Position_Type;
      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := 0;
      Register.R(1) := Adr_To_Int(Block'Address);
      Error := Kernel.Swi (Wimp_GetCaretPosition, Register'Access, Register'Access);
 
      if Error /= null then
         pragma Debug(Reporter.Report("Caret.Get_Position: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
 
      Window   := Block.Window;
      Icon     := Block.Icon;
      X_Offset := Block.X_Offset;
      Y_Offset := Block.Y_Offset;
      Flags    := Block.Flags;
      Index    := Block.Index;
 
   end Get_Position;
 
   --
 
   procedure Set_Position (Window   : in Wimp_Handle_Type;
                           Icon     : in Icon_Handle_Type := -1;
                           X_Offset : in Integer          := 0;
                           Y_Offset : in Integer          := 0;
                           Flags    : in Integer          := -1;
                           Index    : in Integer          := -1) is
 
      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := Int(Window);
      Register.R(1) := Int(Icon);
      Register.R(2) := Int(X_Offset);
      Register.R(3) := Int(Y_Offset);
      Register.R(4) := Int(Flags);
      Register.R(5) := Int(Index);
 
      Error := Kernel.Swi (Wimp_SetCaretPosition, Register'Access, Register'Access);
 
      if Error /= null then
         pragma Debug(Reporter.Report("Caret.Set_Position: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
    end Set_Position;

   --

end RASCAL.Caret;
