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

with RASCAL.Utility;    use RASCAL.Utility;
with RASCAL.Memory;     use RASCAL.Memory;
with RASCAL.OS;         use RASCAL.OS;
with RASCAL.SystemInfo;

with Interfaces.C;      use Interfaces.C;
with Kernel;            use Kernel;
with Reporter;

package body RASCAL.Pointer is

  --
  
  procedure Get_PointerInfo(X_Pos  : out integer;
                            Y_Pos  : out integer;
                            Button : out integer;
                            Window : out Wimp_Handle_Type;
                            Icon   : out Icon_Handle_Type) is

     Wimp_GetPointerInfo : constant Interfaces.C.unsigned :=16#400CF#;

     Register : aliased Kernel.swi_regs;
     Error    : oserror_access;
     Block    : array (1..5) of integer;
  begin
      Register.R(0) := 0;
      Register.R(1) := Adr_To_Int(Block'Address);

      Error := Kernel.swi(Wimp_GetPointerInfo,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Pointer.Get_PointerInfo: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      X_Pos  := Block(1);
      Y_Pos  := Block(2);
      Button := Block(3);
      Window := Wimp_Handle_Type(Block(4));
      Icon   := Icon_Handle_Type(Block(5));

  end Get_PointerInfo;

  --

  function Is_Select return Boolean is

     Window             : Wimp_Handle_Type;
     Icon               : Icon_Handle_Type;
     X_Pos,Y_Pos,Button : Integer := 0;
  begin
     Get_PointerInfo(X_Pos,Y_Pos,Button,Window,Icon);
     return Utility."and"(Button,4) = 4;
  end Is_Select;

  --

  function Is_Adjust return Boolean is

     Window             : Wimp_Handle_Type;
     Icon               : Icon_Handle_Type;
     X_Pos,Y_Pos,Button : Integer := 0;
  begin
     Get_PointerInfo(X_Pos,Y_Pos,Button,Window,Icon);
     return Utility."and"(Button,1) = 1;
  end Is_Adjust;

  --

  function Is_Menu return Boolean is

     Window             : Wimp_Handle_Type;
     Icon               : Icon_Handle_Type;
     X_Pos,Y_Pos,Button : Integer := 0;
  begin
     Get_PointerInfo(X_Pos,Y_Pos,Button,Window,Icon);
     return Utility."and"(Button,2) = 2;
  end Is_Menu;

  --

  function Get_DoubleClick_Delay return Integer is

     OS_Byte : constant Interfaces.C.unsigned :=16#6#;

     Register : aliased Kernel.swi_regs;
     Error    : oserror_access;

     DoubleCick_Delay : Integer := 0;
     Step             : Integer := 0;
     WindowManager    : Integer := SystemInfo.Get_WindowManager_Version;
  begin
     Register.R(0) := 161;
     Register.R(1) := 223;

     Error := Kernel.swi(OS_Byte,Register'Access,Register'Access);
  
     if Error /= null then
        pragma Debug(Reporter.Report("Pointer.Get_DoubleCick_Delay: I" & To_Ada(Error.ErrMess)));
        OS.Raise_Error(Error);
     end if;
     DoubleCick_Delay := Integer(Register.R(2));

     if WindowManager < 380 then
        DoubleCick_Delay := Utility."xor"(DoubleCick_Delay,10) * 10;
     else
        Register.R(0) := 161;
        Register.R(1) := 22;
        Error := Kernel.swi(OS_Byte,Register'Access,Register'Access);
  
        if Error /= null then
           pragma Debug(Reporter.Report("Pointer.Get_DoubleCick_Delay II: " & To_Ada(Error.ErrMess)));
           OS.Raise_Error(Error);
        end if;

        Step := Integer (Register.R(2));
        if "and"(step,1) = 1 then
           DoubleCick_Delay := Utility."and"(DoubleCick_Delay,15) * 100;
        else
           DoubleCick_Delay := Utility."and"(DoubleCick_Delay,15) * 10;
        end if;
     end if;
     return DoubleCick_Delay;
     
  end Get_DoubleClick_Delay;

  --

end RASCAL.Pointer;
