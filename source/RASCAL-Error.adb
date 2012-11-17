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
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;
with Reporter;
with Text_IO;

with RASCAL.Utility;          use RASCAL.Utility;
with RASCAL.Memory;
with RASCAL.MessageTrans;
with RASCAL.OS;

package body RASCAL.Error is

   OS_SpriteOp   : constant := 16#2E#;

   --

   function Len (Str : in String) return Natural is
   begin
      return Memory.MemoryToString(Str'Address,0,ASCII.NUL)'Length;
   end Len;

   --

   function Show_Message (Error   : in Error_Pointer;
                          Message : in Error_Message_Pointer) return Error_Return_Type is

      Wimp_ReportError : constant := 16#400DF#;
      Task_Name        : Unbounded_String := U("");
      Message_String   : Unbounded_String := U("");
      Error_Block      : array(0..255) of Integer;
      Flags            : integer := 0;
      Register         : aliased Kernel.swi_regs;      
      Err              : oserror_access;
      Sprite_Name      : String (1..13) := S(13*ASCII.NUL);
   begin

    -- Prepare String
      -- Look using message msg_handle (if there is a token)
      if (Len(Message.all.Token) > 0) and
         (Error.all.Msg_Handle /= null) then

          Message_String := U(MessageTrans.Lookup(Message.all.Token,
                                                  Error.all.Msg_Handle,
                                                  Message.all.Param1,Message.all.Param2,
                                                  Message.all.Param3,Message.all.Param4));
      end if;

      -- Look using error msg_handle (if there is a token)
      if (Length(Message_String) = 0) and
         (Len(Message.all.Token) > 0)  and
         (Error.all.Msg_Handle /= null) then

         Message_String := U(MessageTrans.Lookup(Message.all.Token,
                                                 Error.all.Msg_Handle,
                                                 Message.all.Param1,Message.all.Param2,
                                                 Message.all.Param3,Message.all.Param4));
      end if;

      -- Look in message data-structure
      if Length(Message_String) = 0 then
         Message_String := U(Message.all.Message);
      end if;

      -- Give up and complain!
      if Length(Message_String) = 0 then
         raise No_Error_Message;
      end if;

   -- Prepare Flags
      Flags := integer(Message.all.Flags);
      if Message.all.Category /= None then
         Flags := Flags + 256;
         Flags := Flags + integer(Error_Category_Type'Pos(Message.all.Category)) * (2 ** 9);
      else
         if Len(Message.all.Buttons) > 0 then
            Flags := Flags + 256;
         end if;   
      end if;

   -- Prepare Task Name

      -- Look in error-datastructure
      Task_Name := Error.all.Task_Name;

      -- Look using error message handle
      if (Length(Task_Name) = 0) and (Error.all.Msg_Handle /= null) then
         Task_Name := To_Unbounded_String(MessageTrans.Lookup("_TaskName",Error.all.Msg_Handle,"","","",""));
      end if;

      -- Look using message handle
      if (Length(Task_Name) = 0) and (Error.all.Msg_Handle /= null) then
         Task_Name := To_Unbounded_String(MessageTrans.Lookup("_TaskName",Error.Msg_Handle,"","","",""));
      end if;

      -- Give up and assign standard value
      if Length(Task_Name) = 0 then
         Task_Name := To_Unbounded_String("Untitled");
      end if;

      Append(Task_Name,ASCII.NUL);
      Append(Message_String,ASCII.NUL);
       -- Prepare error block
         Error_Block(0) := 0;
         Memory.StringToMemory (S(Message_String),Error_Block(0)'Address,4);
       -- Prepare for SWI

         Register.R(0) := Adr_To_Int(Error_Block(0)'Address);
         Register.R(1) := int(Flags);
         Register.R(2) := Adr_To_Int(S(Task_Name)'Address);

         if Len(Message.all.Spr_Name) <=1 then
            Register.R(3) := 0;
            Register.R(4) := 0;
         else
            Register.R(3) := Adr_To_Int(Message.all.Spr_Name'Address);
            Register.R(4) := Adr_To_Int(Address(Message.all.Spr_Area));
         end if;
         if Len(Message.all.Buttons) <= 1 then
            Register.R(5) := 0;
         else
            Register.R(5) := Adr_To_Int(Message.all.Buttons'Address);
         end if;

       -- Issue SWI
         Err := Kernel.swi (Wimp_ReportError, Register'Access, Register'Access);

         if Err /= null then
            pragma Debug(Reporter.Report("Error.Show_Message: " & To_Ada(Err.errmess)));
            return Cancel;
         else
            return Error_Return_Type'Val(Register.R(1));
         end if;
   end Show_Message;

   --

end RASCAL.Error;
