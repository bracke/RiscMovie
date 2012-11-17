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

-- @brief Standard single tasking error window.
-- $Author$
-- $Date$
-- $Revision$

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with RASCAL.OS;             use RASCAL.OS;
with RASCAL.Utility;        use RASCAL.Utility;

package RASCAL.Error is

   type Error_Type is
   record
   Msg_Handle: Messages_Handle_Type;
   Task_Name : Unbounded_String;
   end record;

   type Error_Pointer is access Error_Type;

   type Error_Category_Type is (None,Info,Warning,Program,Question,User1,User2);
   type Error_Return_Type is (Nothing,Ok,Cancel,XButton1,XButton2,XButton3);
   type Error_Flags_Type is new integer;

   Error_Flag_Ok        : constant Error_Flags_Type :=   1;
   Error_Flag_Cancel    : constant Error_Flags_Type :=   2;
   Error_Flag_Highlight : constant Error_Flags_Type :=   4;
   Error_Flag_NoPrompt  : constant Error_Flags_Type :=   8;
   Error_Flag_NoPrefix  : constant Error_Flags_Type :=  16;
   Error_Flag_Immediate : constant Error_Flags_Type :=  32;
   Error_Flag_Simulate  : constant Error_Flags_Type :=  64;
   Error_Flag_NoBeep    : constant Error_Flags_Type := 128;

   type Error_Message_Type is
   record
   Spr_Area   : System_Sprite_Pointer;
   Flags      : Error_Flags_Type       := Error_Flag_Ok;
   Category   : Error_Category_Type    := None;
   Spr_Name   : String (1..13)         := S(13  * ASCII.NUL);
   Buttons    : String (1..256)        := S(256 * ASCII.NUL);
   Message    : String (1..252)        := S(252 * ASCII.NUL);
   Token      : String (1..256)        := S(256 * ASCII.NUL);
   Param1     : String (1..256)        := S(256 * ASCII.NUL);
   Param2     : String (1..256)        := S(256 * ASCII.NUL);
   Param3     : String (1..256)        := S(256 * ASCII.NUL);
   Param4     : String (1..256)        := S(256 * ASCII.NUL);
   end record;

   type Error_Message_Pointer is access Error_Message_Type;

   No_Error_Message : Exception;

   --
   -- Opens a standard non-multitasking WIMP Error window.
   --
   --{/}How to use:{/}
   --#Tab
   --#fCode
   --E : Error_Pointer := Get_Error(Main_Task);
   --#Tab
   --declare
   --  Result\t:\tError_Return_Type;
   --  M\t:\tError_Message_Pointer := new Error_Message_Type;
   --#Tab
   --begin
   --  M.all.Token (1..5)\t:= "DUMMY";
   --  M.all.Spr_Name(1..5)\t:= "!Dummy";
   --  M.all.Category\t:= Info;   --
   --  Result := Show_Message ( E , M );
   --end;
   --#f
   --#Tab
   --
   function Show_Message (Error   : in Error_Pointer;
                          Message : in Error_Message_Pointer) return Error_Return_Type;

end RASCAL.Error;
