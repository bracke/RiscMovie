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

-- @brief Event definitions handling messages from !Help and a subprogram to run !Help.
-- $Author$
-- $Date$
-- $Revision$

with Interfaces.C;   use Interfaces.C;
with RASCAL.OS;      use RASCAL.OS;

package RASCAL.InteractiveHelp is

   Help_Not_Found : Exception;

   type Help_Enable_Flag_Type is (Enable,Disable,Reread_Config);
    
   --
   -- The !Help application sends out this message.
   --
   --If you want to give help on the given window and icon, then reply with a HelpReply message.
   --
   type Message_HelpRequest is
   record
   Header         : Message_Event_Header;
   Mouse_X        : Integer;
   Mouse_Y        : Integer;
   Button_State   : Integer;
   Window         : Wimp_Handle_Type;
   Icon           : Icon_handle_Type;
   end record;
   pragma Convention (C, Message_HelpRequest);

   type Message_HelpRequest_Pointer is access Message_HelpRequest;

   type AMEL_Message_HelpRequest is abstract new
        Message_EventListener(Message_Event_HelpRequest) with
   record
   Event : Message_HelpRequest_Pointer;
   end record;

   --
   -- This message is sent by an application in reply to a HelpRequest message.
   --Escape codes may be used under RISC OS 3 !Help or equivalent.
   --The current !Help implementation passes the message block direct to OS_GSTrans before checking for "\\" escape codes.
   --Passing CR or LF terminated strings and using "|" codes other than {fCode}|M{f} is undocumented.
   --
   type Message_HelpReply is
   record
   Header         : Message_Event_Header;
   Help           : Char_Array(1..235);
   end record;
   pragma Convention (C, Message_HelpReply);

   type Message_HelpReply_Pointer is access Message_HelpReply;

   type AMEL_Message_HelpReply is abstract new
        Message_EventListener(Message_Event_HelpReply) with
   record
   Event : Message_HelpReply_Pointer;
   end record;

   --
   -- Used for the RISC OS 4 !Help application and allows configuration to be handled by another program.
   --
   type Message_HelpEnable is
   record
   Header         : Message_Event_Header;
   Flag           : Help_Enable_Flag_Type;
   end record;
   pragma Convention (C, Message_HelpEnable);

   type Message_HelpEnable_Pointer is access Message_HelpEnable;

   type AMEL_Message_HelpEnable is abstract new
        Message_EventListener(Message_Event_HelpEnable) with
   record
   Event : Message_HelpEnable_Pointer;
   end record;

   --
   -- Run the Interactive Help application.
   --
   procedure Run;

   procedure Handle (The : in AMEL_Message_HelpRequest)             is abstract;
   procedure Handle (The : in AMEL_Message_HelpReply)               is abstract;
   procedure Handle (The : in AMEL_Message_HelpEnable)              is abstract;
   

end RASCAL.InteractiveHelp;
