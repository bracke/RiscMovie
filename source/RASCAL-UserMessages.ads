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

-- @brief UserMessage related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with Interfaces.C;   use Interfaces.C;
with System;         use System;

with RASCAL.OS;      use RASCAL.OS;

package RASCAL.UserMessages is

   --
   -- The sender does not expect a reply.
   --
   type Reason_UserMessage is null record;
   pragma Convention (C, Reason_UserMessage);

   type Reason_UserMessage_Pointer is access Reason_UserMessage;

   type AWEL_Reason_UserMessage is abstract new
        Wimp_EventListener(Reason_Event_UserMessage,-1,-1) with
   record
   Event : Reason_UserMessage_Pointer;
   end record;

   --
   -- If noone answers, it returns.
   --
   type Reason_UserMessageRecorded is null record;
   pragma Convention (C, Reason_UserMessageRecorded);

   type Reason_UserMessageRecorded_Pointer is access Reason_UserMessageRecorded;

   type AWEL_Reason_UserMessageRecorded is abstract new
        Wimp_EventListener(Reason_Event_UserMessageRecorded,-1,-1) with
   record
   Event : Reason_UserMessageRecorded_Pointer;
   end record;

   --
   -- This is a UserMessageRecorded that returns.
   --
   type Reason_UserMessageAcknowledge is null record;
   pragma Convention (C, Reason_UserMessageAcknowledge);

   type Reason_UserMessageAcknowledge_Pointer is access Reason_UserMessageAcknowledge;

   type AWEL_Reason_UserMessageAcknowledge is abstract new
        Wimp_EventListener(Reason_Event_UserMessageAcknowledge,-1,-1) with
   record
   Event : Reason_UserMessageAcknowledge_Pointer;
   end record;

   type Byte is mod 2**8;

   type Real_Time_Type is
   record
   Time : Char_Array(1..5);
   end record;
   pragma Convention (C, Real_Time_Type);

   --
   -- This sends a usermessage
   --
   procedure SendMessage(Message    : in Address;
                         Reason     : in Reason_Event_Code_Type;
                         Event_Code : in Message_Event_Code_Type;
                         Window     : in Wimp_Handle_Type := 0;
                         Icon       : in Icon_handle_Type := 0;
                         Length     : in Integer := 256);
                         
   --
   -- This acknowledges a usermessage.
   --
   procedure Acknowledge(Message  : in Address;
                         Window   : in Integer := 0;
                         Icon     : in Integer := 0);

   procedure Handle (The : in AWEL_Reason_UserMessage)              is abstract;
   procedure Handle (The : in AWEL_Reason_UserMessageRecorded)      is abstract;
   procedure Handle (The : in AWEL_Reason_UserMessageAcknowledge)   is abstract;

end RASCAL.UserMessages;
