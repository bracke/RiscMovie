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

-- @brief Event definitions for handling the Drag'N'Drop protocol (datasave and dataload). Includes a stack implementation for handling multiple dataloads.
-- $Author$
-- $Date$
-- $Revision$

with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Interfaces.C;                 use Interfaces.C;
with RASCAL.OS;                    use RASCAL.OS;

package RASCAL.DragNDrop is

   --
   -- This indicates that the sender wants to save data to the receiver.
   --
   --Normal use
   --十t User has terminated a drag
   --十t You get the first four words with Wimp_GetPointerInfo.
   --十t You send message to mentioned window.
   --
   --Possible responses from other task
   --十t It sends DataSaveAck
   --十t It sends RamFetch
   --
   --You should store the my_ref of each data save message,
   --to check against any future DataSaved message you might receive.
   --
   type Message_DataSave is
   record
   Header         : Message_Event_Header;
   Window         : Wimp_Handle_Type;
   Icon           : Icon_Handle_Type;
   Screen_X       : Integer;
   Screen_Y       : Integer;
   Size           : Integer;
   File_Type      : Integer;
   Leaf_Name      : String (1..208);
   end record;
   pragma Convention (C, Message_DataSave);

   type Message_DataSave_Pointer is access Message_DataSave;

   type AMEL_Message_DataSave is abstract new
        Message_EventListener(Message_Event_DataSave) with
   record
   Event : Message_DataSave_Pointer;
   end record;

   --
   -- Icon nr, and screen x,y are preserved from the DataSave message.
   --This is usually followed by a DataLoad message.
   --
   type Message_DataSaveAck is
   record
   Header         : Message_Event_Header;
   Window         : Wimp_Handle_Type;
   Icon           : Icon_Handle_Type;
   Screen_X       : Integer;
   Screen_Y       : Integer;
   Size           : Integer; 
   File_Type      : Integer;
   Full_Path      : String (1..208); 
   end record;
   pragma Convention (C, Message_DataSaveAck);

   type Message_DataSaveAck_Pointer is access Message_DataSaveAck;

   type AMEL_Message_DataSaveAck is abstract new
        Message_EventListener(Message_Event_DataSaveAck) with
   record
   Event : Message_DataSaveAck_Pointer;
   end record;

   --
   -- Receiver of message should load, and answer with DataLoadAck if successfull.
   --The column and row information is not present in RISC OS 2.
   --
   type Message_DataLoad is
   record
   Header         : Message_Event_Header;
   Window         : Wimp_Handle_Type;
   Icon           : Icon_Handle_Type;
   Screen_X       : Integer;
   Screen_Y       : Integer;
   Size           : Integer; 
   File_Type      : Integer;
   Full_Path      : String (1..200);
   Column         : Integer;
   Row            : Integer;
   end record;
   pragma Convention (C, Message_DataLoad);

   type Message_DataLoad_Pointer is access Message_DataLoad;

   type AMEL_Message_DataLoad is abstract new
        Message_EventListener(Message_Event_DataLoad) with
   record
   Event : Message_DataLoad_Pointer;
   end record;

   --
   -- This is sent as an answer to a DataLoad message.
   --All that should be done, is to change message type to 4 (DataLoadAck), and fill in the your_ref field.
   --
   type Message_DataLoadAck is
   record
   Header         : Message_Event_Header;
   Window         : Wimp_Handle_Type;
   Icon           : Icon_Handle_Type;
   Screen_X       : Integer;
   Screen_Y       : Integer;
   Size           : Integer; 
   File_Type      : Integer;
   Full_Path      : String (1..208);
   end record;
   pragma Convention (C, Message_DataLoadAck);

   type Message_DataLoadAck_Pointer is access Message_DataLoadAck;

   type AMEL_Message_DataLoadAck is abstract new
        Message_EventListener(Message_Event_DataLoadAck) with
   record
   Event : Message_DataLoadAck_Pointer;
   end record;

   --
   -- This message is broadcasted when the user doubleclicks on a file.
   --If the receiver wants to load the file, he should acknowledge this message by returning a DataLoadAck message.
   --If no one acknowledges the message, the filer will try to run It.
   --
   type Message_DataOpen is
   record
   Header         : Message_Event_Header;
   Window         : Wimp_Handle_Type;
   Unused         : Integer;
   X_Offset       : Integer;
   Y_Offset       : Integer;
   Zero           : Integer;
   File_Type      : Integer;
   Full_Path      : String (1..208);
   end record;
   pragma Convention (C, Message_DataOpen);

   type Message_DataOpen_Pointer is access Message_DataOpen;

   type AMEL_Message_DataOpen is abstract new
        Message_EventListener(Message_Event_DataOpen) with
   record
   Event : Message_DataOpen_Pointer;
   end record;

   --
   -- This message is sent back to the originator of a saved file if at some later time it becomes "safe" (i.e. on disc).
   --You should compare the your_ref in this message with the my_ref you stored when you sent the original DataSave message.
   --If they tally the file can now be marked as safe, unless the user had edited it in the meantime.
   --
   --Probably the best implementation is for your program to store the my_ref with the file, but zero it on any modification, as 0 will not be a valid your_ref.
   --It appears that this message is designed for cases where one program saves a file to another program, and then that second program saves the unmodified file to disc.
   --
   type Message_DataSaved is
   record
   Header         : Message_Event_Header;
   end record;
   pragma Convention (C, Message_DataSaved);

   type Message_DataSaved_Pointer is access Message_DataSaved;

   type AMEL_Message_DataSaved is abstract new
        Message_EventListener(Message_Event_DataSaved) with
   record
   Event : Message_DataSaved_Pointer;
   end record;

   --
   --
   --
   type Message_RamFetch is
   record
   Header         : Message_Event_Header;
   Buffer_Pointer : Integer;
   Buffer_Size    : Integer;
   end record;
   pragma Convention (C, Message_RamFetch);

   type Message_RamFetch_Pointer is access Message_RamFetch;

   type AMEL_Message_RamFetch is abstract new
        Message_EventListener(Message_Event_RamFetch) with
   record
   Event : Message_RamFetch_Pointer;
   end record;

   --
   -- After receiving a RamFetch message, you write data to buffer, using Wimp_TransferBlock,
   --and then send back this message.
   --
   type Message_RamTransmit is
   record
   Header         : Message_Event_Header;
   Buffer_Pointer : Integer;
   Amount_Used    : Integer;
   end record;
   pragma Convention (C, Message_RamTransmit);

   type Message_RamTransmit_Pointer is access Message_RamTransmit;

   type AMEL_Message_RamTransmit is abstract new
        Message_EventListener(Message_Event_RamTransmit) with
   record
   Event : Message_RamTransmit_Pointer;
   end record;

   --
   -- Is the stack empty ?
   --
   function Empty return boolean;

   --
   -- Add a new DragNDrop object to the stack.
   --
   procedure Push_DragObject (Path   : in Unbounded_String;
                              Window : in Wimp_Handle_Type := 0;
                              Icon   : in Icon_Handle_Type := 0);

   --
   -- Return data of first drag object in the stack and removes that element from the stack.
   --
   procedure Pop_DragObject (Path   : out Unbounded_String;
                             Window : out Wimp_Handle_Type;
                             Icon   : out Icon_Handle_Type);

   procedure Handle (The : in AMEL_Message_DataSave)    is abstract;
   procedure Handle (The : in AMEL_Message_DataSaveAck) is abstract;
   procedure Handle (The : in AMEL_Message_DataLoad)    is abstract;
   procedure Handle (The : in AMEL_Message_DataLoadAck) is abstract;
   procedure Handle (The : in AMEL_Message_DataOpen)    is abstract;
   procedure Handle (The : in AMEL_Message_DataSaved)   is abstract;

   procedure Handle (The : in AMEL_Message_RAMFetch)    is abstract;
   procedure Handle (The : in AMEL_Message_RAMTransmit) is abstract;

private

   type DragObject;
   type DragObject_Pointer is access DragObject;
   
   type DragObject is
   record
      Window   : Wimp_Handle_type;
      Icon     : Icon_Handle_Type;
      Filename : Unbounded_String;
      Next     : DragObject_Pointer;
   end record;

   Head_Object        : DragObject_Pointer := null;
   Current_DragObject : DragObject_Pointer := null;
   
end RASCAL.DragNDrop;