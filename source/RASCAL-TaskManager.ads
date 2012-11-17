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

-- @brief Task & TaskManager related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with Interfaces.C;               use Interfaces.C;
with System;                     use System;
with System.Unsigned_Types;      use System.Unsigned_Types;
with RASCAL.OS;                  use RASCAL.OS;

package RASCAL.TaskManager is

   --
   -- Type used by Enumerate_Task to return information about each task.
   --
   type Task_Type is
   record
   Handle      : Integer;
   Name        : Address;
   Slot_Size   : Integer;
   Flags       : System.Unsigned_Types.Unsigned;
   end record;
   pragma Convention (C, Task_Type);
   
   type Task_Type_Pointer is access Task_type;

   Unknown_Task : Exception;

   --
   -- An array with a Task_Type for each task.
   --
   type Task_List_Type is array (natural range <>) of Task_Type;
   type Task_List_Pointer is access Task_List_Type;

   --
   -- On receiving this message the task should tidy up, call Wimp_CloseDown and OS_Exit.
   --
   type Message_Quit is
   record
   Header         : Message_Event_Header;
   end record;
   pragma Convention (C, Message_Quit);

   type Message_Quit_Pointer is access Message_Quit;

   type AMEL_Message_Quit is abstract new
        Message_EventListener(Message_Event_Quit) with
   record
   Event : Message_Quit_Pointer;
   end record;

   --
   -- This message is broadcasted when Ctrl-Shift F12 is pressed.
   --It is also sent to a specific task, when the user tries to quit it from the task display menu.
   --If you have no objections to being killed, then ignore this message, and you will eventually receive a Quit message.
   --If you object to being killed (unsaved data..), you should acknowledge this message, and then open a dialogue box asking the user what to do.
   --If the user chooses to quit and the 'flag' was zero, you are supposed to restart the closedown sequence by emitting a Ctrl-Shift F12 (508), using Wimp_ProcessKey.
   --
   type Message_PreQuit is
   record
   Header         : Message_Event_Header;
   Flag           : Integer;
   end record;
   pragma Convention (C, Message_PreQuit);

   type Message_PreQuit_Pointer is access Message_PreQuit;

   type AMEL_Message_PreQuit is abstract new
        Message_EventListener(Message_Event_PreQuit) with
   record
   Event : Message_PreQuit_Pointer;
   end record;

   --
   -- This message is broadcasted by the task manager when the user has selected "Save boot file", or on exit from desktop if the system variable SaveDesk$File has a value (a pathname..).
   --
   type Message_SaveDesktop is
   record
   Header         : Message_Event_Header;
   File_Handle    : System.Unsigned_Types.Unsigned;
   Flags          : System.Unsigned_Types.Unsigned;
   end record;
   pragma Convention (C, Message_SaveDesktop);

   type Message_SaveDesktop_Pointer is access Message_SaveDesktop;

   type AMEL_Message_SaveDesktop is abstract new
        Message_EventListener(Message_Event_SaveDesktop) with
   record
   Event : Message_SaveDesktop_Pointer;
   end record;

   --
   -- This message is broadcast when TaskManager_Shutdown is called with bit 3 of R0 set, to indicate a forced shutdown, for example due to power failure.
   --
   type Message_Shutdown is
   record
   Header         : Message_Event_Header;
   Flags          : System.Unsigned_Types.Unsigned;
   end record;
   pragma Convention (C, Message_Shutdown);

   type Message_Shutdown_Pointer is access Message_Shutdown;

   type AMEL_Message_Shutdown is abstract new
        Message_EventListener(Message_Event_Shutdown) with
   record
   Event : Message_Shutdown_Pointer;
   end record;

   --
   -- This message is broadcast when a task calls Wimp_Initialise.
   --The fields are set so that it appears to originate from the new task.
   --The Task Manager uses this message to identify the task's name. These names are stored in a table which can be queried using TaskManager_TaskNameFromHandle.
   --
   type Message_TaskInitialise is
   record
   Header         : Message_Event_Header;
   Task_Handle    : System.Unsigned_Types.Unsigned;
   CAO            : System.Unsigned_Types.Unsigned;
   Slot_Size      : System.Unsigned_Types.Unsigned;
   Task_Name      : Char_Array (1..228);
   end record;
   pragma Convention (C, Message_TaskInitialise);

   type Message_TaskInitialise_Pointer is access Message_TaskInitialise;

   type AMEL_Message_TaskInitialise is abstract new
        Message_EventListener(Message_Event_TaskInitialise) with
   record
   Event : Message_TaskInitialise_Pointer;
   end record;

   --
   -- This message is broadcast when a task calls Wimp_CloseDown.
   --The fields are set so that it appears to originate from the dying task.
   --
   type Message_TaskCloseDown is
   record
   Header         : Message_Event_Header;
   Task_Handle    : System.Unsigned_Types.Unsigned;
   end record;
   pragma Convention (C, Message_TaskCloseDown);

   type Message_TaskCloseDown_Pointer is access Message_TaskCloseDown;

   type AMEL_Message_TaskCloseDown is abstract new
        Message_EventListener(Message_Event_TaskCloseDown) with
   record
   Event : Message_TaskCloseDown_Pointer;
   end record;

   --
   -- This message is broadcast after Wimp_SlotSize is called.
   --It is mainly used by the task manager to update its display.
   --This message should not be acknowledged.
   --
   type Message_SlotSize is
   record
   Header         : Message_Event_Header;
   Task_Handle    : System.Unsigned_Types.Unsigned;
   Slot_Size      : System.Unsigned_Types.Unsigned;
   Next_Size      : System.Unsigned_Types.Unsigned;
   end record;
   pragma Convention (C, Message_SlotSize);

   type Message_SlotSize_Pointer is access Message_SlotSize;

   type AMEL_Message_SlotSize is abstract new
        Message_EventListener(Message_Event_SlotSize) with
   record
   Event : Message_SlotSize_Pointer;
   end record;

   --
   -- This message is broadcast when the user tries to alter a task's slot size by dragging the "memorybar" in the task manager.
   --If a task can cope with this, it should first check the taskhandle, acknowledge the message and then call Wimp_SlotSize.
   --This message is also sent by the task manager when the task is started.
   --If this, or any later message, is *not* acknowledged, the task will get a green memorybar in the task manager (instead of a red one) and will not receive this message again;
   --it will not be possible for the user to alter the task's slotsize.
   --
   type Message_SetSlot is
   record
   Header         : Message_Event_Header;
   Slot_Size      : System.Unsigned_Types.Unsigned;
   Task_Handle    : System.Unsigned_Types.Unsigned;
   end record;
   pragma Convention (C, Message_SetSlot);

   type Message_SetSlot_Pointer is access Message_SetSlot;

   type AMEL_Message_SetSlot is abstract new
        Message_EventListener(Message_Event_SetSlot) with
   record
   Event : Message_SetSlot_Pointer;
   end record;

   --
   -- This message can be used if your task want to find out the name of another task.
   --Broadcast this message with correct handle, and the task mamager will respond with a TaskNameIs message.
   --RISC OS 3 provides TaskManager_TaskNameFromHandle, which should be used in preference.
   --
   type Message_TaskNameRQ is
   record
   Header         : Message_Event_Header;
   Task_Handle    : System.Unsigned_Types.Unsigned;
   end record;
   pragma Convention (C, Message_TaskNameRQ);

   type Message_TaskNameRQ_Pointer is access Message_TaskNameRQ;

   type AMEL_Message_TaskNameRQ is abstract new
        Message_EventListener(Message_Event_TaskNameRQ) with
   record
   Event : Message_TaskNameRQ_Pointer;
   end record;

   --
   -- This message is sent by the task manager in response to a TaskNameRq message.
   --RISC OS 3 provides TaskManager_TaskNameFromHandle, which should be used in preference.
   --
   type Message_TaskNameIs is
   record
   Header         : Message_Event_Header;
   Task_Handle    : System.Unsigned_Types.Unsigned;
   Slot_Size      : System.Unsigned_Types.Unsigned;
   Task_Name      : Char_Array (1..232);
   end record;
   pragma Convention (C, Message_TaskNameIs);

   type Message_TaskNameIs_Pointer is access Message_TaskNameIs;

   type AMEL_Message_TaskNameIs is abstract new
        Message_EventListener(Message_Event_TaskNameIs) with
   record
   Event : Message_TaskNameIs_Pointer;
   end record;

   --
   -- This message is sent by the Filer once it has started up all of the desktop filers so that the Task Manager can 'renumber' it.
   --This is done so that during the SaveDesktop sequence the Filer's own commands can be performed after any network logons.
   --
   type Message_TaskStarted is
   record
   Header         : Message_Event_Header;
   end record;
   pragma Convention (C, Message_TaskStarted);

   type Message_TaskStarted_Pointer is access Message_TaskStarted;

   type AMEL_Message_TaskStarted is abstract new
        Message_EventListener(Message_Event_TaskStarted) with
   record
   Event : Message_TaskStarted_Pointer;
   end record;

   --
   -- This message is returned when the poll word becomes non-zero.
   --If bit 23 of the Wimp_Poll mask is set then this may be before messages and redraw requests have been returned (for example Message_ModeChange)
   --which allows tasks to make updates to data which affect the contents of windows before the windows have been redrawn with out of date contents.
   --With the bit clear this message is received at lower priority than user messages and redraw requests.
   --   
   type Reason_PollWordNonZero is
   record
   PollWord_Address : System.Unsigned_Types.Unsigned;
   PollWord         : System.Unsigned_Types.Unsigned;
   end record;
   pragma Convention (C, Reason_PollWordNonZero);

   type Reason_PollWordNonZero_Pointer is access Reason_PollWordNonZero;

   type AWEL_Reason_PollWordNonZero is abstract new
        Wimp_EventListener(Reason_Event_PollWordNonZero,-1,-1) with
   record
   Event : Reason_PollWordNonZero_Pointer;
   end record;


   --
   --
   --
   type Reason_NullReason is null record;
   pragma Convention (C, Reason_NullReason);

   type Reason_NullReason_Pointer is access Reason_NullReason;

   type AWEL_Reason_NullReason is abstract new
        Wimp_EventListener(Reason_Event_NullReason,-1,-1) with
   record
   Event : Reason_NullReason_Pointer;
   end record;

   --
   -- This SWI creates a new task, and executes the given *command.
   --The SWI returns to you when new task exits, or calls Wimp_Poll.
   --Note that you can only call this SWI from USR mode, and only if you are a 'live' task (ie. gained control from Wimp_Initialise or Wimp_Poll.
   --
   function Start_Task (Command : in String) return Integer;
   
   --
   -- Return array with information about all current tasks.
   --
   function Get_Task_List return Task_List_Type;

   --
   -- Enumerates through all running tasks.
   --
   function Enumerate_Task (Buffer : in Task_Type;
                            Index  : in integer := 0) return integer;

   --
   -- Finds the taskname of task with the 'Handle'.
   --
   function Get_TaskName (Handle : in integer) return String;

   --
   -- Finds a task given its name and returns the task's handle.
   --
   function Get_TaskHandle (Name : in String) return Integer;

   --
   -- Is the task 'Name' running ?
   --
   function Is_Task (Name : in String) return boolean;

   --
   -- Returns the number of tasks
   --
   function Nr_Of_Tasks return Natural;

   --
   -- Returns the wimphandle of the task owning the window.
   --
   function Get_WindowOwner (Window : Wimp_Handle_Type;
                             Icon   : Icon_Handle_Type) return Integer;

   procedure Handle (The : in AMEL_Message_TaskInitialise)          is abstract;
   procedure Handle (The : in AMEL_Message_TaskCloseDown)           is abstract;
   procedure Handle (The : in AMEL_Message_SlotSize)                is abstract;
   procedure Handle (The : in AMEL_Message_SetSlot)                 is abstract;
   procedure Handle (The : in AMEL_Message_TaskNameRq)              is abstract;
   procedure Handle (The : in AMEL_Message_TaskNameIs)              is abstract;
   procedure Handle (The : in AMEL_Message_TaskStarted)             is abstract;

   procedure Handle (The : in AMEL_Message_Quit)                    is abstract;
   procedure Handle (The : in AMEL_Message_PreQuit)                 is abstract;
   procedure Handle (The : in AMEL_Message_SaveDesktop)             is abstract;
   procedure Handle (The : in AMEL_Message_Shutdown)                is abstract;

   procedure Handle (The : in AWEL_Reason_NullReason)               is abstract;
   procedure Handle (The : in AWEL_Reason_PollWordNonZero)          is abstract;

private
end RASCAL.TaskManager;
