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

-- @brief Wimp Task related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with Interfaces.C;            use Interfaces.C;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Kernel;                  use Kernel;

with List;                    use List;
with HandlerList;             use HandlerList;

with RASCAL.OS;               use RASCAL.OS;
with RASCAL.Error;            use RASCAL.Error;
with RASCAL.Toolbox;          use RASCAL.Toolbox;

package RASCAL.WimpTask is

   Unable_To_Initialise_WimpTask : Exception;
   Unknown_Deleted_Event         : Exception;
   
   subtype Poll_Mask_Type is unsigned;

   type Task_Status_Type is new boolean;

   type Task_Handle_Type is new integer;
      
   type Wimp_Task_Class is tagged private;

   type Wimp_Version_Type is new integer;

   Unable_Initialise_ToolboxTask   : Exception;
   Remove_Event_Is_Toolbox_Event   : Exception;
   No_Toolbox_Res_Path             : Exception;
   TB_Event_Added_After_Task_Init  : Exception;

   type ToolBox_Task_Class is new WimpTask.Wimp_Task_Class with private;

   --
   -- Initialise Wimp task.
   --
   procedure Initialise (The : in out Wimp_Task_Class);

   --
   -- Initilise Toolbox task.
   --
   procedure Initialise (The : in out ToolBox_Task_Class);

   --
   -- Start multitasking / polling.
   --
   procedure Poll (The : in out Wimp_Task_Class'Class);

   --
   -- Make a single poll to ensure multitasking during calculations.
   --
   procedure Single_Poll (The : in out Wimp_Task_Class'Class;
                          Mask: in Integer := 0);

   --
   -- Add event listener.
   --
   procedure Add_Listener (The      : in out Wimp_Task_Class'Class;
                           Listener : in Event_Pointer);

   --
   -- Remove event listener, only wimp and message events.
   --
   procedure Remove_Listener (The      : in out Wimp_Task_Class'Class;
                              Listener : in Event_Pointer);

   --
   -- Returns Wimp version.
   --
   function Get_Wimp_Version (The : in Wimp_Task_Class'Class)
                                    return Wimp_Version_Type;

   --
   -- Returns Task handle.
   --
   function Get_Task_Handle (The : in Wimp_Task_Class'Class) return Task_Handle_Type;

   --
   -- Returns task name.
   --
   function Get_Name (The : in Wimp_Task_Class'Class) return String;

   --
   -- Returns Wimp block.
   --
   function Get_Wimp_Block (The : in Wimp_Task_Class'Class)
                                    return Wimp_Block_Pointer;

   --
   -- Returns task status.
   --
   function Get_Status (The : in Wimp_Task_Class'Class)
                                   return Task_Status_Type;

   --
   -- Returns the poll mask.
   --
   function Get_Poll_Mask (The : in Wimp_Task_Class'Class)
                                   return Poll_Mask_Type;

   --
   -- Gets the return time for Wimp_Poll_Idle
   --
   function Get_WakeUp_Time (The  : in Wimp_Task_Class'Class) return Integer;

   --
   -- Returns list of messages.
   --
   function Get_Messages (The : in Wimp_Task_Class'Class)
                                   return Messages_List_Pointer;

   --
   -- Returns message block.
   --
   function Get_Message_Block (The : in Wimp_Task_Class'Class)
                                    return Messages_Handle_Type;

   --
   -- Returns error block.
   --
   function Get_Error (The : in Wimp_Task_Class'Class) return Error.Error_Pointer;

   --
   -- Returns the path of the toolbox resource file.
   --
   function Get_Resources_Path (The : in ToolBox_Task_Class) return String;

   --
   --
   --
   function Get_Self_Id (The : in ToolBox_Task_Class) return Object_ID;

   --
   --
   --
   function Get_Self_Component (The : in ToolBox_Task_Class) return Component_ID;

   --
   --
   --
   function Get_Parent_Id (The : in ToolBox_Task_Class) return Object_ID;

   --
   --
   --
   function Get_Parent_Component (The : in ToolBox_Task_Class) return Component_ID;

   --
   --
   --
   function Get_Ancestor_Id (The : in ToolBox_Task_Class) return Object_ID;

   --
   --
   --
   function Get_Ancestor_Component (The : in ToolBox_Task_Class) return Component_ID;
   
   --
   -- Returns the spritearea of the toolbox task.
   --
   function Get_Sprite_Area (The : in ToolBox_Task_Class) return System_Sprite_Pointer;


   --
   -- Sets the required Wimp version.
   --
   procedure Set_Wimp_Version (The : in out Wimp_Task_Class'Class;
                               Nr  : in Wimp_Version_Type);

   --
   -- Sets the task handle.
   --
   procedure Set_Task_Handle (The      : in out Wimp_Task_Class'Class;
                              Handle   : in Task_Handle_Type);

   --
   -- Sets the task name.
   --
   procedure Set_Name (The : in out Wimp_Task_Class'Class; Name : in String);

   --
   -- Sets the task status.
   --
   procedure Set_Status (The : in out Wimp_Task_Class'Class;
                         Status : in Task_Status_Type);

   --
   -- Sets the poll mask.
   --
   procedure Set_Poll_Mask (The       : in out Wimp_Task_Class'Class;
                            Poll_Mask : in Poll_Mask_Type);

   --
   -- Changes the poll mask.
   --
   procedure Change_Mask (The   : in out Wimp_Task_Class'Class;
                          Value : in unsigned;
                          Set   : in Boolean := true);

   --
   -- Sets the return time for Wimp_Poll_Idle
   --
   procedure Set_WakeUp_Time (The  : in out Wimp_Task_Class'Class;
                              Time : in Integer);

   --
   -- Sets the error block.
   --
   procedure Set_Error (The : in out Wimp_Task_Class'Class;
                        E   : in Error_Pointer);

   --
   -- Defines the path of the toolbox resource file.
   --
   procedure Set_Resources_Path(The : in out ToolBox_Task_Class;
                                Path: in String);

   --
   -- Sets the spritearea for the toolbox task.
   --
   procedure Set_Sprite_Area (The : in out ToolBox_Task_Class;
                              Area: in System_Sprite_Pointer);

   --
   -- Writes the contents of the ID block to 'Reporter'.
   --
   procedure Report_ID_Block (The : in ToolBox_Task_Class);

   subtype Message_Pointer is Event_Pointer;
   
private

   --
   -- Remove wimp event listener.
   --
   procedure Remove_WimpListener (The      : in out Wimp_Task_Class'Class;
                                  Listener : in Event_Pointer);


   --
   -- Removes message event listener.
   --
   procedure Remove_MessageListener (The      : in out Wimp_Task_Class'Class;
                                     Listener : in Event_Pointer);

   --
   -- Add wimp event listener.
   --
   procedure Add_WimpListener (The      : in out Wimp_Task_Class'Class;
                               Listener : in Event_Pointer);

   --
   -- Add a toolbox event listener.
   --
   procedure Add_ToolboxListener (The      : in out ToolBox_Task_Class'Class;
                                  Listener : in Event_Pointer);

   --
   -- Add message event listener.
   --
   procedure Add_MessageListener (The      : in out Wimp_Task_Class'Class;
                                  Listener : in Event_Pointer);

   procedure Dispatch (The         : in out Wimp_Task_Class'Class;
                       Reason_Code : in Reason_Event_Code_Type;
                       Window      : in Wimp_Handle_Type;
                       Icon        : in Icon_Handle_Type);

   procedure Dispatch (The         : in out Wimp_Task_Class'Class;
                       Reason_Code : in Message_Event_Code_Type);

   procedure Dispatch (The         : in out ToolBox_Task_Class;
                       Reason_Code : in ToolBox_Event_Code_Type);

   function Find_Handler (The : in ToolBox_Task_Class;
                          Reason_Code : in ToolBox_Event_Code_Type) return HandlerList.ListPointer;

   function Find_Handler (The : in Wimp_Task_Class'Class;
                          Reason_Code : in Reason_Event_Code_Type)
                                 return HandlerList.ListPointer;

   function Find_Handler (The          : in Wimp_Task_Class'Class;
                          Message_Code : in Message_Event_Code_Type)
                                       return HandlerList.ListPointer;

   procedure Delete_Handler (The    : in Wimp_Task_Class'Class;
                             Events : in HandlerList.ListPointer);
                             
   type Wimp_Task_Class is tagged
   record
      WakeUpTime     : Integer                := 0;
      Continue       : Task_Status_Type       := false;
      Task_Handle    : Task_Handle_Type       := 0;
      Task_Name      : Unbounded_String       := To_Unbounded_String ("Untitled" & ASCII.NUL);
      Wimp_Nr        : Wimp_Version_Type      := 100;
      Wimp_Block     : Wimp_Block_Pointer     := new Wimp_Block_Type;
      Messages       : Messages_List_Pointer  := new Messages_List_Type;
      Mask           : Poll_Mask_Type         := 2#00000000000000000011100101110011#;
      Events         : List.ListPointer       := new List.List;
      Msg_Events     : List.ListPointer       := new List.List;
      Msg_Block      : Messages_Handle_Type   := new Messages_Control_Block_Type;
      Error          : Error_Pointer          := new Error_Type;
      Deleted_Events : HandlerList.ListPointer:= new HandlerList.List(-1,-1,-1);
   end record;

   Number_of_Toolbox_Events : integer := 0;

   Max_Number_of_Toolbox_Events : constant := 63;

   type ToolBox_Event_List_Type is
        array (0..Max_Number_Of_Toolbox_Events) of integer;

   type ToolBox_Event_List_Pointer is access ToolBox_Event_List_Type;

   type ToolBox_Task_Class is new WimpTask.Wimp_Task_Class with
   record
      Res_Path           : Unbounded_String;
      TB_Wanted_Events   : ToolBox_Event_List_Pointer
                         := new ToolBox_Event_List_Type;
      TB_Block           : Toolbox_Id_Block_Pointer
                         := new ToolBox_Id_Block_Type;
      TB_Event_Handlers  : List.ListPointer
                         := new List.List;
      Sprite_Area        : System_Sprite_Pointer;
   end record;
   
end RASCAL.WimpTask;
