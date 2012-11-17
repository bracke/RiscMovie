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

with RASCAL.Utility;          use RASCAL.Utility;
with RASCAL.MessageTrans;     use RASCAL.MessageTrans;
with RASCAL.Memory;           use RASCAL.Memory;

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Interfaces.C;            use Interfaces.C;
with System.Storage_Elements; use System.Storage_Elements;
with Kernel;                  use Kernel;
with Reporter;

package body RASCAL.WimpTask is

   --

   procedure Initialise (The : in out Wimp_Task_Class) is

      Wimp_Initialise : constant := 16#400C0#;
      Register        : aliased Kernel.swi_regs;
      Error           : oserror_access;
   begin
      --  Initialise Task
      Register.R (0) := int (350);
      Register.R (1) := 16#4B534154#;
      Register.R (2) := int (To_Integer (Get_Name (The)'address));
      Register.R (3) := int (To_Integer (The.Messages (0)'address));
      Error := Kernel.SWI (Wimp_Initialise, Register'access, Register'access);

      if Error /= null then
         pragma Debug(Reporter.Report("WimpTask.Initialise: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
         raise Unable_To_Initialise_WimpTask;
      else
         Set_Status (The, true);      
         Set_Task_Handle (The, Task_Handle_Type (Register.R (1)));
         Set_Wimp_Version (The, Wimp_Version_Type (Register.R (0)));
         Get_Message_Block(The).all(5) := 1;
         The.Error.all.Msg_Handle:=Get_Message_Block(The);
         The.Error.all.Task_Name :=U(Get_Name(The));
      end if;
   end Initialise;

   --

   procedure Initialise (The : in out ToolBox_Task_Class) is

      ToolBox_Initialise : constant := 16#44ECF#;
      Register           : aliased Kernel.swi_regs;
      Path               : String(To_String(The.Res_Path)'first..To_String(The.Res_Path)'last+1):= To_String(The.Res_Path) & Character'Val(0);
      M                  : Messages_List_Pointer := Get_Messages(The);
      Error              : oserror_access;
   begin
      if Length(The.Res_Path) = 0 then
         raise No_Toolbox_Res_Path;
      end if;
      -- Init task
      Register.R(0) := int(0);
      Register.R(1) := int(350);
      Register.R(2) := Adr_To_Int(M.all'Address);
      Register.R(3) := Adr_To_Int(The.TB_Wanted_Events.all'Address);
      Register.R(4) := Adr_To_Int(Path'Address);
      Register.R(5) := Adr_To_Int(Get_Message_Block(The).all'Address);
      Register.R(6) := Adr_To_Int(The.TB_Block.all'Address);
      Error := Kernel.Swi (ToolBox_Initialise, Register'Access, Register'Access);
      
      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxTask.Initialise: " & To_Ada(Error.errmess)));
         OS.Raise_Error(Error);
         raise Unable_Initialise_ToolboxTask;
      else
         Set_Status (The,true);
         Get_Message_Block (The).all(5) := 1;
         Set_Wimp_Version (The,Wimp_Version_Type(Register.R(0)));
         Set_Task_Handle (The,Task_Handle_Type(Register.R(1)));
         Set_Sprite_Area (The,System_Sprite_Pointer(Int_To_Adr(Register.R(2))));
         Set_Name (Wimp_Task_Class(The),
                   MessageTrans.Lookup("_TaskName" & Character'Val(0),
                   Get_Message_Block(Wimp_Task_Class(The))));

         Get_Error (Wimp_Task_Class(The)).all.Msg_Handle := Get_Message_Block(Wimp_Task_Class(The));
         Get_Error (Wimp_Task_Class(The)).all.Task_Name  := U(Get_Name(Wimp_Task_Class(The)));
      end if;
   end Initialise;

   --

   procedure Single_Poll (The : in out Wimp_Task_Class'Class;
                          Mask: in Integer := 0) is

      Block       : Wimp_Block_Pointer := Get_Wimp_Block(The);
      Wimp_Poll   : constant           := 16#400C7#;
      Register    : aliased Kernel.swi_regs;
      Reason_Code : integer;
      Error       : oserror_access;
   begin
      Register.R (0) := int(Mask);
      Register.R (1) := int (To_Integer (The.Wimp_Block (0)'Address));
      Error := Kernel.SWI (Wimp_Poll, Register'Access, Register'Access);

      if Error = null then
         Reason_Code := integer (Register.R (0));
         
         case Reason_Code is
         when 17 | 18 | 19 => Dispatch (The,Message_Event_Code_Type (The.Wimp_Block (4)));
         when 16#200#      => Dispatch (Toolbox_Task_Class(The),Toolbox_Event_Code_Type(Block(2)));
         when others       => Dispatch (The, Reason_Event_Code_Type (Reason_Code),
                                        Wimp_Handle_Type (The.Wimp_Block (0)),
                                        Icon_Handle_Type (The.Wimp_Block (1)));
         end case;
      else
         pragma Debug(Reporter.Report("WimpTask.SinglePoll: " & To_Ada(Error.errmess)));
         OS.Raise_Error(Error);
      end if;
   end Single_Poll;

   --

   procedure Poll (The : in out Wimp_Task_Class'Class) is

      Wimp_Poll     : constant := 16#400C7#;
      Wimp_PollIdle : constant := 16#400E1#;
      Register      : aliased Kernel.swi_regs;
      Reason_code   : integer;   
      Block         : Wimp_Block_Pointer := Get_Wimp_Block(The);
      Error         : oserror_access;
      i             : HandlerList.Position;
      Listener      : Event_Pointer;
   begin
      while Get_Status(The) loop

         while not IsEmpty(The.Deleted_Events.all) loop
            i := First (The.Deleted_Events.all);
            Listener := Retrieve (The.Deleted_Events.all,i);
            case Listener.all.Kind is
            when Wimp    => Remove_WimpListener (The,Listener);
            when Message => Remove_MessageListener (The,Listener);
            when others  => raise Unknown_Deleted_Event;
            end case;
            Delete (The.Deleted_Events.all,i);
         end loop;
         
         Register.R(0) := int(Get_Poll_Mask(The));
         Register.R(1) := Adr_To_Int(Block.all(0)'Address);
         if The.WakeUpTime = 0 then
            Error := Kernel.swi (Wimp_Poll, Register'Access, Register'Access);
         else
            Register.R(2) := int(The.WakeUpTime);
            Error := Kernel.swi (Wimp_PollIdle, Register'Access, Register'Access);
         end if;
         if Error = null then
            Reason_Code := integer(Register.R(0));
            case Reason_Code is
            when 17 | 18 | 19 => Dispatch(The,Message_Event_Code_Type(Block(4)));
            when 16#200#      => Dispatch(ToolBox_Task_Class(The),Toolbox_Event_Code_Type(Block(2)));
            when others       => Dispatch(The,Reason_Event_Code_Type(Reason_Code),Wimp_Handle_Type(Block(0)),Icon_Handle_Type(Block(1)));
            end case;
         else
            pragma Debug(Reporter.Report("WimpTask.Poll: " & To_Ada(Error.errmess)));
            OS.Raise_Error(Error);
         end if;
      end loop;
   end Poll;

--  Event handling

   procedure Add_Listener (The      : in out Wimp_Task_Class'Class;
                           Listener : in Event_Pointer) is
   begin
      case Listener.all.Kind is
      when Wimp    => Add_WimpListener (The,Listener);
      when Message => Add_MessageListener (The,Listener);
      when Others  => Add_ToolboxListener (Toolbox_Task_Class(The),Listener);
      end case;
   end Add_Listener;

   --

   procedure Remove_Listener (The      : in out Wimp_Task_Class'Class;
                              Listener : in Event_Pointer) is
   begin
      case Listener.all.Kind is
      when Wimp | Message => AddToRear(The.Deleted_Events.all,Listener);
      when Others         => raise Remove_Event_Is_Toolbox_Event;
      end case;
   end Remove_Listener;

   --

   procedure Add_ToolboxListener (The      : in out ToolBox_Task_Class'Class;
                                  Listener : in Event_Pointer) is

      Handlers     : HandlerList.ListPointer;
      Reason_Code  : Toolbox_Event_Code_Type := Toolbox_EventListener(Listener.all).Event_Code;
      Block        : Wimp_Block_Pointer      := Get_Wimp_Block(The);
   begin
      if Get_Status(The) then
         raise TB_Event_Added_After_Task_Init;
      end if;
      -- Initialise ID_Block pointer
      Toolbox_EventListener(Listener.all).ID_Block := The.TB_Block;

      -- Update Event pointer
      Memory.PutWord(Adr_To_Integer (Block.all'Address),
                     ToolBox_EventListener(Listener.all).ID_Block'Address,4);

      Handlers := Find_Handler(The,Reason_Code) ;
      if Handlers = null then
         Handlers := new HandlerList.List(integer(Reason_Code),-1,-1);
         AddToRear(The.TB_Event_Handlers.all,Handlers);
         The.TB_Wanted_Events(Number_of_Toolbox_Events) := integer(Reason_Code);
         Number_of_Toolbox_Events := Number_of_Toolbox_Events + 1;
      end if;
      AddToRear(Handlers.all,Listener);
   end Add_ToolboxListener;

   --

   procedure Add_WimpListener (The      : in out Wimp_Task_Class'Class;
                               Listener : in Event_Pointer) is

      Handlers    : HandlerList.ListPointer;
      Reason_Code : Reason_Event_Code_Type :=
                    Wimp_EventListener (Listener.all).Event_Code;
      Block       : Wimp_Block_Pointer := Get_Wimp_Block(The);
   begin
      -- Initialise Event pointer
      Memory.PutWord(Adr_To_Integer (Block.all'Address),
                     Wimp_EventListener(Listener.all).Icon'Address,4);

      Handlers := Find_Handler (The, Reason_Code);
      if Handlers = null then
         Handlers := new HandlerList.List (integer (Reason_Code), -1, -1);
         AddToRear (The.Events.all, Handlers);
         case Reason_Code is
         when Reason_Event_NullReason | Reason_Event_RedrawWindow |
              Reason_Event_PointerLeavingWindow | Reason_Event_PointerEnteringWindow |
              Reason_Event_MouseClick | Reason_Event_KeyPressed | Reason_Event_LoseCaret |
              Reason_Event_GainCaret | Reason_Event_PollWordNonZero |
              Reason_Event_UserMessage | Reason_Event_UserMessageRecorded |
              Reason_Event_UserMessageAcknowledge | 22 | 23 | 24 => Change_Mask (The, 2**integer (Reason_Code), false);
         when others => null;
         end case;
      end if;
      AddToRear (Handlers.all, Listener);
   end Add_WimpListener;

   --

   procedure Remove_WimpListener (The : in out Wimp_Task_Class'Class;
                                  Listener : in Event_Pointer) is

      Handlers    : HandlerList.ListPointer;
      Handler     : Event_Pointer;
      i           : HandlerList.Position;
      Reason_Code : Reason_Event_Code_Type :=
                    Wimp_EventListener (Listener.all).Event_Code;
   begin
      Handlers := Find_Handler (The, Reason_Code);
      if Handlers /= null then
         if not IsEmpty (Handlers.all) then
            i := First (Handlers.all);
            loop
               Handler := Retrieve (Handlers.all, i);
               if Handler.all = Listener.all then
                  Delete (Handlers.all, i);

                  exit;
               end if;
               exit when IsLast (Handlers.all, i);
               GoAhead (Handlers.all, i);
            end loop;
         end if;
         --  Remove reason if there are no handlers
         if IsEmpty (Handlers.all) then
            --  Update mask
            Change_Mask (The, 2**integer (Reason_Code));
            Delete_Handler(The,Handlers);
         end if;
      end if;

   end Remove_WimpListener;

   --

   procedure Add_MessageListener (The      : in out Wimp_Task_Class'Class;
                                  Listener : in Event_Pointer) is

      Handlers         : HandlerList.ListPointer;
      Buffer           : array (1 .. 2) of integer;
      Wimp_AddMessages : constant := 16#400F6#;
      Register         : aliased Kernel.swi_regs;
      Block            : Wimp_Block_Pointer := Get_Wimp_Block(The);
      Reason_Code      : Message_Event_Code_Type
                       := Message_EventListener (Listener.all).Event_Code;
   begin
      -- Initialise event pointer
      Memory.PutWord(Adr_To_Integer (Block.all'Address),
                     Message_EventListener(Listener.all).Event_Code'Address,4);

      Handlers := Find_Handler (The, Reason_Code);
      if Handlers = null then
         Handlers := new HandlerList.List (integer (Reason_Code), -1, -1);
         AddToRear (The.Msg_Events.all, Handlers);
         The.Messages (Number_Of_Messages) := integer (Reason_Code);

         -- Ensure message '0' is the last message in the array.
         if (Number_Of_Messages > 0) and then
            (The.Messages (Number_Of_Messages-1) = 0) then

            The.Messages (Number_Of_Messages-1) := The.Messages (Number_Of_Messages);
            The.Messages (Number_Of_Messages)   := 0;
         end if;   
         Number_Of_Messages := Number_Of_Messages + 1;
         --  If task is running...
         if Get_Status (The) then
            Buffer (1) := integer (Reason_Code);
            Buffer (2) := 0;
            Register.R (0) := int (To_Integer (Buffer (1)'Address));
            Kernel.SWI (Wimp_AddMessages, Register'Access, Register'Access);
         end if;
      end if;
      AddToRear (Handlers.all, Listener);
   end Add_MessageListener;

   --

   procedure Remove_MessageListener (The      : in out Wimp_Task_Class'Class;
                                     Listener : in Event_Pointer) is

      List                : HandlerList.ListPointer;
      Handler             : Event_Pointer;
      Buffer              : array (1 .. 2) of integer;
      Wimp_RemoveMessages : constant := 16#400F7#;
      Register            : aliased Kernel.swi_regs;
      i                   : HandlerList.Position;
      Reason_Code         : Message_Event_Code_Type
                          := Message_EventListener (Listener.all).Event_Code;
   begin
      List := Find_Handler (The, Reason_Code);
      if List /= null then
         if not IsEmpty (List.all) then
            i := First (List.all);
            loop
               Handler := Retrieve (List.all, i);
               if Handler.all = Listener.all then
                  Delete (List.all, i);
                  exit;
               end if;
               exit when IsLast (List.all, i);
               GoAhead (List.all, i);
            end loop;
         end if;
         --  Remove message if there are no handlers
         if IsEmpty (List.all) then
            Buffer (1) := integer (Reason_Code);
            Buffer (2) := 0;
            Register.R (0) := int (To_Integer (Buffer (1)'Address));
            Kernel.SWI (Wimp_RemoveMessages, Register'Access,
                                             Register'Access);
            Delete_Handler(The,List);                                 
         end if;
      end if;
   end Remove_MessageListener;
   
   --

   procedure Dispatch (The         : in out Wimp_Task_Class'Class;
                       Reason_Code : in Reason_Event_Code_Type;
                       Window      : in Wimp_Handle_Type;
                       Icon        : in Icon_Handle_Type) is

      Handlers : HandlerList.ListPointer;
      Handler  : Event_Pointer;
      i        : HandlerList.Position;
      W        : Wimp_Handle_Type;
      Ic       : Icon_Handle_Type;
   begin
      Handlers := Find_Handler (The, Reason_Code);
      if Handlers /= null then
         if not IsEmpty (Handlers.all) then
            i     := First (Handlers.all);
            loop
               Handler := Retrieve (Handlers.all, i);
               W := Wimp_EventListener (Handler.all).Window;
               Ic := Wimp_EventListener (Handler.all).Icon;
               if (W = Window or W = -1) then
                  if (Ic = Icon or Ic = -1) then
                     Handle (Handler.all);
                  end if;
               end if;
               exit when IsLast (Handlers.all, i);
               GoAhead (Handlers.all, i);
            end loop;
         end if;
      end if;
   end Dispatch;

   --

   procedure Dispatch (The         : in out Wimp_Task_Class'Class;
                       Reason_Code : in Message_Event_Code_Type) is

      List     : HandlerList.ListPointer;
      Handler  : Event_Pointer;
      i        : HandlerList.Position;
   begin
      List := Find_Handler (The, Reason_Code);
      if List /= null then
         if not IsEmpty (List.all) then
            i := First (List.all);
            loop
               Handler := Retrieve (List.all, i);
               Handle (Handler.all);
               exit when IsLast (List.all, i);
               GoAhead (List.all, i);
            end loop;
         end if;
      end if;
   end Dispatch;

   --

   procedure Dispatch (The         : in out ToolBox_Task_Class;
                       Reason_Code : in ToolBox_Event_Code_Type) is

      Object    : constant Object_ID    := Get_Self_Id(The);
      Component : constant Component_ID := Get_Self_Component(The);
      Handlers  : HandlerList.ListPointer;
      Handler   : Event_Pointer;
      I         : HandlerList.Position;
      O         : Object_ID;
      C         : Component_ID;
   begin
       Handlers := Find_Handler(The,Reason_Code);
       if Handlers /= null then
          if not IsEmpty(Handlers.all) then
             I := First(Handlers.all);
             loop
                Handler := Retrieve(Handlers.all,I);
                O := Object_ID(ToolBox_EventListener(Handler.all).Object);
                C := Component_ID(ToolBox_EventListener(Handler.all).Component);
                if (O = Object or O = -1) then
                   if (C = Component or C = -1) then
                      Handle(Handler.all);
                   end if;
                end if;
                exit when IsLast(Handlers.all,I);
                GoAhead(Handlers.all,I);
             end loop;
          end if;
       end if;
   end Dispatch;

   --
   
   function Find_Handler (The : in ToolBox_Task_Class;
                          Reason_Code : in ToolBox_Event_Code_Type) return HandlerList.ListPointer is

      i        : List.Position;
      Handlers : HandlerList.ListPointer;
   begin
     if not IsEmpty(The.TB_Event_Handlers.all) then
        i     := First(The.TB_Event_Handlers.all);
        loop
           Handlers := Retrieve(The.TB_Event_Handlers.all,i);
           if Get_Event_Code(Handlers.all) = integer(Reason_Code) then
              return Handlers;
           end if;
           exit when IsLast(The.TB_Event_Handlers.all,i);
           GoAhead(The.TB_Event_Handlers.all,i);
        end loop;
     end if;
     return null;
   end Find_Handler;

--  Get methods

   function Get_Message_Block (The : in Wimp_Task_Class'Class)
                                       return Messages_Handle_Type is
   begin
      return The.Msg_Block;
   end Get_Message_Block;

   --

   function Get_Wimp_Version (The : in Wimp_Task_Class'Class)
                                       return Wimp_Version_Type is
   begin
      return The.Wimp_Nr;
   end Get_Wimp_Version;

   --

   function Get_Task_Handle (The : in Wimp_Task_Class'Class)
                                       return Task_Handle_Type is
   begin
      return The.Task_Handle;
   end Get_Task_Handle;

   --

   function Get_Name (The : in Wimp_Task_Class'Class) return String is
   begin
      return To_String(The.Task_Name);
   end Get_Name;

   --

   function Get_Wimp_Block (The : in Wimp_Task_Class'Class)
                                       return Wimp_Block_Pointer is
   begin
      return The.Wimp_Block;
   end Get_Wimp_Block;

   --

   function Get_Status (The : in Wimp_Task_Class'Class)
                                       return Task_Status_Type is
   begin
      return The.Continue;
   end Get_Status;

   --

   function Get_Poll_Mask (The : in Wimp_Task_Class'Class)
                                       return Poll_Mask_Type is
   begin
      return The.Mask;
   end Get_Poll_Mask;

   --

   function Get_WakeUp_Time (The  : in Wimp_Task_Class'Class) return Integer is
   begin
      return The.WakeUpTime;
   end Get_WakeUp_Time;

   --

   function Get_Error (The : in Wimp_Task_Class'Class) return Error.Error_Pointer is
   begin
      return The.Error;
   end Get_Error;

   --
   
   function Get_Resources_Path (The : in ToolBox_Task_Class) return String is
   begin
      return To_String(The.Res_Path);
   end Get_Resources_Path;

   --

   function Get_Self_Id (The : in ToolBox_Task_Class) return Object_ID is
   begin
      return The.TB_Block.all.Self_Id;
   end Get_Self_Id;

   --

   function Get_Self_Component (The : in ToolBox_Task_Class) return Component_ID is
   begin
      return The.TB_Block.all.Self_Component;
   end Get_Self_Component;

   --

   function Get_Parent_Id (The : in ToolBox_Task_Class) return Object_ID is
   begin
      return The.TB_Block.all.Parent_Id;
   end Get_Parent_Id;

   --

   function Get_Parent_Component (The : in ToolBox_Task_Class) return Component_ID is
   begin
      return The.TB_Block.all.Parent_Component;
   end Get_Parent_Component;

   --

   function Get_Ancestor_Id (The : in ToolBox_Task_Class) return Object_ID is
   begin
      return The.TB_Block.all.Ancestor_Id;
   end Get_Ancestor_Id;

   --

   function Get_Ancestor_Component (The : in ToolBox_Task_Class) return Component_ID is
   begin
      return The.TB_Block.all.Ancestor_Component;
   end Get_Ancestor_Component;

   --

   function Get_Sprite_Area (The : in ToolBox_Task_Class) return System_Sprite_Pointer is
   begin
      return The.Sprite_Area;
   end Get_Sprite_Area;

--  Set methods

   procedure Set_Resources_Path (The  : in out ToolBox_Task_Class;
                                 Path : in String) is
   begin
      The.Res_Path := To_Unbounded_String(Path);
   end Set_Resources_Path;

   --

   procedure Set_Sprite_Area (The : in out ToolBox_Task_Class;
                              Area: in System_Sprite_Pointer) is
   begin
      The.Sprite_Area:=Area;
   end Set_Sprite_Area;

   --
   
   procedure Set_Wimp_Version (The : in out Wimp_Task_Class'Class;
                               Nr  : in Wimp_Version_Type) is
   begin
      The.Wimp_Nr := Nr;
   end Set_Wimp_Version;

   --

   procedure Set_Task_Handle (The      : in out Wimp_Task_Class'Class;
                              Handle   : in Task_Handle_Type) is
   begin
      The.Task_Handle := Handle;
   end Set_Task_Handle;

   --

   procedure Set_Name (The  : in out Wimp_Task_Class'Class;
                       Name : in String) is
   begin
      The.Task_Name := To_Unbounded_String (Name);
   end Set_Name;

   --

   procedure Set_Status (The    : in out Wimp_Task_Class'Class;
                         Status : in Task_Status_Type) is
   begin
      The.Continue := Status;
   end Set_Status;

   --

   procedure Set_Poll_Mask (The       : in out Wimp_Task_Class'Class;
                            Poll_Mask : in Poll_Mask_Type) is
   begin
      The.Mask := Poll_Mask;
   end Set_Poll_Mask;

   --

   procedure Set_WakeUp_Time (The  : in out Wimp_Task_Class'Class;
                              Time : in Integer) is
   begin
      The.WakeUpTime := Time;
   end Set_WakeUp_Time;

   --

   function Get_Messages (The : in Wimp_Task_Class'Class)
                                 return Messages_List_Pointer is
   begin
      return The.Messages;
   end Get_Messages;

--  Misc

   function Find_Handler (The          : in Wimp_Task_Class'Class;
                          Reason_Code  : in Reason_Event_Code_Type)
                                 return HandlerList.ListPointer is

      i        : List.Position;
      Handlers : HandlerList.ListPointer;
   begin
      if not IsEmpty (The.Events.all) then
         i     := First (The.Events.all);
         loop
            Handlers := Retrieve (The.Events.all, i);
            if Get_Event_Code (Handlers.all) = integer (Reason_Code) then
               return Handlers;
            end if;
            exit when IsLast (The.Events.all, i);
            GoAhead (The.Events.all, i);
         end loop;
      end if;
      return null;
   end Find_Handler;

   --

   function Find_Handler (The          : in Wimp_Task_Class'Class;
                          Message_Code : in Message_Event_Code_Type)
                                    return HandlerList.ListPointer is

      i        : List.Position;
      Handlers : HandlerList.ListPointer;
   begin
      if not IsEmpty (The.Msg_Events.all) then
         i := First (The.Msg_Events.all);
         loop
            Handlers := Retrieve (The.Msg_Events.all, i);
            if Get_Event_Code (Handlers.all) = integer (Message_Code) then
               return Handlers;
            end if;
            exit when IsLast (The.Msg_Events.all, i);
            GoAhead (The.Msg_Events.all, i);
         end loop;
      end if;
      return null;
   end Find_Handler;

   --

   procedure Delete_Handler (The    : in Wimp_Task_Class'Class;
                             Events : in HandlerList.ListPointer) is

      i        : List.Position;
      Handlers : HandlerList.ListPointer;
   begin
      if not IsEmpty (The.Events.all) then
         i := First (The.Events.all);
         loop
            Handlers := Retrieve (The.Events.all, i);
            if Handlers = Events then
               Delete (The.Events.all,i);
               exit;
            end if;
            exit when IsLast (The.Events.all, i);
            GoAhead (The.Events.all, i);
         end loop;
      end if;
   end Delete_Handler;

   --

   procedure Change_Mask (The   : in out Wimp_Task_Class'Class;
                          Value : in unsigned;
                          Set   : in Boolean := true) is

      New_Mask : Poll_Mask_Type := Get_Poll_Mask (The);
   begin
      if Set then
         New_Mask := New_Mask or Value;
      else
         New_Mask := New_Mask and (not Value);
      end if;
      Set_Poll_Mask (The, New_Mask);
   end Change_Mask;

   --

   procedure Set_Error (The : in out Wimp_Task_Class'Class;
                        E   : in Error_Pointer) is
   begin
      The.Error := E;
   end Set_Error;

   --

   procedure Report_ID_Block (The : in ToolBox_Task_Class) is
   begin
      pragma Debug(Reporter.Report("Ancestor_ID: " & intstr(integer(The.TB_Block.all.Ancestor_ID))));
      null; pragma Debug(Reporter.Report("Ancestor_Component: " & intstr(integer(The.TB_Block.all.Ancestor_Component))));
      pragma Debug(Reporter.Report("Parent_ID: " & intstr(integer(The.TB_Block.all.Parent_ID))));
      pragma Debug(Reporter.Report("Parent_Component: " & intstr(integer(The.TB_Block.all.Parent_Component))));
      pragma Debug(Reporter.Report("Self_ID: " & intstr(integer(The.TB_Block.all.Self_ID))));
      pragma Debug(Reporter.Report("Self_Component: " & intstr(integer(The.TB_Block.all.Self_Component))));
   end Report_ID_Block;

   --
    
end RASCAL.WimpTask;
