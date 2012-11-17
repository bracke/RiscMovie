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

with System;            use System;
with Kernel;            use Kernel;
with Interfaces.C;      use Interfaces.C;
with Ada.Strings.Unbounded;
with Reporter;

with RASCAL.Utility;    use RASCAL.Utility;
with RASCAL.Memory;     use RASCAL.Memory;
with RASCAL.WimpWindow; use RASCAL.WimpWindow;

package body RASCAL.ToolboxWindow is

   Toolbox_ObjectMiscOp      : constant := 16#44EC6#;

   Window_ClassSWI                     : constant := 16#082880#;
   Window_PostFilter                   : constant := 16#082881#;
   Window_PreFilter                    : constant := 16#082882#;
   Window_GetPointerInfo               : constant := 16#082883#;
   Window_Wimp_To_Toolbox              : constant := 16#082884#;
   Window_RegisterExternal             : constant := 16#082885#;
   Window_DeregisterExternal           : constant := 16#082886#;
   Window_SupportExternal              : constant := 16#082887#;
   Window_RegisterFilter               : constant := 16#082888#;
   Window_DeregisterFilter             : constant := 16#082889#;
   Window_EnumerateGadgets             : constant := 16#08288A#;
   Window_GadgetGetIconList            : constant := 16#08288B#;
   Window_ExtractGadgetInfo            : constant := 16#0828BE#;
   Window_PlotGadget                   : constant := 16#0828BF#;

   --

   function Gadget_Get_BufferSize (Object    : in Object_ID;
                                   Component : in Component_ID) return Integer is

      Icon       : Icon_Handle_Type;
      Window     : Wimp_Handle_Type;
      Buffer_Size: Integer;
      Index      : Integer := 1;
   begin
      case Get_Type (Object,Component) is
      when WritableField_Base => Index := 1;
      when DisplayField_Base  => Index := 1;
      when ActionButton_Base  => Index := 1;
      when OptionButton_Base  => Index := 1;
      when LabelledBox_Base   => Index := 1;
      when Label_Base         => Index := 1;
      when RadioButton_Base   => Index := 2;
      when others             => Index := -1;
      end case;
      if Index = -1 then
         return -1;
      end if;
      Icon        := ToolboxWindow.Gadget_Get_Icon_List(Object,Component)(Index);
      Window      := ToolboxWindow.Get_Wimp_Handle (Object);
      Buffer_Size := Memory.GetWord(WimpWindow.Get_WindowInfo(Window).Icon_Block(Integer(Icon)).Icon_Data'Address,8-4);

      return Buffer_Size;
   end Gadget_Get_BufferSize;

   --

   procedure Gadget_SetValue (Window    : in Object_ID;
                              Component : in Component_ID;
                              Value     : in String;
                              Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register    : aliased Kernel.swi_regs;
      Error       : oserror_access;

      Value_0     : UString := U(Value & ASCII.NUL);
      Buffer_Size : Integer := (Gadget_Get_BufferSize(Window,Component))-1;
      Method      : Integer;
   begin
      if Buffer_Size > -1 then
         if Value'Length > Buffer_Size then
            Value_0 := Ada.Strings.Unbounded.Head(Value_0,Buffer_Size);
            Ada.Strings.Unbounded.Append(Value_0,ASCII.NUL);
         end if;

         case Get_Type (Window,Component) is
         when WritableField_Base => Method := 512;
         when DisplayField_Base  => Method := 448;
         when ActionButton_Base  => Method := 80;
         when OptionButton_Base  => Method := 192;
         when RadioButton_Base   => Method := 384;
         when others             => Method := -1;
         end case;

         if Method /= -1 then
            Register.R(0) := Int (Unsigned_to_Int(Flags));
            Register.R(1) := Int (Window);
            Register.R(2) := Int (Method);
            Register.R(3) := Int (Component);
            Register.R(4) := Adr_To_Int (S(Value_0)'Address);
            
            Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);
            
            if Error /= null then
               pragma Debug(Reporter.Report("ToolboxWindow.Gadget_SetValue: " & To_Ada(Error.ErrMess)));
               OS.Raise_Error(Error);
            end if;
         end if;
      end if;
   end Gadget_SetValue;

   --

   function Gadget_GetBBox (Window    : in Object_ID;
                            Component : in Component_ID;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) return Toolbox_BBox_Type is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
      Buffer   : Toolbox_BBox_Type;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 72;
      Register.R(3) := int(Component);
      Register.R(4) := Adr_To_Int(Buffer'Address);

      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Gadget_GetBBox: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Buffer;
   end Gadget_GetBBox;

   --

   function Gadget_GetFlags (Window    : in Object_ID;
                             Component : in Component_ID;
                             Flags     : in System.Unsigned_Types.Unsigned := 0) return integer is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 64;
      Register.R(3) := Int(Component);

      Error := Kernel.swi(Toolbox_ObjectMiscOp,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Gadget_GetFlags: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return integer(Register.R(0));
      
   end Gadget_GetFlags;

   --

   function Get_Type (Window    : in Object_ID;
                      Component : in Component_ID;
                      Flags     : in System.Unsigned_Types.Unsigned := 0) return Gadget_ID_Type is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 70;
      Register.R(3) := Int(Component);
      Error := Kernel.swi(Toolbox_ObjectMiscOp,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Get_Type: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Gadget_ID_Type(Register.R(0));
   end Get_Type;

   --

   function Gadget_Get_Help (Window    : in Object_ID;
                             Component : in Component_ID;
                             Flags     : in System.Unsigned_Types.Unsigned := 0) return String is

      Register    : aliased Kernel.swi_regs;
      Buffer_Size : integer := 0;
      Error       : oserror_access;
   begin
      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 67;
      Register.R(3) := Int(Component);
      Register.R(4) := 0;
      Register.R(5) := 0;

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Gadget_Get_Help: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := Integer(Register.R(5));

      declare
         Buffer : String(1..Buffer_Size);
      begin
         Register.R(0) := Int(Unsigned_to_Int(Flags));
         Register.R(1) := Int(Window);
         Register.R(2) := 67;
         Register.R(3) := Int(Component);
         Register.R(4) := Adr_To_Int(Buffer'Address);
         Register.R(5) := Int(Buffer_Size);

         Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxWindow.Gadget_Get_Help: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;

         return MemoryToString(Buffer'Address);
      end;
   end Gadget_Get_Help;

   --

   function Get_Icon_List (Window    : in Object_ID;
                           Component : in Component_ID;
                           Flags     : in System.Unsigned_Types.Unsigned := 0) return Icon_List_Type is

      Register    : aliased Kernel.swi_regs;
      Buffer_Size : integer := 0;
      Error       : oserror_access;
   begin

      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 68;
      Register.R(3) := Int(Component);
      Register.R(4) := 0;
      Register.R(5) := 0;

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Get_Icon_List: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := Integer(Register.R(5));
      if Buffer_Size mod 4 > 0 then
         Buffer_Size := Buffer_Size / 4 + 1;
      else
         Buffer_Size := Buffer_Size / 4;
      end if;

      declare
         Buffer : Icon_List_Type(1..Buffer_Size);
      begin

         Register.R(0) := Int(Unsigned_to_Int(Flags));
         Register.R(1) := Int(Window);
         Register.R(2) := 68;
         Register.R(3) := Int(Component);
         Register.R(4) := Adr_To_Int(Buffer'Address);
         Register.R(5) := Int(Buffer_Size);

         Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxWindow.Get_Icon_List: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;

         return Buffer;
      end;      

   end Get_Icon_List;

   --

   procedure Gadget_SetFlags (Window    : in Object_ID;
                              Component : in Component_ID;
                              New_Flags : in System.Unsigned_Types.Unsigned;
                              Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin

      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 65;
      Register.R(3) := Int(Component);
      Register.R(4) := Int(Utility.Unsigned_To_Int(New_Flags));

      Error := Kernel.swi(Toolbox_ObjectMiscOp,register'Access,register'Access);

      if Error /=null then
         pragma Debug(Reporter.Report("ToolboxWindow.Gadget_SetFlags: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Gadget_SetFlags;

   --

   procedure Gadget_Fade (Window    : in Object_ID;
                          Component : in Component_ID;
                          Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Flag : System.Unsigned_Types.Unsigned := 16#80000000#;
   begin
      Gadget_SetFlags(Window,Component,Flag);
   end Gadget_Fade;

   --

   procedure Gadget_UnFade (Window    : in Object_ID;
                            Component : in Component_ID;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Flag : System.Unsigned_Types.Unsigned := 16#0#;
   begin
      Gadget_SetFlags(Window,Component,Flag);
   end Gadget_UnFade;

   --

   procedure Set_Focus (Window    : in Object_ID;
                        Component : in Component_ID;
                        Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin

      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 68;
      Register.R(3) := Int(Component);

      Error := Kernel.swi(Toolbox_ObjectMiscOp,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Set_Focus: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Focus;

   --

   procedure Gadget_Set_Help  (Window    : in Object_ID;
                               Component : in Component_ID;
                               Help      : in String;
                               Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register  : aliased Kernel.swi_regs;
      Error     : oserror_access;
      Help_0    : String := Help & Character'Val(0);
   begin

      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 66;
      Register.R(3) := Int(Component);
      Register.R(4) := Adr_To_Int(Help_0'Address);

      Error := Kernel.swi(Toolbox_ObjectMiscOp,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Gadget_SetHelp: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Gadget_Set_Help;

   --

   procedure Move_Gadget  (Window    : in Object_ID;
                           Component : in Component_ID;
                           BBox      : in Toolbox_BBox_Type;
                           Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin

      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 71;
      Register.R(3) := Int(Component);
      Register.R(4) := Adr_To_Int(BBox'Address);

      Error := Kernel.swi(Toolbox_ObjectMiscOp,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Move_Gadget: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Move_Gadget;

   --

-- Window Methods

   procedure Extract_GadgetInfo (Template : in Address;
                                 Gadget   : in Component_ID;
                                 Block    : out Address;
                                 BlockSize: out Integer;
                                 Flags    : in System.Unsigned_Types.Unsigned := 0) is
   
      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Adr_To_Int(Template);
      Register.R(2) := int(Gadget);

      Error := Kernel.Swi (Window_ExtractGadgetInfo, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Extract_GadgetInfo " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Block     := Int_To_Adr(Register.R(0));
      BlockSize := Integer (Register.R(1));
      
   end Extract_GadgetInfo;

   --

   function Enumerate_Gadgets (Window : in Object_ID;
                               Flags  : in System.Unsigned_Types.Unsigned := 0) return Gadget_List_Type is

      Register    : aliased Kernel.swi_regs;
      Buffer_Size : integer := 0;
      Error       : oserror_access;
   begin
      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := -1;
      Register.R(3) := 0;
      Register.R(4) := 0;

      Error := Kernel.Swi (Window_EnumerateGadgets, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Enumerate_Gadget(1): " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := integer(Register.R(4));
      if Buffer_Size mod 4 > 0 then
         Buffer_Size := Buffer_Size / 4 + 1;
      else
         Buffer_Size := Buffer_Size / 4;
      end if;

      if Buffer_Size = -1 then
         raise No_Toolbox_Window;
      end if;

      declare
         Buffer : Gadget_List_Type(1..Buffer_Size);
      begin
         Register.R(0) := Int(Unsigned_to_Int(Flags));
         Register.R(1) := Int(Window);
         Register.R(2) := -1;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := Int(Buffer_Size);
         
         Error := Kernel.Swi (Window_EnumerateGadgets, Register'Access, Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxWindow.Enumerate_Gadget(2): " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;

         if integer(Register.R(2)) /= 0 then
            raise Enumeration_Buffer_Overrun;
         end if;
         
         return Buffer;
      end;

   end Enumerate_Gadgets;

   --

   function Gadget_Get_Icon_List (Window    : in Object_ID;
                                  Component : in Component_ID;
                                  Flags     : in System.Unsigned_Types.Unsigned := 0) return Icon_List_Type is

      Register    : aliased Kernel.swi_regs;
      Buffer_Size : integer := 0;
      Error       : oserror_access;
   begin

      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 68;
      Register.R(3) := Int(Component);
      Register.R(4) := 0;
      Register.R(5) := 0;

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Gadget_Get_Icon_List: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := integer(Register.R(5));
      if Buffer_Size mod 4 > 0 then
         Buffer_Size := Buffer_Size / 4 + 1;
      else
         Buffer_Size := Buffer_Size / 4;
      end if;

      declare
         Buffer : Icon_List_Type(1..Buffer_Size);
      begin
         Register.R(0) := Int(Unsigned_to_Int(Flags));
         Register.R(1) := Int(Window);
         Register.R(2) := 68;
         Register.R(3) := Int(Component);
         Register.R(4) := Adr_To_Int(Buffer'Address);
         Register.R(5) := Int(Buffer_Size);
         
         Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxWindow.Gadget_Get_Icon_List: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         if integer(Register.R(4)) = -1 then
            raise No_Toolbox_Window;
         end if;
         
         return Buffer;
      end;

   end Gadget_Get_Icon_List;

   --

--   procedure Add_Gadget (Window : in Object_ID;
--                         Gadget : in Gadget_Type;
--                         Flags  : in System.Unsigned_Types.Unsigned := 0) is
--
--      Register : aliased Kernel.swi_regs;
--      Error    : oserror_access;
--   begin
--
--      Register.R(0) := Int(Unsigned_to_Int(Flags));
--      Register.R(1) := Int(Window);
--      Register.R(2) := 1;
--      Register.R(3) := Adr_To_Int(Gadget'Address);
--
--      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);
--
--      if Error /= null then
--         pragma Debug(Reporter.Report("ToolboxWindow.Add_Gadget: " & To_Ada(Error.ErrMess)));
--         OS.Raise_Error(Error);
--      end if;
--   end Add_Gadget;

   --

   procedure Add_Gadget (Window : in Object_ID;
                         Gadget : in Address;
                         Flags  : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin

      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 1;
      Register.R(3) := Adr_To_Int(Gadget);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Add_Gadget (2): " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Add_Gadget;

   --

   procedure Plot_Gadget (Gadget : in Address;
                          Flags  : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Adr_To_Int(Gadget);
      Error := Kernel.Swi (Window_PlotGadget, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Plot_Gadget : " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Plot_Gadget;

   --

   procedure Remove_Gadget (Window    : in Object_ID;
                            Component : in Component_ID;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin

      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 2;
      Register.R(3) := Int(Component);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Remove_Gadget: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Remove_Gadget;

   --

   procedure Set_Menu (Window  : in Object_ID;
                       Menu    : in Object_ID;
                       Flags   : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin

      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 3;
      Register.R(3) := Int(Menu);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Set_Menu: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Menu;

   --

   function Get_Menu (Window : in Object_ID;
                      Flags  : in System.Unsigned_Types.Unsigned := 0) return Object_ID is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin

      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 4;

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Get_Menu: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Object_ID(Register.R(0));

   end Get_Menu;

   --

   procedure Set_Pointer (Window  : in Object_ID;
                          Sprite  : in string;
                          X_Spot  : in integer;
                          Y_Spot  : in integer;
                          Flags   : in System.Unsigned_Types.Unsigned := 0) is

      Register    : aliased Kernel.swi_regs;
      Error       : oserror_access;
      Sprite_0    : String := Sprite & ASCII.NUL;
   begin

      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 5;
      Register.R(3) := Adr_To_Int(Sprite_0'Address);
      Register.R(4) := Int(X_Spot);
      Register.R(5) := Int(Y_Spot);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Set_Pointer: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Pointer;

   --

   function Get_Pointer (Window : in Object_ID;
                         X_Spot : in integer;
                         Y_Spot : in integer;
                         Flags  : in System.Unsigned_Types.Unsigned := 0) return String is

      Register    : aliased Kernel.swi_regs;
      Buffer_Size : integer := 0;
      Error       : oserror_access;
   begin

      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 6;
      Register.R(3) := 0;
      Register.R(4) := 0;
      Register.R(5) := Int(X_Spot);
      Register.R(6) := Int(Y_Spot);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Get_Pointer: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := Integer(Register.R(4));

      declare
         Buffer : String(1..Buffer_Size);
      begin
         Register.R(0) := Int(Unsigned_to_Int(Flags));
         Register.R(1) := Int(Window);
         Register.R(2) := 6;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := Int(Buffer_Size);
         Register.R(5) := Int(X_Spot);
         Register.R(6) := Int(Y_Spot);
         
         Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxWindow.Get_Pointer: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;

         return MemoryToString(Buffer'Address);
      end;      

   end Get_Pointer;

   --

   procedure Set_Help (Window : in Object_ID;
                       Help   : in String;
                       Flags  : in System.Unsigned_Types.Unsigned := 0) is

      Register  : aliased Kernel.swi_regs;
      Error     : oserror_access;
      Help_0    : String := Help & ASCII.NUL;
   begin
      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 7;
      Register.R(3) := Adr_To_Int(Help_0'Address);

      Error := Kernel.swi(Toolbox_ObjectMiscOp,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Set_Help: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Help;

   --

   function Get_Help (Window : in Object_ID;
                      Flags  : in System.Unsigned_Types.Unsigned := 0) return String is

      Register    : aliased Kernel.swi_regs;
      Buffer_Size : integer := 0;
      Error       : oserror_access;
   begin
      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 8;
      Register.R(3) := 0;
      Register.R(4) := 0;

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Get_Help: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := Integer(Register.R(4));

      declare
         Buffer : String(1..Buffer_Size);
      begin
         Register.R(0) := Int(Unsigned_to_Int(Flags));
         Register.R(1) := Int(Window);
         Register.R(2) := 8;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := Int(Buffer_Size);

         Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxWindow.Get_Help: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;

         return MemoryToString(Buffer'Address);
      end;      

   end Get_Help;

   --

   procedure Set_Title (Window : in Object_ID;
                        Title  : in String;
                        Flags  : in System.Unsigned_Types.Unsigned := 0) is

      Register   : aliased Kernel.swi_regs;
      Error      : oserror_access;
      Title_0    : String := Title & ASCII.NUL;
   begin
      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 11;
      Register.R(3) := Adr_To_Int(Title_0'Address);

      Error := Kernel.swi(Toolbox_ObjectMiscOp,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Set_Title: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Title;

   --

   function Get_Title (Window : in Object_ID;
                       Flags  : in System.Unsigned_Types.Unsigned := 0) return String is

      Register    : aliased Kernel.swi_regs;
      Buffer_Size : integer := 0;
      Error       : oserror_access;
   begin

      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 12;
      Register.R(3) := 0;
      Register.R(4) := 0;

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Get_Title: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := Integer(Register.R(4));

      declare
         Buffer : String(1..Buffer_Size);
      begin
         Register.R(0) := Int(Unsigned_to_Int(Flags));
         Register.R(1) := Int(Window);
         Register.R(2) := 12;
         Register.R(3) := Adr_To_Int(Buffer'Address);
         Register.R(4) := Int(Buffer_Size);

         Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("ToolboxWindow.Get_Title: " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return MemoryToString(Buffer'Address);
      end;      

   end Get_Title;

   --

   procedure Set_Default_Focus (Window    : in Object_ID;
                                Component : in Component_ID;
                                Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 13;
      Register.R(3) := Int(Component);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Set_Default_Focus: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Default_Focus;

   --

   function Get_Default_Focus (Window : in Object_ID;
                               Flags  : in System.Unsigned_Types.Unsigned := 0) return Component_ID is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin

      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 14;

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Get_Default_Focus: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Component_ID(Register.R(0));
            
   end Get_Default_Focus;

   --

   procedure Set_Extent (Window : in Object_ID;
                         BBox   : in Toolbox_BBox_Type;
                         Flags  : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 15;
      Register.R(3) := Adr_To_Int(BBox'Address);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Set_Extent: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Extent;

   --

   function Get_Extent (Window : in Object_ID;
                        Flags  : in System.Unsigned_Types.Unsigned := 0) return Toolbox_BBox_Type is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
      BBox     : Toolbox_BBox_Type;
   begin
      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 16;
      Register.R(3) := Adr_To_Int(BBox'Address);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Get_Extent: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return BBox;
            
   end Get_Extent;

   --

   procedure Force_Redraw (Window : in Object_ID;
                           BBox   : in Toolbox_BBox_Type;
                           Flags  : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := 17;
      Register.R(3) := Adr_To_Int(BBox'Address);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Force_Redraw: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Force_Redraw;

   --
   
   procedure Set_Toolbars (Window : in Object_ID;
                           Toolbar: in Object_ID;
                           Bar_Type: in Toolbox_Toolbar_Type) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(3) := 0;
      Register.R(4) := 0;
      Register.R(5) := 0;
      Register.R(6) := 0;

      case Bar_Type is
      when Internal_Bottom_Left => Register.R(0) := 1;
                                   Register.R(3) := Int(Toolbar);
      when Internal_Top_Left    => Register.R(0) := 2;
                                   Register.R(4) := Int(Toolbar);
      when External_Bottom_Left => Register.R(0) := 4;
                                   Register.R(5) := Int(Toolbar);
      when External_Top_Left    => Register.R(0) := 8;
                                   Register.R(6) := Int(Toolbar);
      end case;

      Register.R(1) := Int(Window);
      Register.R(2) := 18;

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Set_Toolbars: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Toolbars;

   --

   function Get_Toolbars (Window : in Object_ID;
                          Bar_Type: in Toolbox_Toolbar_Type) return Object_ID is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      case Bar_Type is
      when Internal_Bottom_Left => Register.R(0) := 1;
      when Internal_Top_Left    => Register.R(0) := 2;
      when External_Bottom_Left => Register.R(0) := 4;
      when External_Top_Left    => Register.R(0) := 8;
      end case;

      Register.R(1) := Int(Window);
      Register.R(2) := 19;

      Error := Kernel.Swi (Toolbox_ObjectMiscOp, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Get_Toolbars: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      case Bar_Type is
      when Internal_Bottom_Left => return Object_ID(Register.R(0));
      when Internal_Top_Left    => return Object_ID(Register.R(1));
      when External_Bottom_Left => return Object_ID(Register.R(2));
      when External_Top_Left    => return Object_ID(Register.R(3));
      end case;
   end Get_Toolbars;

   --

   function Is_Open (Window : in Object_ID) return Boolean is

      WinID : Wimp_Handle_Type := Get_Wimp_Handle (Window);
   begin
      return WimpWindow.Is_Open (WinID);
   end Is_Open;
   --

   function Get_Pointer_Info(Flags: in System.Unsigned_Types.Unsigned := 0) return Toolbox_Pointer_Info_Type is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
      Info     : Toolbox_Pointer_Info_Type;
   begin
      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Error := Kernel.Swi (Window_GetPointerInfo, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Get_Pointer_Info: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Info.X_Pos := integer(Register.R(0));
      Info.Y_Pos := integer(Register.R(1));
      Info.Buttons := integer(Register.R(2));

      return Info;
      
   end Get_Pointer_Info;

   --

   procedure Get_WindowPosition (Window : in Object_ID;
                                 X_Pos  : out Integer;
                                 Y_Pos  : out Integer) is

      WinID : Wimp_Handle_Type := Get_Wimp_Handle (Window);
   begin
      WimpWindow.Get_WindowPosition(WinID,X_Pos,Y_Pos);
   end Get_WindowPosition;

   --

   function Get_Wimp_Handle (Window : in Object_ID;
                             Flags  : in System.Unsigned_Types.Unsigned := 0) return Wimp_Handle_Type is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Unsigned_to_Int(Flags));
      Register.R(1) := int(Window);
      Register.R(2) := 0;

      Error := Kernel.swi(Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("Toolbox.Get_Wimp_Handle: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      return Wimp_Handle_Type(Register.R(0));

   end Get_Wimp_Handle;

   --

   procedure Wimp_To_Toolbox (Window    : in Wimp_Handle_Type;
                              Icon      : in Icon_Handle_Type;
                              Object    : out Object_ID;
                              Component : out Component_ID;
                              Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := Int(Unsigned_to_Int(Flags));
      Register.R(1) := Int(Window);
      Register.R(2) := Int(Icon);

      Error := Kernel.Swi (Window_Wimp_To_Toolbox, Register'Access, Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("ToolboxWindow.Wimp_To_Toolbox: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      Object := Object_ID(Register.R(0));
      Component := Component_ID(Register.R(1));
      
   end Wimp_To_Toolbox;

   --

   
end RASCAL.ToolboxWindow;

