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

with Kernel;            use Kernel;
with Interfaces.C;      use Interfaces.C;
with Reporter;

with RASCAL.Utility;    use RASCAL.Utility;
with RASCAL.Memory;     use RASCAL.Memory;
with RASCAL.OS;
with RASCAL.Mode;

package body RASCAL.WimpWindow is

   Wimp_Extend           : constant := 16#400FB#;

   Wimp_GetWindowInfo    : constant := 16#400CC#;
   Wimp_GetWindowState   : constant := 16#400CB#;
   Wimp_GetWindowOutline : constant := 16#400E0#;

   Wimp_CreateWindow     : constant := 16#400C1#;
   Wimp_DeleteWindow     : constant := 16#400C3#;
   Wimp_OpenWindow       : constant := 16#400C5#;
   Wimp_CloseWindow      : constant := 16#400C6#;
   Wimp_SetExtent        : constant := 16#400D7#;
   
   Wimp_SetCaretPosition : constant := 16#400D2#;
   Wimp_GetCaretPosition : constant := 16#400D3#;
   
   Wimp_RedrawWindow     : constant := 16#400C8#;
   Wimp_UpdateWindow     : constant := 16#400C9#;
   Wimp_GetRectangle     : constant := 16#400CA#;
   Wimp_ForceRedraw      : constant := 16#400D1#;

   --

   procedure Open_WindowMax (Window : in Wimp_Handle_Type) is

      Info         : Wimp_WindowInfo_Type(0) := Get_WindowInfo(Window,false);
      Open         : Wimp_WindowState_Type;
   begin
      Open.Visible_Area_Max_Y := Info.Visible_Area_Max_Y;
      Open.Visible_Area_Min_X := Info.Visible_Area_Min_X;
      Open.Visible_Area_Min_Y := Info.Visible_Area_Max_Y + Info.Work_Area_Min_Y;
      if Open.Visible_Area_Min_Y < 40 then
         Open.Visible_Area_Min_Y := 40;
      end if;
      Open.Visible_Area_Max_X := Info.Visible_Area_Min_X + Info.Work_Area_Max_X;
      Open.Window := Window;
      Open_Window (Open);
   end Open_WindowMax;

   --

   procedure Open_WindowCentered (Window : in Wimp_Handle_Type) is

      right,top    : Integer;
      width,height : Integer;
      x,y          : Integer;
      
      Info         : Wimp_WindowInfo_Type(0) := Get_WindowInfo(Window,false);
      Open         : Wimp_WindowState_Type;
   begin
      Right := Mode.Get_X_Resolution;
      Top   := Mode.Get_Y_Resolution;

      width  := Info.Visible_Area_Min_X - Info.Visible_Area_Max_X;
      height := Info.Visible_Area_Max_Y - Info.Visible_Area_Min_Y;

      x := (right - width) / 2 + width;
      y := (top - height) / 2 + height;

      Open.Visible_Area_Min_X := x;
      Open.Visible_Area_Min_Y := y - height;
      Open.Visible_Area_Max_X := x - width;
      Open.Visible_Area_Max_Y := y;

      Open.Window := Window;
      Open_Window (Open);
   end Open_WindowCentered;

   --

   procedure Open_WindowAt (Window : in Wimp_Handle_Type;
                            x      : in Integer;
                            y      : in Integer) is
                            
      width,height : Integer;
      Info         : Wimp_WindowInfo_Type(0) := Get_WindowInfo(Window,false);
      Open         : Wimp_WindowState_Type;
   begin
      width  := Info.Visible_Area_Min_X - Info.Visible_Area_Max_X;
      height := Info.Visible_Area_Max_Y - Info.Visible_Area_Min_Y;

      Open.Visible_Area_Min_X := x;
      Open.Visible_Area_Min_Y := y - height;
      Open.Visible_Area_Max_X := x - width;
      Open.Visible_Area_Max_Y := y;

      Open.Window := Window;
      Open_Window (Open);
   end Open_WindowAt;

   --

   procedure Open_Window (Block : in Wimp_WindowState_Type) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(1) := Adr_To_Int(Block'Address);
      Error := Kernel.swi(Wimp_OpenWindow,register'Access,register'Access);

      if Error /=null then
         pragma Debug(Reporter.Report("WimpWindow.Open_Window: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Open_Window;

   --

   procedure Close_Window (Window : in Wimp_Handle_Type) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(1) := Int (Window);
      Error := Kernel.swi(Wimp_CloseWindow,register'Access,register'Access);

      if Error /=null then
         pragma Debug(Reporter.Report("WimpWindow.Close_Window: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

   end Close_Window;

   --

   procedure Redraw_Window (Window : in Wimp_Handle_Type;
                            Block  : in out Wimp_RedrawInfo_Type;
                            More   : out Boolean)  is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Block.Window  := Window;
      Register.R(1) := Adr_To_Int(Block'Address);
      Error         := Kernel.SWI(Wimp_RedrawWindow,Register'Access,Register'Access);
      More := Error = null;
   end Redraw_Window;

   --

   function Get_Rectangle (Block : in Wimp_RedrawInfo_Type) return Boolean is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := 0;
      Register.R(1) := Adr_To_Int(Block'Address);
      Error := Kernel.swi(Wimp_GetRectangle,register'Access,register'Access);

      if Error /=null then
         pragma Debug(Reporter.Report("WimpWindow.Get_Rectangle: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);         
      end if;
      return Integer(Register.R(0)) /= 0;
   end Get_Rectangle;

   --

   function Get_WindowOutline (Window : in Wimp_Handle_Type) return Wimp_WindowOutline_Type is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;

      Block    : Wimp_WindowOutline_Type;
   begin
      Block.Window  := Window;  -- workaround for 32bit-bug
      Memory.PutWord(Integer(Window),Block'Address,0);
      Register.R(1) := Adr_To_Int(Block'Address);

      Error := Kernel.swi(Wimp_GetWindowOutline,register'Access,register'Access);

      if Error /=null then
         pragma Debug(Reporter.Report("WimpWindow.Get_WindowOutline: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Block;
   end Get_WindowOutline;

   --

   function Get_WindowState (Window : in Wimp_Handle_Type) return Wimp_WindowState_Type is
   
      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;

      Block    : Wimp_WindowState_Type;
   begin
      Block.Window  := Window;  -- workaround for 32bit-bug
      Memory.PutWord(Integer(Window),Block'Address,0);
      Register.R(1) := Adr_To_Int(Block'Address);

      Error := Kernel.swi(Wimp_GetWindowState,register'Access,register'Access);

      if Error /=null then
         pragma Debug(Reporter.Report("WimpWindow.Get_WindowState: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Block;
   end Get_WindowState;

   --

   function Get_ParentWindow (Window : in Wimp_Handle_Type) return Wimp_Handle_Type is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;

      Block    : Wimp_WindowState_Type;
   begin
      Block.Window  := Window;  -- workaround for 32bit-bug
      Memory.PutWord(Integer(Window),Block'Address,0);
      Register.R(1) := Adr_To_Int(Block'Address);
      Register.R(2) := 16#4B534154#;

      Error := Kernel.swi(Wimp_GetWindowState,register'Access,register'Access);

      if Error /=null then
         pragma Debug(Reporter.Report("WimpWindow.Get_ParentWindow: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Wimp_Handle_Type(Register.R(3));
   end Get_ParentWindow;

   --

   function Get_AncestorWindow (Window : in Wimp_Handle_Type) return Wimp_Handle_Type is

      Child  : Wimp_Handle_Type := Window;
      Parent : Wimp_Handle_Type;
   begin
      loop
         Parent := Get_ParentWindow(Child);
         -- Child has no parent if GetParent returns -1
         exit when Integer(Parent) = -1;
         Child := Parent;
      end loop;
      return Child;
   end Get_AncestorWindow;

   --

   function Get_WindowInfo(Window : in Wimp_Handle_Type;
                           Icons  : in Boolean := true) return Wimp_WindowInfo_Type is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;

      Icon_Nr  : Integer := 0;
      NoIcons  : Wimp_WindowInfo_Type (0);      
   begin
      NoIcons.Window := Window;
      Register.R(1)  := Adr_To_Int(NoIcons.Window'Address);
      Register.R(1)  := Register.R(1) + 1; -- Set 'No icons' flag

      Error := Kernel.swi(Wimp_GetWindowInfo,register'Access,register'Access);

      if Error /=null then
         pragma Debug(Reporter.Report("WimpWindow.Get_WindowInfo (I): " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      if not Icons then
         return NoIcons;
      end if;      

      Icon_Nr := NoIcons.Nr_Of_Icons_Initially;
      declare
         Block : Wimp_WindowInfo_Type(Icon_Nr);
      begin
         Block.Window := Window;
         Register.R(1)  := Adr_To_Int(Block.Window'Address);         
         Error := Kernel.swi(Wimp_GetWindowInfo,register'Access,register'Access);
         
         if Error /=null then
            pragma Debug(Reporter.Report("WimpWindow.Get_WindowInfo (II): " & To_Ada(Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return Block;
      end;
   end Get_WindowInfo;

   --

   function Get_External_WindowInfo (Window : in Wimp_Handle_Type) return Wimp_External_WindowInfo_Type is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
      Info     : Wimp_External_WindowInfo_Type;
   begin
      Info.Window := Window;
      Register.R(0) := 11;
      Register.R(1) := Adr_To_Int(Info'Address);
      Error := Kernel.swi(Wimp_Extend,register'Access,register'Access);

      if Error /=null then
         pragma Debug(Reporter.Report("WimpWindow.Get_External_WindowInfo: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Info;
   end Get_External_WindowInfo;

   --

   function Get_Generic_WindowInfo return Wimp_External_WindowInfo_Type is
   begin
      return Get_External_WindowInfo(0);
   end Get_Generic_WindowInfo;

   --

   procedure Get_WindowPosition (Window : in Wimp_Handle_Type;
                                 X_Pos  : out Integer;
                                 Y_Pos  : out Integer) is

      State : Wimp_WindowState_Type := WimpWindow.Get_WindowState (Window);
   begin
      X_Pos := State.Visible_Area_Min_X;
      Y_Pos := State.Visible_Area_Max_Y;
   end Get_WindowPosition;

   --

   function Is_Open (Window : in Wimp_Handle_Type) return Boolean is
   
      Register : aliased Kernel.swi_regs;
      Error    : Kernel.oserror_access;
      Block    : Wimp_WindowState_Type;
   begin
      Block.Window := Window;
      -- 32bit - bug workaround
      Memory.PutWord(Integer(Window),Block'Address,0);
      Register.R(1) := Adr_To_Int(Block'Address);
      Error := Kernel.SWI (Wimp_GetWindowState, Register'Access, Register'Access);
      return ((Block.Window_Flags and 2#10000000000000000#) > 0);
   end Is_Open;

   --

   procedure Work_To_Screen (Window   : in Wimp_Handle_Type;
                             Work_X   : in Integer;
                             Work_Y   : in Integer;
                             Screen_X : out Integer;
                             Screen_Y : out Integer) is
   
      Block : Wimp_WindowState_Type := Get_WindowState (Window);
   begin
      screen_x := work_x - (Block.Scroll_X_Offset - Block.Visible_Area_Min_X);
      screen_y := work_y - (Block.Scroll_Y_Offset - Block.Visible_Area_Max_Y);
   end Work_To_Screen;

   --

   procedure Screen_To_Work (Window   : in Wimp_Handle_Type;
                             Screen_X : in Integer;
                             Screen_Y : in Integer;
                             Work_X   : out Integer;
                             Work_Y   : out Integer) is
   
      Block : Wimp_WindowState_Type := Get_WindowState (Window);
   begin
      work_x := screen_x + (Block.Scroll_X_Offset - Block.Visible_Area_Min_X);
      work_y := screen_y + (Block.Scroll_Y_Offset - Block.Visible_Area_Max_Y);
   end Screen_To_Work;

   --

   procedure Force_Redraw (Window : in Wimp_Handle_Type;
                           Min_X  : in Integer;
                           Min_Y  : in Integer;
                           Max_X  : in Integer;
                           Max_Y  : in Integer) is

      Register : aliased Kernel.swi_regs;
      Error    : Kernel.oserror_access;
   begin
      Register.R(0) := Int(Window);
      Register.R(1) := Int(Min_X);
      Register.R(2) := Int(Min_Y);
      Register.R(3) := Int(Max_X);
      Register.R(4) := Int(Max_Y);
      Error := Kernel.swi (Wimp_ForceRedraw, register'Access, register'Access);
      if Error /=null then
         pragma Debug(Reporter.Report("WimpWindow.ForceRedraw: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Force_Redraw;

   --

   procedure Force_RedrawAll is
   begin
      Force_Redraw (-1,0,0,9999,9999);
   end Force_RedrawAll;

   --

   procedure Force_WindowRedraw (Window : in Wimp_Handle_Type) is

      Register : aliased Kernel.swi_regs;
      Error    : Kernel.oserror_access;
      Info     : Wimp_WindowInfo_Type(0) := Get_WindowInfo(Window,false);
   begin
      Register.R(0) := Int(Window);
      Register.R(1) := Int(Info.Work_Area_Min_X);
      Register.R(2) := Int(Info.Work_Area_Min_Y);
      Register.R(3) := Int(Info.Work_Area_Max_X);
      Register.R(4) := Int(Info.Work_Area_Max_Y);
      Error := Kernel.swi (Wimp_ForceRedraw, register'Access, register'Access);
      if Error /=null then
         pragma Debug(Reporter.Report("WimpWindow.Force_WindowRedraw: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Force_WindowRedraw;

   --

   function Get_WindowTitle (Window : in Wimp_Handle_Type) return String is

      Info : Wimp_WindowInfo_Type(0) := Get_WindowInfo(Window,false);
   begin
      return To_Ada(Info.Title_Data.Title_Data);
   end Get_WindowTitle;

   --

   function Get_Extent (Window : in Wimp_Handle_Type) return Wimp_WindowExtent_Type is

      Extent   : Wimp_WindowExtent_Type;
      Info     : Wimp_WindowInfo_Type(0) := Get_WindowInfo(Window,false);
   begin
      Extent.Work_Area_Min_X := Info.Work_Area_Min_X;
      Extent.Work_Area_Min_Y := Info.Work_Area_Min_Y;
      Extent.Work_Area_Max_X := Info.Work_Area_Max_X;
      Extent.Work_Area_Max_Y := Info.Work_Area_Max_Y;
      return Extent;
   end Get_Extent;

   --

   procedure Set_Extent (Window : in Wimp_Handle_Type;
                         Min_X  : in Integer;
                         Min_Y  : in Integer;
                         Max_X  : in Integer;
                         Max_Y  : in Integer) is

      Register : aliased Kernel.swi_regs;
      Error    : Kernel.oserror_access;
      Extent   : Wimp_WindowExtent_Type;
   begin
      Extent.Work_Area_Min_X := Min_X;
      Extent.Work_Area_Min_Y := Min_Y;
      Extent.Work_Area_Max_X := Max_X;
      Extent.Work_Area_Max_Y := Max_Y;

      Register.R(0) := Int(Window);
      Register.R(1) := Adr_To_Int(Extent'Address);
      Error := Kernel.swi (Wimp_SetExtent, Register'Access, Register'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("WimpWindow.Set_Extent: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Extent;

   --

   procedure Delete_Window (Window : in Wimp_Handle_Type) is

      Register : aliased Kernel.swi_regs;
      Error    : Kernel.oserror_access;
      Block    : Wimp_WindowOutline_Type;
   begin
      Block.Window := Window;
      Register.R(1) := Adr_To_Int(Block'Address);
      Error := Kernel.SWI (Wimp_DeleteWindow, Register'Access, Register'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("WimpWindow.Delete_Window: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Delete_Window;

   --

   function Get_Parent (Window : in Wimp_Handle_Type) return Wimp_Handle_Type is

      Register : aliased Kernel.swi_regs;
      Error    : Kernel.oserror_access;
   begin
      Register.R(0)  := 6;
      Register.R(1)  := Int(Window);
      Error := Kernel.swi(Wimp_Extend,register'Access,register'Access);

      if Error /=null then
         pragma Debug(Reporter.Report("WimpWindow.Get_Parent: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Wimp_Handle_Type(Register.R(1));
   end Get_Parent;

   --

   function Get_Top_ChildWindow (Window : in Wimp_Handle_Type) return Wimp_Handle_Type is
   
      Register : aliased Kernel.swi_regs;
      Error    : Kernel.oserror_access;
   begin
      Register.R(0)  := 7;
      Register.R(1)  := Int(Window);
      Error := Kernel.swi(Wimp_Extend,register'Access,register'Access);

      if Error /=null then
         pragma Debug(Reporter.Report("WimpWindow.Get_Top_ChildWindow: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Wimp_Handle_Type(Register.R(1));
   end Get_Top_ChildWindow;

   --

   function Get_Bottom_ChildWindow (Window : in Wimp_Handle_Type) return Wimp_Handle_Type is
   
      Register : aliased Kernel.swi_regs;
      Error    : Kernel.oserror_access;
   begin
      Register.R(0)  := 8;
      Register.R(1)  := Int(Window);
      Error := Kernel.swi(Wimp_Extend,register'Access,register'Access);

      if Error /=null then
         pragma Debug(Reporter.Report("WimpWindow.Get_Bottom_ChildWindow: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Wimp_Handle_Type(Register.R(1));
   end Get_Bottom_ChildWindow;

   --

   function Get_Sibling_Below (Window : in Wimp_Handle_Type) return Wimp_Handle_Type is
   
      Register : aliased Kernel.swi_regs;
      Error    : Kernel.oserror_access;
   begin
      Register.R(0)  := 9;
      Register.R(1)  := Int(Window);
      Error := Kernel.swi(Wimp_Extend,register'Access,register'Access);

      if Error /=null then
         pragma Debug(Reporter.Report("WimpWindow.Get_Sibling_Below: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Wimp_Handle_Type(Register.R(1));
   end Get_Sibling_Below;

   --

   function Get_Sibling_Above (Window : in Wimp_Handle_Type) return Wimp_Handle_Type is
   
      Register : aliased Kernel.swi_regs;
      Error    : Kernel.oserror_access;
   begin
      Register.R(0)  := 10;
      Register.R(1)  := Int(Window);
      Error := Kernel.swi(Wimp_Extend,register'Access,register'Access);

      if Error /=null then
         pragma Debug(Reporter.Report("WimpWindow.Get_Sibling_Above: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Wimp_Handle_Type(Register.R(1));
   end Get_Sibling_Above;

   --

   function Count_Children (Window : in Wimp_Handle_Type) return Natural is

      Count : Natural := 0;
      Child : Wimp_Handle_Type;
   begin
      Child := Get_Top_ChildWindow(Window);
      while Child /= -1 loop
         Count := Count + 1;
         Child := Get_Sibling_Below (Child);
      end loop;
      return Count;
   end Count_Children;

   --

   function Get_Children (Window : in Wimp_Handle_Type) return Child_List_Type is

      Count : constant Natural := Count_Children (Window);
      List  : Child_List_Type (1..Count);
      Child : Wimp_Handle_Type;
   begin
      Child := Get_Top_ChildWindow(Window);
      for i in List'range loop
         List (i) := Child;
         Child := Get_Sibling_Below (Child);
      end loop;
      return List;
   end Get_Children;

   --
   
end RASCAL.WimpWindow;
