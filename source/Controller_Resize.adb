with RASCAL.WimpTask;            use RASCAL.WimpTask;
with RASCAL.WimpWindow;          use RASCAL.WimpWindow;
with RASCAL.Toolbox;             use RASCAL.Toolbox;
with RASCAL.ToolboxWindow;       use RASCAL.ToolboxWindow;

with View_Main;                  use View_Main;
with Main;                       use Main;

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Exceptions;

with Reporter;

package body Controller_Resize is

   --

   package ToolboxWindow renames RASCAL.ToolboxWindow;
   package TB_Win        renames RASCAL.ToolboxWindow;
   package WimpWindow    renames RASCAL.WimpWindow;
   package Toolbox       renames RASCAL.Toolbox;

   --
   procedure Resize (State : in out Wimp_WindowState_Type) is

      Window : Wimp_Handle_Type := State.Window;
      VMin_X : constant Integer := State.Visible_Area_Min_X;
      VMax_Y : constant Integer := State.Visible_Area_Max_Y;
      VMin_Y : constant Integer := State.Visible_Area_Min_Y;
      VMax_X : constant Integer := State.Visible_Area_Max_X;
      VWidth : constant Integer := VMax_X-VMin_X;
      VHeight: constant Integer := VMax_Y-VMin_Y;
      Object : Object_ID;
      Gadget : Component_ID;
      Child  : Wimp_Handle_Type;
      Template          : Unbounded_String;
      WinBBox,AreaBBox  : Toolbox_BBox_Type;
      ChildExtent       : Wimp_WindowExtent_Type;
      ChildState        : Wimp_WindowState_Type;
      Child_V_X,Child_V_Y,Child_W_X,Child_W_Y,GHeight,GWidth,Topoffset,Bottumoffset : Integer;
   begin
      ToolboxWindow.Wimp_To_Toolbox(Window,0,Object,Gadget);
      Gadget       := 0;
      Topoffset    := 90;
      Bottumoffset := 0;

      -- Prevent scrolling
      State.Scroll_X_Offset := 0;
      State.Scroll_Y_Offset := 0;

      WimpWindow.Open_Window(State);

      -- Get textarea window data
      Child       := WimpWindow.Get_Top_ChildWindow(Window);
      ChildExtent := WimpWindow.Get_Extent(Child);
      ChildState  := WimpWindow.Get_WindowState(Child);
      Child_W_Y   := ChildExtent.Work_Area_Max_Y-ChildExtent.Work_Area_Min_Y;
      Child_V_Y   := ChildState.Visible_Area_Max_Y-ChildState.Visible_Area_Min_Y;
      Child_W_X   := ChildExtent.Work_Area_Max_X-ChildExtent.Work_Area_Min_X;
      Child_V_X   := ChildState.Visible_Area_Max_X-ChildState.Visible_Area_Min_X;

      -- Resize window workarea
      WinBBox.ymax := 0;
      WinBBox.ymin := -VHeight-(Child_W_Y-Child_V_Y);
      WinBBox.Xmin := 0;
      WinBBox.xmax := VWidth+(Child_W_X-Child_V_X);
      ToolboxWindow.Set_Extent(Object,WinBBox);

      -- Resize gadget
      AreaBBox := ToolboxWindow.Gadget_GetBBox (Object,Gadget);
      GWidth   := AreaBBox.xmax-AreaBBox.xmin;
      GHeight  := AreaBBox.ymax-AreaBBox.ymin;
      AreaBBox.ymin := AreaBBox.ymin - (VHeight-(GHeight+Topoffset)) + Bottumoffset;
      AreaBBox.xmax := AreaBBox.xmax + (VWidth-(GWidth+20));
      ToolboxWindow.Move_Gadget (Object,Gadget,AreaBBox);
   end Resize;

   --

   procedure Handle (The : in WEL_Reason_OpenWindow) is

      State : Wimp_WindowState_Type := The.Event.all.OpenWindow;
   begin
      Resize (State);
   exception
      when Exception_Data : others =>
           Report_Error("RESIZE",Ada.Exceptions.Exception_Information (Exception_Data));
   end Handle;
   --
        
end Controller_Resize;
