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

-- @brief Wimp Window related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with Interfaces.C; use Interfaces.C;

with RASCAL.OS;           use RASCAL.OS;
with RASCAL.Memory;       use RASCAL.Memory;

package RASCAL.WimpWindow is

   --
   -- Some of the window is out of date and needs to be redrawn.
   -- You should enter a redraw:
   -- First call Wimp_RedrawWindow and then call Wimp_GetRectangle
   -- until done.
   --
   type Reason_RedrawWindow is
   record
   Window : Wimp_Handle_Type;
   end record;
   pragma Convention (C, Reason_RedrawWindow);

   type Reason_RedrawWindow_Pointer is access Reason_RedrawWindow;

   type AWEL_Reason_RedrawWindow is abstract new
        Wimp_EventListener(Reason_Event_RedrawWindow,-1,-1) with
   record
   Event : Reason_RedrawWindow_Pointer;
   end record;

   --
   -- This message is broadcast by the Wimp when a window's close icon is shift clicked.
   --If an iconiser is absent nothing will happen.
   --If one is present it will acknowledge the message, and send Message_WindowInfo to the window.
   --
   type Message_Iconize is
   record
   Header         : Message_Event_Header;
   Window         : Wimp_Handle_Type;
   Task_Handle    : Integer;
   Title          : Char_Array(1..20);
   end record;
   pragma Convention (C, Message_Iconize);

   type Message_Iconize_Pointer is access Message_Iconize;

   type AMEL_Message_Iconize is abstract new
        Message_EventListener(Message_Event_Iconize) with
   record
   Event : Message_Iconize_Pointer;
   end record;

   --
   -- In the absence of this message, Pinboard will just position the window icon beneath the window's close button.
   --
   type Message_IconizeAt is
   record
   Header         : Message_Event_Header;
   Window         : Wimp_Handle_Type;
   Task_Handle    : Integer;
   X_Coordinate   : Integer;
   Y_Coordinate   : Integer;
   Flags          : Integer;
   end record;
   pragma Convention (C, Message_IconizeAt);

   type Message_IconizeAt_Pointer is access Message_IconizeAt;

   type AMEL_Message_IconizeAt is abstract new
        Message_EventListener(Message_Event_IconizeAt) with
   record
   Event : Message_IconizeAt_Pointer;
   end record;

   --
   -- This message is bradcasted when a window is closed, so that the iconiser can remove the icon.
   --
   type Message_WindowClosed is
   record
   Header         : Message_Event_Header;
   Window         : Wimp_Handle_Type;
   end record;
   pragma Convention (C, Message_WindowClosed);

   type Message_WindowClosed_Pointer is access Message_WindowClosed;

   type AMEL_Message_WindowClosed is abstract new
        Message_EventListener(Message_Event_WindowClosed) with
   record
   Event : Message_WindowClosed_Pointer;
   end record;

   --
   -- This message is sent by the iconiser when one of your windows is iconised, to find out which sprite and name to give to the icon.
   --
   type Message_WindowInfo is
   record
   Header      : Message_Event_Header;
   Window      : Wimp_Handle_Type;
   Reserved    : Integer;
   Sprite_Name : Char_Array(1..8);
   Title       : Char_Array(1..200);
   end record;
   pragma Convention (C, Message_WindowInfo);

   type Message_WindowInfo_Pointer is access Message_WindowInfo;

   type AMEL_Message_WindowInfo is abstract new
        Message_EventListener(Message_Event_WindowInfo) with
   record
   Event : Message_WindowInfo_Pointer;
   end record;

   type Wimp_Icon_Data is 
   record
   Min_X     : Integer;
   Min_Y     : Integer;
   Max_X     : Integer;
   Max_Y     : Integer;
   Icon_Flags: Integer;
   Icon_Data : Char_Array(1..12);
   end record;
   pragma Convention (C, Wimp_Icon_Data);

   type Double_Byte is mod 2**16;

   type Wimp_Title_Data is
   record
   Title_Data : Char_Array(1..12);
   end record;
   pragma Convention (C, Wimp_Title_Data);

   type Wimp_Window_Block_Type is
   record
   Visible_Area_Min_X       : Integer;
   Visible_Area_Min_Y       : Integer;
   Visible_Area_Max_X       : Integer;
   Visible_Area_Max_Y       : Integer;
   Scroll_X_Offset          : Integer;
   Scroll_Y_Offset          : Integer;
   Open_Behind              : Wimp_Handle_Type := Wimp_Handle_Type(-1);
   Window_Flags             : Integer;
   Title_Foreground         : Byte             := 7;
   Title_Background         : Byte             := 2;
   Work_Area_Foreground     : Byte;
   Work_Area_Background     : Byte;
   Scrollbar_Outer_Colour   : Byte             := 3;
   Scrollbar_Inner_Colour   : Byte             := 1;
   Title_Input_Focus_Colour : Byte             := 12;
   Extra_Flags              : Byte;
   Work_Area_Min_X          : Integer;
   Work_Area_Min_Y          : Integer;
   Work_Area_Max_X          : Integer;
   Work_Area_Max_Y          : Integer;
   Title_Bar_Icon_Flags     : Integer;
   Work_Area_Button_Flags   : Integer;
   Sprite_Area_Pointer      : Integer;
   Minimum_Width            : Double_Byte;
   Minimum_Height           : Double_Byte;
   Title_Data               : Wimp_Title_Data;
   Nr_Of_Icons_Initially    : Integer;
   end record;
   pragma Convention (C, Wimp_Window_Block_Type);

   type Wimp_Icon_Block_Type is array(integer range <>) of Wimp_Icon_Data;
   pragma Convention (C, Wimp_Icon_Block_Type);

   type Wimp_WindowInfo_Type(Icon_Nr : Integer) is
   record
   Window        : Wimp_Handle_Type;
   Visible_Area_Min_X       : Integer;
   Visible_Area_Min_Y       : Integer;
   Visible_Area_Max_X       : Integer;
   Visible_Area_Max_Y       : Integer;
   Scroll_X_Offset          : Integer;
   Scroll_Y_Offset          : Integer;
   Open_Behind              : Wimp_Handle_Type := Wimp_Handle_Type(-1);
   Window_Flags             : Integer;
   Title_Foreground         : Byte             := 7;
   Title_Background         : Byte             := 2;
   Work_Area_Foreground     : Byte;
   Work_Area_Background     : Byte;
   Scrollbar_Outer_Colour   : Byte             := 3;
   Scrollbar_Inner_Colour   : Byte             := 1;
   Title_Input_Focus_Colour : Byte             := 12;
   Extra_Flags              : Byte;
   Work_Area_Min_X          : Integer;
   Work_Area_Min_Y          : Integer;
   Work_Area_Max_X          : Integer;
   Work_Area_Max_Y          : Integer;
   Title_Bar_Icon_Flags     : Integer;
   Work_Area_Button_Flags   : Integer;
   Sprite_Area_Pointer      : Integer;
   Minimum_Width            : Double_Byte;
   Minimum_Height           : Double_Byte;
   Title_Data               : Wimp_Title_Data;
   Nr_Of_Icons_Initially    : Integer;
   Icon_Block               : Wimp_Icon_Block_Type(0..Icon_Nr);
   end record;
   pragma Convention (C, Wimp_WindowInfo_Type);

   type Wimp_WindowInfo_Pointer is access Wimp_WindowInfo_Type;

   type Wimp_WindowState_Type is
   record
   Window                   : Wimp_Handle_Type;
   Visible_Area_Min_X       : Integer;
   Visible_Area_Min_Y       : Integer;
   Visible_Area_Max_X       : Integer;
   Visible_Area_Max_Y       : Integer;
   Scroll_X_Offset          : Integer;
   Scroll_Y_Offset          : Integer;
   Open_Behind              : Wimp_Handle_Type := Wimp_Handle_Type(-1);
   Window_Flags             : Integer;
   end record;
   pragma Convention (C, Wimp_WindowState_Type);

   type Wimp_WindowOutline_Type is
   record
   Window      : Wimp_Handle_Type;
   Min_X       : Integer;
   Min_Y       : Integer;
   Max_X       : Integer;
   Max_Y       : Integer;
   end record;
   pragma Convention (C, Wimp_WindowOutline_Type);

   type Wimp_RedrawInfo_Type is
   record
   Window              : Wimp_Handle_Type;
   Visible_Area_Min_X  : Integer;
   Visible_Area_Min_Y  : Integer;
   Visible_Area_Max_X  : Integer;
   Visible_Area_Max_Y  : Integer;
   Scroll_X_Offset     : Integer;
   Scroll_Y_Offset     : Integer;
   Redraw_Min_X        : Integer;
   Redraw_Min_Y        : Integer;
   Redraw_Max_X        : Integer;
   Redraw_Max_Y        : Integer;
   end record;
   pragma Convention (C, Wimp_RedrawInfo_Type);

--   type Wimp_OpenWindow_Type is
--   record
--   Window                   : Wimp_Handle_Type;
--   Visible_Area_Min_X       : Integer;
--   Visible_Area_Min_Y       : Integer;
--   Visible_Area_Max_X       : Integer;
--   Visible_Area_Max_Y       : Integer;
--   Scroll_X_Offset          : Integer;
--   Scroll_Y_Offset          : Integer;
--   Open_Behind              : Wimp_Handle_Type := Wimp_Handle_Type(-1);
--   end record;
--   pragma Convention (C, Wimp_OpenWindow_Type);

   type Wimp_WindowExtent_Type is
   record
   Work_Area_Min_X : Integer;
   Work_Area_Min_Y : Integer;
   Work_Area_Max_X : Integer;
   Work_Area_Max_Y : Integer;
   end record;

   type Wimp_External_WindowInfo_Type is
   record
   Window : Wimp_Handle_Type;
   Left_Border      : Integer;
   Bottom_Border    : Integer;
   Right_Border     : Integer;
   Top_Border       : Integer;
   Back_Width       : Integer;
   Close_Width      : Integer;
   Reserved_1       : Integer;
   Title_Width      : Integer;
   Reserved_2       : Integer;
   Iconise_Width    : Integer;
   Toggle_Width     : Integer;
   Toggle_Height    : Integer;
   VUpper_Gap       : Integer;
   UpArrow_Height   : Integer;
   VWell_Height     : Integer;
   DownArrow_Height : Integer;
   VLower_Gap       : Integer;
   Adjust_Height    : Integer;
   Adjust_Width     : Integer;
   HRight_Gap       : Integer;
   RightArrow_Width : Integer;
   HWell_Width      : Integer;
   LeftArrow_Width  : Integer;
   HLeft_Gap        : Integer;
   end record;
   pragma Convention (C, Wimp_External_WindowInfo_Type);

   type Child_List_Type is array(integer range <>) of Wimp_Handle_Type;

   --
   -- Closes the window.
   --
   procedure Close_Window (Window : in Wimp_Handle_Type);

   --
   -- Open window at maksimum size.
   --
   procedure Open_WindowMax (Window : in Wimp_Handle_Type);

   --
   -- Open the window at the center of the screen.
   --
   procedure Open_WindowCentered (Window : in Wimp_Handle_Type);

   --
   -- Open window at a specific position.
   --
   procedure Open_WindowAt (Window : in Wimp_Handle_Type;
                            X      : in Integer;
                            Y      : in Integer);

   --
   -- This opens a window.
   --
   procedure Open_Window (Block : in Wimp_WindowState_Type);

   --
   -- This is called in response to a RedrawWindow request.
   --
   procedure Redraw_Window (Window : in Wimp_Handle_Type;
                            Block  : in out Wimp_RedrawInfo_Type;
                            More   : out Boolean);

   --
   -- Updates block with info  for the next rectangle to be redrawn.
   --Returns true until there is no more to be redrawn.
   --
   function Get_Rectangle (Block : in Wimp_RedrawInfo_Type) return Boolean;

   --
   -- Returns the coordinates of a rectangle which completely covers the window, borders and all.
   --
   function Get_WindowOutline (Window : in Wimp_Handle_Type) return Wimp_WindowOutline_Type;

   --
   -- Returns the complete details of the given window's state.
   --
   function Get_WindowInfo (Window : in Wimp_Handle_Type;
                            Icons  : in Boolean := true) return Wimp_WindowInfo_Type;

   --
   -- Returns extensive external (toolicons etc.) information about the window.
   --Requires RISC OS 4.
   --
   function Get_External_WindowInfo (Window : in Wimp_Handle_Type) return Wimp_External_WindowInfo_Type;

   --
   -- Returns extensive external (toolicons etc.) information about windows with line borders enabled.
   --Requires RISC OS 4.
   --
   function Get_Generic_WindowInfo return Wimp_External_WindowInfo_Type;
   
   --
   -- Reads a window's visible state.
   --
   function Get_WindowState (Window : in Wimp_Handle_Type) return Wimp_WindowState_Type;

   --
   -- Returns the parent of a child in a nested window. Returns '-1' if the window has no parent. Requires the nested windowmanager.
   --
   function Get_ParentWindow (Window : in Wimp_Handle_Type) return Wimp_Handle_Type;

   --
   -- Returns the Wimp handle of the top level window in a nested window. Requires the nested windowmanager.
   --
   function Get_AncestorWindow (Window : in Wimp_Handle_Type) return Wimp_Handle_Type;

   --
   -- Return Top_left coordinates of window in OS units.
   --
   procedure Get_WindowPosition (Window : in Wimp_Handle_Type;
                                 X_Pos  : out Integer;
                                 Y_Pos  : out Integer);

   --
   -- Is the window open ?
   --
   function Is_Open (Window : in Wimp_Handle_Type) return Boolean;

   --
   -- Converts a set of work coordinates of the given window into
   --screen coordinates.
   --
   procedure Work_To_Screen (Window   : in Wimp_Handle_Type;
                             Work_X   : in Integer;
                             Work_Y   : in Integer;
                             Screen_X : out Integer;
                             Screen_Y : out Integer);

   --
   -- Converts a set of screen coordinates into work coordinates of the
   --given window.
   --
   procedure Screen_To_Work (Window   : in Wimp_Handle_Type;
                             Screen_X : in Integer;
                             Screen_Y : in Integer;
                             Work_X   : out Integer;
                             Work_Y   : out Integer);

   --
   -- Forces the WIMP to redraw a special set of coordinates.
   --
   procedure Force_Redraw (Window : in Wimp_Handle_Type;
                           Min_X  : in Integer;
                           Min_Y  : in Integer;
                           Max_X  : in Integer;
                           Max_Y  : in Integer);

   --
   -- Force a redraw of the entire screen.
   --
   procedure Force_RedrawAll;

   --
   -- Forces the WIMP to do a redraw of the whole visible area of a
   -- given window.
   --
   procedure Force_WindowRedraw (Window : in Wimp_Handle_Type);

   --
   -- Reads window title.
   --
   function Get_WindowTitle (Window : in Wimp_Handle_Type) return String;

   --
   -- Returns window extent - window workspace size.
   --
   function Get_Extent (Window : in Wimp_Handle_Type) return Wimp_WindowExtent_Type;

   --
   -- Set window extent, visible workarea must be wholly within new workarea extent.
   --
   procedure Set_Extent (Window : in Wimp_Handle_Type;
                         Min_X  : in Integer;
                         Min_Y  : in Integer;
                         Max_X  : in Integer;
                         Max_Y  : in Integer);

   --
   -- Closes the window and deletes window definition from memory.
   --
   procedure Delete_Window (Window : in Wimp_Handle_Type);

   --
   -- Return parent window handle. Returns '-1' for none.
   --Requires the nested WIMP (3.8).
   --
   function Get_Parent (Window : in Wimp_Handle_Type) return Wimp_Handle_Type;

   --
   -- Returns window handle of top child window. Returns '-1' for none.
   --Requires the nested WIMP (3.8).
   --
   function Get_Top_ChildWindow (Window : in Wimp_Handle_Type) return Wimp_Handle_Type;

   --
   -- Returns window handle of bottom child window. Returns '-1' for none.
   --Requires the nested WIMP (3.8).
   --
   function Get_Bottom_ChildWindow (Window : in Wimp_Handle_Type) return Wimp_Handle_Type;

   --
   -- Returns window handle of sibling window below. Returns '-1' if none.
   --Requires the nested WIMP (3.8).
   --
   function Get_Sibling_Below (Window : in Wimp_Handle_Type) return Wimp_Handle_Type;

   --
   -- Returns window handle of sibling window above. Returns '-1' if none.
   --Requires the nested WIMP (3.8).
   --
   function Get_Sibling_Above (Window : in Wimp_Handle_Type) return Wimp_Handle_Type;

   --
   -- Returns the nr of child windows the window has.
   --
   function Count_Children (Window : in Wimp_Handle_Type) return Natural;

   --
   -- Returns an array of the windows child windows.
   --
   function Get_Children (Window : in Wimp_Handle_Type) return Child_List_Type;

   procedure Handle (The : in AMEL_Message_Iconize)      is abstract;
   procedure Handle (The : in AMEL_Message_IconizeAt)    is abstract;
   procedure Handle (The : in AMEL_Message_WindowInfo)   is abstract;
   procedure Handle (The : in AMEL_Message_WindowClosed) is abstract;

end RASCAL.WimpWindow;
