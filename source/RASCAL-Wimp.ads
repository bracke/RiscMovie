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

-- @brief Wimp related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with system.Unsigned_Types;
with Kernel;                 use Kernel;
with Interfaces.C;           use Interfaces.C;

with RASCAL.WimpWindow;      use RASCAL.WimpWindow;
with RASCAL.Utility;         use RASCAL.Utility;
with RASCAL.OS;              use RASCAL.OS;

package RASCAL.Wimp is

   --
   -- Nothing has happened, its just your turn.
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
   -- The window size or scrollbars have been altered. You should call
   -- Wimp_OpenWindow.
   --
   type Reason_OpenWindow is
   record
   OpenWindow : Wimp_WindowState_Type;
   end record;
   pragma Convention (C, Reason_OpenWindow);

   type Reason_OpenWindow_Pointer is access Reason_OpenWindow;

   type AWEL_Reason_OpenWindow is abstract new
        Wimp_EventListener(Reason_Event_OpenWindow,-1,-1) with
   record
   Event : Reason_OpenWindow_Pointer;
   end record;

   --
   -- The user has clicked on the close icon of the window.
   -- Call Wimp_CloseWindow to close the window.
   --
   type Reason_CloseWindow is
   record
   Window : Wimp_Handle_Type;
   end record;
   pragma Convention (C, Reason_CloseWindow);

   type Reason_CloseWindow_Pointer is access Reason_CloseWindow;

   type AWEL_Reason_CloseWindow is abstract new
        Wimp_EventListener(Reason_Event_CloseWindow,-1,-1) with
   record
   Event : Reason_CloseWindow_Pointer;
   end record;

   --
   -- The pointer is no longer over this window, either because it has been moved
   -- outside or because something has been opened in front of the window.
   --
   type Reason_PointerLeavingWindow is
   record
   Window : Wimp_Handle_Type;
   end record;
   pragma Convention (C, Reason_PointerLeavingWindow);

   type Reason_PointerLeavingWindow_Pointer is access Reason_PointerLeavingWindow;

   type AWEL_Reason_PointerLeavingWindow is abstract new
        Wimp_EventListener(Reason_Event_PointerLeavingWindow,-1,-1) with
   record
   Event : Reason_PointerLeavingWindow_Pointer;
   end record;

   --
   -- The pointer is now over the window either because it has been moved there or
   -- because it is no longer obstructed by other objects.
   --
   type Reason_PointerEnteringWindow is
   record
   Window : Wimp_Handle_Type;
   end record;
   pragma Convention (C, Reason_PointerEnteringWindow);

   type Reason_PointerEnteringWindow_Pointer is access Reason_PointerEnteringWindow;

   type AWEL_Reason_PointerEnteringWindow is abstract new
        Wimp_EventListener(Reason_Event_PointerEnteringWindow,-1,-1) with
   record
   Event : Reason_PointerEnteringWindow_Pointer;
   end record;

   --
   -- The mouse has been clicked.
   --
   type Reason_MouseClick is
   record
   Mouse_X       : Integer;
   Mouse_Y       : Integer;
   Buttons       : Integer;
   Window        : Wimp_Handle_Type;
   Icon          : Icon_Handle_Type;
   Button_State  : Integer;
   end record;
   pragma Convention (C, Reason_MouseClick);
   type Reason_MouseClick_Pointer is access Reason_MouseClick;

   type AWEL_Reason_MouseClick is abstract new
        Wimp_EventListener(Reason_Event_MouseClick,-1,-1) with
   record
   Event : Reason_MouseClick_Pointer;
   end record;

   --
   -- All buttons have been released to finish a user drag operation.
   -- The coordinates are the final position of the dragbox.
   -- Call Wimp_GetPointerInfo to find out where the user dropped the box.
   --
   type Reason_UserDrag is
   record
   Min_X       : System.Unsigned_Types.Unsigned;
   Min_Y       : System.Unsigned_Types.Unsigned;
   Max_X       : System.Unsigned_Types.Unsigned;
   Max_Y       : System.Unsigned_Types.Unsigned;
   end record;
   pragma Convention (C, Reason_UserDrag);
   type Reason_UserDrag_Pointer is access Reason_UserDrag;

   type AWEL_Reason_UserDrag is abstract new
        Wimp_EventListener(Reason_Event_UserDrag,-1,-1) with
   record
   Event : Reason_UserDrag_Pointer;
   end record;

   --
   -- The user has pressed a key.
   -- If you are not interested in this keypress, then pass it on
   -- with Wimp_ProcessKey.
   --
   type Reason_KeyPressed is
   record
   Window        : Wimp_Handle_Type;
   Icon_Handle   : Icon_Handle_Type;
   Caret_X_Offset: Integer; -- relative to
   Caret_Y_Offset: Integer; -- Window origin
   Caret_Height  : Integer; -- Caret height and flags
   Caret_Index   : Integer; -- Index into string (if icon)
   Character_Code: Integer;
   end record;
   pragma Convention (C, Reason_KeyPressed);

   type Reason_KeyPressed_Pointer is access Reason_KeyPressed;

   type AWEL_Reason_KeyPressed is abstract new
        Wimp_EventListener(Reason_Event_KeyPressed,-1,-1) with
   record
   Event : Reason_KeyPressed_Pointer;
   end record;

   --
   -- The user wants to scroll the window.
   -- Use the scroll directions to update the scroll offsets,
   -- and then call Wimp_OpenWindow.
   --
   type Reason_ScrollRequest is
   record
   Window          : Wimp_Handle_Type;
   Visible_Min_X   : System.Unsigned_Types.Unsigned;
   Visible_Min_Y   : System.Unsigned_Types.Unsigned;
   Visible_Max_X   : System.Unsigned_Types.Unsigned;
   Visible_Max_Y   : System.Unsigned_Types.Unsigned;
   Scroll_Offset_X : System.Unsigned_Types.Unsigned;
   Scroll_Offset_Y : System.Unsigned_Types.Unsigned;
   Open_Behind     : Wimp_Handle_Type;
   Scroll_Direction_X : Integer;
   Scroll_Direction_Y : Integer;
   end record;
   pragma Convention (C, Reason_ScrollRequest);

   type Reason_ScrollRequest_Pointer is access Reason_ScrollRequest;

   type AWEL_Reason_ScrollRequest is abstract new
        Wimp_EventListener(Reason_Event_ScrollRequest,-1,-1) with
   record
   Event : Reason_ScrollRequest_Pointer;
   end record;

   procedure Handle (The : in AWEL_Reason_PointerLeavingWindow)     is abstract;
   procedure Handle (The : in AWEL_Reason_PointerEnteringWindow)    is abstract;

   procedure Handle (The : in AWEL_Reason_OpenWindow)               is abstract;
   procedure Handle (The : in AWEL_Reason_CloseWindow)              is abstract;

   procedure Handle (The : in AWEL_Reason_RedrawWindow)             is abstract;


   procedure Handle (The : in AWEL_Reason_ScrollRequest)            is abstract;

   procedure Handle (The : in AWEL_Reason_UserDrag)                 is abstract;
   procedure Handle (The : in AWEL_Reason_MouseClick)               is abstract;
    
   wimpblocksize        : constant := 63;
   wimpmessagesize      : constant := 255;
   
   type Mouse_Button_Type is
         (none,right,middle,right_middle,left,right_left,middle_left);
   
   type taskhandletyp is new Integer;
   type fontarraytyp is new Integer;
   type pollmask is new Integer;
   type reasoncodetyp is new Integer;
   type wimpblocktyp is array(0..wimpblocksize) of Integer;
   type wimpmessagetyp is array (0..wimpmessagesize) of Character;
   type nullmemtyp is array (0..1) of Integer;
   
   nullmemadr          : int;
   validation_r4_str   : String := "R4" & Character'Val(0);
   validation_r4       : Integer;
   validation_r53_str  : String := "R5,3" & Character'Val(0);
   validation_r53      : Integer;
   texticonbufferadr   : Integer;
   
   taskid              : constant := 16#4B534154#;
   nullmem             : nullmemtyp;
   
   blk                 : wimpblocktyp;
   blkadr              : int;
   
   wimpmessage         : wimpmessagetyp;
   wimpmessageadr      : int;
   
   register            : aliased Kernel.swi_regs;
   currentosversion    : Integer;
   
   fertig              : Boolean := false;
   
   wimp_appname : Ustring;

end RASCAL.Wimp;
