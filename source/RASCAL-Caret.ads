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

-- @brief Caret related event definitions and subprograms.
-- $Author$
-- $Date$
-- $Revision$

with RASCAL.OS; use RASCAL.OS;

package RASCAL.Caret is

   --
   -- The caret has been moved away from the window.
   --
   type Reason_LoseCaret is
   record
   Window_Handle : Wimp_Handle_Type;
   Icon_Handle   : Icon_Handle_Type;
   Caret_X_Offset: Integer;
   Caret_Y_Offset: Integer;
   Caret_Height  : Integer;
   Caret_Index   : Integer;
   end record;
   pragma Convention (C, Reason_LoseCaret);

   type Reason_LoseCaret_Pointer is access Reason_LoseCaret;

   type AWEL_Reason_LoseCaret is abstract new
        Wimp_EventListener(Reason_Event_LoseCaret,-1,-1) with
   record
   Event : Reason_LoseCaret_Pointer;
   end record;

   --
   -- The caret has been moved into the window.
   --
   type Reason_GainCaret is
   record
   Window_Handle : Wimp_Handle_Type;
   Icon_Handle   : Icon_Handle_Type;
   Caret_X_Offset: Integer;
   Caret_Y_Offset: Integer;
   Caret_Height  : Integer;
   Caret_Index   : Integer;
   end record;
   pragma Convention (C, Reason_GainCaret);

   type Reason_GainCaret_Pointer is access Reason_GainCaret;

   type AWEL_Reason_GainCaret is abstract new
        Wimp_EventListener(Reason_Event_GainCaret,-1,-1) with
   record
   Event : Reason_GainCaret_Pointer;
   end record;

   type Caret_Position_Type is
   record
   Window : Wimp_Handle_Type;
   Icon   : Icon_Handle_Type;
   X_Offset : Integer;
   Y_Offset : Integer;
   Flags    : Integer;
   Index    : Integer;
   end record;
   pragma Convention (C, Caret_Position_Type);


   --
   -- Removes the Caret from wherever it currently is.
   --
   procedure Remove;

   --
   -- Returns information on the position of thw Caret.
   --
   procedure Get_Position (Window   : out Wimp_Handle_Type;
                           Icon     : out Icon_Handle_Type;
                           X_Offset : out Integer;
                           Y_Offset : out Integer;
                           Flags    : out Integer;
                           Index    : out Integer);

   --
   -- This moves the caret to a new position.
   --If the caret is moved to a new window, a LoseCaret message is sent to the owner of the old window, and a GainCaret message is sent to the owner of the new one.
   --
   procedure Set_Position (Window   : in Wimp_Handle_Type;
                           Icon     : in Icon_Handle_Type := -1;
                           X_Offset : in Integer          := 0;
                           Y_Offset : in Integer          := 0;
                           Flags    : in Integer          := -1;
                           Index    : in Integer          := -1);

   --
   --
   --
   procedure Handle (The : in AWEL_Reason_LoseCaret)  is abstract;

   --
   --
   --
   procedure Handle (The : in AWEL_Reason_GainCaret)  is abstract;

end RASCAL.Caret;
