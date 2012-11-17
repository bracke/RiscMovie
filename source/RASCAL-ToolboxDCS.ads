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

-- @brief ToolboxDCS related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with Interfaces.C;            use Interfaces.C;
with System;                  use System;
with System.Unsigned_Types;   use System.Unsigned_Types;

with RASCAL.Toolbox;          use RASCAL.Toolbox;
with RASCAL.OS;               use RASCAL.OS;

package RASCAL.ToolboxDCS is

   --
   -- Event is raised just before the DCS window is opened.
   --
   --Type lacks union.
   --
   type Toolbox_DCS_AboutToBeShown is
   record
   Header    : Toolbox_Event_Header;
   Show_Type : Integer;
   end record;
   pragma Convention (C, Toolbox_DCS_AboutToBeShown);

   type Toolbox_DCS_AboutToBeShown_Pointer is access Toolbox_DCS_AboutToBeShown;

   type ATEL_Toolbox_DCS_AboutToBeShown is abstract new Toolbox_EventListener(Toolbox_Event_DCS_AboutToBeShown,-1,-1) with
   record
   Event : Toolbox_DCS_AboutToBeShown_Pointer;
   end record;

   --
   -- Event is raised when the user clicks on the 'Cancel' button.
   --
   type Toolbox_DCS_Cancel is
   record
   Header  : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_DCS_Cancel);

   type Toolbox_DCS_Cancel_Pointer is access Toolbox_DCS_Cancel;

   type ATEL_Toolbox_DCS_Cancel is abstract new Toolbox_EventListener(Toolbox_Event_DCS_Cancel,-1,-1) with
   record
   Event : Toolbox_DCS_Cancel_Pointer;
   end record;

   --
   -- Event is raised when the DCS window is hidden.
   --
   type Toolbox_DCS_DialogueCompleted is
   record
   Header  : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_DCS_DialogueCompleted);

   type Toolbox_DCS_DialogueCompleted_Pointer is access Toolbox_DCS_DialogueCompleted;

   type ATEL_Toolbox_DCS_DialogueCompleted is abstract new Toolbox_EventListener(Toolbox_Event_DCS_DialogueCompleted,-1,-1) with
   record
   Event : Toolbox_DCS_DialogueCompleted_Pointer;
   end record;

   --
   -- Event is raised when the user clicks on the 'Discard' button.
   --
   type Toolbox_DCS_Discard is
   record
   Header  : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_DCS_Discard);

   type Toolbox_DCS_Discard_Pointer is access Toolbox_DCS_Discard;

   type ATEL_Toolbox_DCS_Discard is abstract new Toolbox_EventListener(Toolbox_Event_DCS_Discard,-1,-1) with
   record
   Event : Toolbox_DCS_Discard_Pointer;
   end record;

   --
   -- Event is raised when the user clicks on the 'Save' button.
   --
   type Toolbox_DCS_Save is
   record
   Header  : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_DCS_Save);

   type Toolbox_DCS_Save_Pointer is access Toolbox_DCS_Save;

   type ATEL_Toolbox_DCS_Save is abstract new Toolbox_EventListener(Toolbox_Event_DCS_Save,-1,-1) with
   record
   Event : Toolbox_DCS_Save_Pointer;
   end record;

   --
   -- Returns the message used in the DCS dialog.
   --
   function Get_Message (DCS   : in Object_ID;
                         Flags : in System.Unsigned_Types.Unsigned := 0) return string;

   --
   -- Returns the title of the DCS dialog.
   --
   function Get_Title (DCS   : in Object_ID;
                       Flags : in System.Unsigned_Types.Unsigned := 0) return string;

   --
   -- Returns the id of the underlying toolbox window object for the DCS dialogue.
   --
   function Get_Window_ID (DCS   : in Object_ID;
                           Flags : in System.Unsigned_Types.Unsigned := 0) return Object_ID;

   --
   -- Sets the message used in the DCS dialogue.
   --
   procedure Set_Message (DCS       : in Object_ID;
                          Message   : in string;
                          Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the title of the DCS dialogue.
   --
   procedure Set_Title (DCS   : in Object_ID;
                        Titel : in string;
                        Flags : in System.Unsigned_Types.Unsigned := 0);

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_DCS_AboutToBeShown) is abstract;

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_DCS_Cancel) is abstract;

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_DCS_DialogueCompleted) is abstract;

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_DCS_Discard) is abstract;

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_DCS_Save) is abstract;

end RASCAL.ToolboxDCS;
