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

-- @brief ToolboxQuit types and methods.
-- $Author$
-- $Date$
-- $Revision$

with RASCAL.Toolbox;          use RASCAL.Toolbox;
with RASCAL.OS;               use RASCAL.OS;

with Interfaces.C;            use Interfaces.C;
with System.Unsigned_Types;   use System.Unsigned_Types;

package RASCAL.ToolboxQuit is

   --
   -- Raised when the Quit Dialogue is about to be shown to the user.
   --Type lacks union.
   --
   type Toolbox_Quit_AboutToBeShown is
   record
   Header    : Toolbox_Event_Header;
   Show_Type : Integer;
   end record;
   pragma Convention (C, Toolbox_Quit_AboutToBeShown);

   type Toolbox_Quit_AboutToBeShown_Pointer is access Toolbox_Quit_AboutToBeShown;

   type ATEL_Toolbox_Quit_AboutToBeShown is abstract new
            Toolbox_EventListener(Toolbox_Event_Quit_AboutToBeShown,-1,-1) with
   record
   Event : Toolbox_Quit_AboutToBeShown_Pointer;
   end record;

   --
   -- Raised when the user clicks on the Cancel action button.
   --
   type Toolbox_Quit_Cancel is
   record
   Header  : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_Quit_Cancel);

   type Toolbox_Quit_Cancel_Pointer is access Toolbox_Quit_Cancel;

   type ATEL_Toolbox_Quit_Cancel is abstract new
            Toolbox_EventListener(Toolbox_Event_Quit_Cancel,-1,-1) with
   record
   Event : Toolbox_Quit_Cancel_Pointer;
   end record;

   --
   -- Raised after the Quit object has been closed.
   --
   type Toolbox_Quit_DialogueCompleted is
   record
   Header  : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_Quit_DialogueCompleted);

   type Toolbox_Quit_DialogueCompleted_Pointer is access Toolbox_Quit_DialogueCompleted;

   type ATEL_Toolbox_Quit_DialogueCompleted is abstract new
            Toolbox_EventListener(Toolbox_Event_Quit_DialogueCompleted,-1,-1) with
   record
   Event : Toolbox_Quit_DialogueCompleted_Pointer;
   end record;

   --
   -- Raised when the user clicks on the Quit action button.
   --
   type Toolbox_Quit_Quit is
   record
   Header  : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_Quit_Quit);

   type Toolbox_Quit_Quit_Pointer is access Toolbox_Quit_Quit;

   type ATEL_Toolbox_Quit_Quit is abstract new
            Toolbox_EventListener(Toolbox_Event_Quit_Quit,-1,-1) with
   record
   Event : Toolbox_Quit_Quit_Pointer;
   end record;

   --
   -- Returns the Wimp id of the underlying window used for the quit dialogue.
   --
   function Get_Window_ID (Quit : Object_ID;
                           Flags: in System.Unsigned_Types.Unsigned := 0) return Object_ID;

   --
   -- Returns the message displayed in the quit dialogue.
   --
   function Get_Message (Quit : Object_ID;
                         Flags: in System.Unsigned_Types.Unsigned := 0) return string;

   --
   -- Returns the title of the Quit dialogue.
   --
   function Get_Title (Quit : Object_ID;
                       Flags: in System.Unsigned_Types.Unsigned := 0) return string;

   --
   -- Sets the message displayed in the quit dialogue.
   --
   procedure Set_Message (Quit      : in Object_ID;
                          Message   : in string;
                          Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the title of the Quit dialogue.
   --
   procedure Set_Title (Quit  : in Object_ID;
                        Title : in string;
                        Flags : in System.Unsigned_Types.Unsigned := 0);

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_Quit_AboutToBeShown) is abstract;

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_Quit_Cancel) is abstract;

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_Quit_DialogueCompleted) is abstract;

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_Quit_Quit) is abstract;

end RASCAL.ToolboxQuit;
