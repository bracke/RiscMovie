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

-- @brief Toolbox Menu related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with System;                  use System;
with System.Unsigned_Types;   use System.Unsigned_Types;
with Interfaces.C;            use Interfaces.C;

with RASCAL.Toolbox;          use RASCAL.Toolbox;
with RASCAL.OS;               use RASCAL.OS;

package RASCAL.ToolboxMenu is

   type TopLeft_Type is
   record
   X   : Integer;
   Y   : Integer;
   end record;
   pragma Convention (C, TopLeft_Type);
   
   type Menu_Entry_Type is
   record
   Flags          : integer:= 0;
   ComponentID    : Component_ID;
   Name           : Address;
   Max_Text       : integer;
   Click_Show     : integer;
   Submenu_Show   : integer;
   Submenu_Event  : integer;
   Click_Event    : integer;
   Help_Message   : Address;
   Max_Entry_Help : integer;
   end record;

   type Menu_Entry_Pointer is access Menu_Entry_Type;

   type Menu_Show_Type  is (Persistent,Transient);
   type Menu_Fade_Type  is (Unfaded,Faded);
   type Menu_Tick_Type  is (Unticked,Ticked);
   
   --
   -- Event is raised due to a call to {fcode}Toolbox_ShowObject{f} on a menu.
   --
   type Toolbox_Menu_AboutToBeShown is
   record
   Header     : Toolbox_Event_Header;
   Show_Type  : Integer;
   Position   : TopLeft_Type;
   end record;
   pragma Convention (C, Toolbox_Menu_AboutToBeShown);

   type Toolbox_Menu_AboutToBeShown_Pointer is access Toolbox_Menu_AboutToBeShown;

   type ATEL_Toolbox_Menu_AboutToBeShown is abstract new Toolbox_EventListener(Toolbox_Event_Menu_AboutToBeShown,-1,-1) with
   record
   Event : Toolbox_Menu_AboutToBeShown_Pointer;
   end record;

   --
   -- Event is raised due to a call to {fcode}Toolbox_HideObject{f} on a menu.
   --
   type Toolbox_Menu_HasBeenHidden is
   record
   Header  : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_Menu_HasBeenHidden);

   type Toolbox_Menu_HasBeenHidden_Pointer is access Toolbox_Menu_HasBeenHidden;

   type ATEL_Toolbox_Menu_HasBeenHidden is abstract new Toolbox_EventListener(Toolbox_Event_Menu_HasBeenHidden,-1,-1) with
   record
   Event : Toolbox_Menu_HasBeenHidden_Pointer;
   end record;

   --
   -- Event is raised when the user moves the mouse over a sub-menu's arrow icon.
   --
   type Toolbox_Menu_SubMenu is
   record
   Header  : Toolbox_Event_Header;
   Position: TopLeft_Type;
   end record;
   pragma Convention (C, Toolbox_Menu_SubMenu);

   type Toolbox_Menu_SubMenu_Pointer is access Toolbox_Menu_SubMenu;

   type ATEL_Toolbox_Menu_SubMenu is abstract new Toolbox_EventListener(Toolbox_Event_Menu_SubMenu,-1,-1) with
   record
   Event : Toolbox_Menu_SubMenu_Pointer;
   end record;

   --
   -- Event is raised when the user makes a selection in a menu object.
   --
   type Toolbox_Menu_Selection is
   record
   Header  : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_Menu_Selection);

   type Toolbox_Menu_Selection_Pointer is access Toolbox_Menu_Selection;

   type ATEL_Toolbox_Menu_Selection is abstract new Toolbox_EventListener(Toolbox_Event_Menu_Selection,-1,-1) with
   record
   Event : Toolbox_Menu_Selection_Pointer;
   end record;

   --
   -- Defines a menu content using a string like the stringset gadget.
   --
   procedure Set_Available (Menu      : in Object_ID;
                            Available : in String;
                            Items     : in out Natural;
                            Event     : in Integer := 16#828C3#;
                            Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Adds a new menu entry.
   --
   procedure Add_Last_Entry (Menu          : in Object_ID;
                             Name          : in String;
                             Id            : in Component_ID;
                             Help          : in String := "";
                             Click_Event   : in integer := 16#828C3#;
                             Click_Show    : in Object_ID := Object_ID'Val(0);
                             Submenu_Event : in integer := 16#828C2#;
                             Submenu_Show  : in Object_ID := Object_ID'Val(0);
                             Flags         : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Adds a new menu entry.
   --
   procedure Add_After_Entry (Menu         : in Object_ID;
                             Name          : in String;
                             Id            : in Component_ID;
                             Next_ID       : in Component_ID;
                             Help          : in String := "";
                             Click_Event   : in integer := 16#828C3#;
                             Click_Show    : in Object_ID := Object_ID'Val(0);
                             Submenu_Event : in integer := 16#828C2#;
                             Submenu_Show  : in Object_ID := Object_ID'Val(0);
                             Flags         : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the Toolbox event to be raised when the user selects the given menu entry.
   --
   function Get_Click_Event (Menu      : in Object_ID;
                             Component : in Component_ID;
                             Flags     : in System.Unsigned_Types.Unsigned := 0) return Toolbox_Event_Code_Type;

   --
   -- Returns the ObjectID of the object to be shown when the user selects the menu event.
   --
   function Get_Click_Show (Menu      : in Object_ID;
                            Component : in Component_ID;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) return Object_ID;

   --
   --
   --
   function Get_Click_Show_Type (Menu      : in Object_ID;
                                 Component : in Component_ID;
                                 Flags     : in System.Unsigned_Types.Unsigned := 0) return Menu_Show_Type;

   --
   -- Returns the interactive help message for that menu entry.
   --
   function Get_Entry_Help (Menu      : in Object_ID;
                            Component : in Component_ID;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) return string;

   --
   -- Returns the sprite which is to be used in the menu entry.
   --
   function Get_Entry_Sprite (Menu      : in Object_ID;
                              Component : in Component_ID;
                              Flags     : in System.Unsigned_Types.Unsigned := 0) return string;

   --
   -- Returns the text which is to be used in the menu entry.
   --
   function Get_Entry_Text (Menu      : in Object_ID;
                            Component : in Component_ID;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) return String;

   --
   -- Returns the fade state of the menu.
   --
   function Get_Fade (Menu      : in Object_ID;
                      Component : in Component_ID;
                      Flags     : in System.Unsigned_Types.Unsigned := 0) return Menu_Fade_Type;

   --
   -- Returns the height of work area of the given menu.
   --
   function Get_Height (Menu      : in Object_ID;
                        Flags     : in System.Unsigned_Types.Unsigned := 0) return integer;

   --
   -- Returns the interactive help message for the menu.
   --
   function Get_Help (Menu      : in Object_ID;
                      Flags     : in System.Unsigned_Types.Unsigned := 0) return string;

   --
   -- Returns the event to be raised when the user moves the mouse over a submenu arrow.
   --
   function Get_Sub_Menu_Event (Menu      : in Object_ID;
                                Component : in Component_ID;
                                Flags     : in System.Unsigned_Types.Unsigned := 0) return Toolbox_Event_Code_Type;

   --
   -- Returns the objectid of the object that will be shown when the user enters a submenu.
   --
   function Get_Sub_Menu_Show (Menu      : in Object_ID;
                               Component : in Component_ID;
                               Flags     : in System.Unsigned_Types.Unsigned := 0) return Object_ID;

   --
   -- Returns the tick state of the menu entry. 
   --
   function Get_Tick (Menu      : in Object_ID;
                      Component : in Component_ID;
                      Flags     : in System.Unsigned_Types.Unsigned := 0) return Menu_Tick_Type;

   --
   -- Returns the title of the menu.
   --
   function Get_Title (Menu      : in Object_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return string;

   --
   -- Returns the width of the workarea of the menu.
   --
   function Get_Width (Menu      : in Object_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return integer;

   --
   -- Removes an entry from the menu.
   --
   procedure Remove_Entry (Menu : in Object_ID;
                           ID   : in integer;
                           Flags: in System.Unsigned_Types.Unsigned := 0);

   --
   -- Removes a range of entries from the menu.
   --
   procedure Remove_Entries (Menu : in Object_ID;
                             Nr   : in integer;
                             Start: in integer := 0;
                             Flags: in System.Unsigned_Types.Unsigned := 0);

   --
   -- Specifies a Toolbox event to be raised when the user selects the given menu.
   --
   procedure Set_Click_Event (Menu        : in Object_ID;
                              Component   : in Component_ID;
                              Event       : in Toolbox_Event_Code_Type;
                              Flags       : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Specifies the objectid of the object to be shown when the user selects the menu entry.
   --
   procedure Set_Click_Show (Menu        : in Object_ID;
                             Component   : in Component_ID;
                             Object      : in Object_ID;
                             Show        : in Menu_Show_Type := Persistent;
                             Flags       : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Specifies the interactive help message for the menu entry.
   --
   procedure Set_Entry_Help (Menu        : in Object_ID;
                             Component   : in Component_ID;
                             Help        : in string;
                             Flags       : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the sprite which is to be used in the menu entry
   --
   procedure Set_Entry_Sprite (Menu        : in Object_ID;
                               Component   : in Component_ID;
                               Sprite      : in string;
                               Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the text for the named menu entry.
   --
   procedure Set_Entry_Text (Menu        : in Object_ID;
                             Component   : in Component_ID;
                             Text        : in string;
                             Flags       : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the fade state of the menu entry.
   --
   procedure Set_Fade (Menu        : in Object_ID;
                       Component   : in Component_ID;
                       Fade        : in Menu_Fade_Type;
                       Flags       : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the interactive help message for the menu.
   --
   procedure Set_Help (Menu        : in Object_ID;
                       Help        : in string;
                       Flags       : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Specifies an event to be raised when the user moves the mouse over a submenu arrow.
   --
   procedure Set_Sub_Menu_Event (Menu        : in Object_ID;
                                 Component   : in Component_ID;
                                 Event       : in Toolbox_Event_Code_Type;
                                 Flags       : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Specifies the objectid of the object to show when the user moves over the submenu arrow.
   --
   procedure Set_Sub_Menu_Show (Menu        : in Object_ID;
                                Component   : in Component_ID;
                                Object      : in Object_ID;
                                Flags       : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the tick state of the menu entry.
   --
   procedure Set_Tick (Menu        : in Object_ID;
                       Component   : in Component_ID;
                       Tick        : in Menu_Tick_Type;
                       Flags       : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the title of the menu.
   --
   procedure Set_Title (Menu      : in Object_ID;
                        Title     : in string;
                        Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_Menu_AboutToBeShown) is abstract;

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_Menu_HasBeenHidden) is abstract;

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_Menu_SubMenu) is abstract;

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_Menu_Selection) is abstract;


end RASCAL.ToolboxMenu;
