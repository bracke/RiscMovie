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

-- @brief Toolbox ScrollList related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with RASCAL.Toolbox;          use RASCAL.Toolbox;
with RASCAL.Font;             use RASCAL.Font;
with RASCAL.OS;               use RASCAL.OS;
with RASCAL.Utility;          use RASCAL.Utility;

with System.Unsigned_Types;   use System.Unsigned_Types;
with Interfaces.C;            use Interfaces.C;

package RASCAL.ToolboxScrollList is

   --
   -- This event is raised when the user has clicked on an item in a scrolllist.
   --Item contains the nr of the item that was selected or deselected.
   --
   type Toolbox_ScrollList_Selection is
   record
   Header  : Toolbox_Event_Header;
   Flags   : System.Unsigned_Types.Unsigned;
   Item    : Integer;
   end record;
   pragma Convention (C, Toolbox_ScrollList_Selection);
   
   type Toolbox_ScrollList_Selection_Pointer is access Toolbox_ScrollList_Selection;
   
   type ATEL_Toolbox_ScrollList_Selection is abstract new Toolbox_EventListener(Toolbox_Event_ScrollList_Selection,-1,-1) with
   record
   Event : Toolbox_ScrollList_Selection_Pointer;
   end record;

   --
   -- An array of string items.
   --
   type Item_List_Type is array (natural range <>) of UString;
   type Item_List_Pointer is access Item_List_Type;

   type ItemNumber_List_Type is array (natural range <>) of Integer;
   type ItemNumber_List_Pointer is access ItemNumber_List_Type;

   --
   -- Returns the state of the scrolllist.
   --
   function Get_State (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return System.Unsigned_Types.Unsigned;

   --
   -- Sets the state of the scrolllist.
   --
   procedure Set_State (Window    : in Object_ID;
                        Component : in Component_ID;
                        State     : in System.Unsigned_Types.Unsigned;
                        Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Adds an item to the scrolllist.
   --
   procedure Add_Item (Window       : in Object_ID;
                       Component    : in Component_ID;
                       Text         : in String;
                       Index        : in integer := -1;
                       Flags        : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Adds an item to the end of the scrolllist.
   --
   procedure Add_Last_Item (Window       : in Object_ID;
                            Component    : in Component_ID;
                            Text         : in String;
                            Flags        : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Removes an item from the scrolllist.
   --
   procedure Delete_Items (Window       : in Object_ID;
                           Component    : in Component_ID;
                           Start_Index  : in integer := 0;
                           End_Index    : in integer := -1;
                           Flags        : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Removes all items from the scrolllist.
   --
   procedure Delete_All (Window       : in Object_ID;
                         Component    : in Component_ID;
                         Start_Index  : in Integer := 0);

   --
   -- Selects an item from the scrolllist.
   --
   procedure Select_Item (Window    : in Object_ID;
                          Component : in Component_ID;
                          Index     : in integer;
                          Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Select 'Items' in the scrolllist.
   --
   procedure Select_Items (Window    : in Object_ID;
                           Component : in Component_ID;
                           Items     : in ItemNumber_List_Type;
                           Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Selects all items in the scrolllist with index numbers from 'Start' to ('Start' + number of items).
   --
   procedure Select_All (Window    : in Object_ID;
                         Component : in Component_ID;
                         Start     : in Natural := 1;
                         Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Deselects an item from the scrolllist.
   --
   procedure DeSelect_Item (Window    : in Object_ID;
                            Component : in Component_ID;
                            Index     : in integer;
                            Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- DeSelect 'Items' in the scrolllist.
   --
   procedure DeSelect_Items (Window    : in Object_ID;
                             Component : in Component_ID;
                             Items     : in ItemNumber_List_Type;
                             Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Deselects all items in the scrolllist.
   --
   procedure DeSelect_All (Window    : in Object_ID;
                           Component : in Component_ID;
                           Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns selected item from the scrolllist. '-1' if no item is selected.
   --
   function Get_Selected (Window    : in Object_ID;
                          Component : in Component_ID;
                          Offset    : in Integer := -1;
                          Flags     : in System.Unsigned_Types.Unsigned := 0) return integer;

   --
   -- Returns nr of selected items.
   --
   function Count_Selections (Window    : in Object_ID;
                              Component : in Component_ID;
                              Flags     : in System.Unsigned_Types.Unsigned := 0) return Natural;

   --
   -- Returns array of selected items - as itemtext.
   --
   function Get_Selections (Window    : in Object_ID;
                            Component : in Component_ID;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) return Item_List_Type;

   --
   -- Returns array of selected items - as index number.
   --
   function Get_SelectionNumbers (Window    : in Object_ID;
                                  Component : in Component_ID;
                                  Flags     : in System.Unsigned_Types.Unsigned := 0) return ItemNumber_List_Type;

   --
   -- Returns true if the item is selected.
   --
   function Is_Selected (Window    : in Object_ID;
                         Component : in Component_ID;
                         Index     : in Integer;
                         Flags     : in System.Unsigned_Types.Unsigned := 0) return Boolean;

   --
   -- Returns the nr of items in the scrolllist.
   --
   function Count_Items (Window    : in Object_ID;
                         Component : in Component_ID;
                         Flags     : in System.Unsigned_Types.Unsigned := 0) return Integer;

   --
   -- Returns array of items in scrolllist.
   --
   function Get_Items (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return Item_List_Type;

   --
   -- Returns a array of strings containing only the part of the items belonging to a specific column.
   --
   function Get_Column (Items     : in Item_List_Type;
                        Column_Nr : in Positive := 1) return Item_List_Type;

   --
   -- If the item is outside the visible area of the ScrollList, then it will be displayed at the top of the visible area.
   --
   procedure Make_Visible (Window    : in Object_ID;
                           Component : in Component_ID;
                           Index     : in integer;
                           Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the foreground and background colours for the specified ScrollList.
   --Do not use this as it is NOT style guide compliant.
   --
   procedure Set_Colour (Window    : in Object_ID;
                         Component : in Component_ID;
                         Foreground: in integer;
                         Background: in integer;
                         Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the foreground and background colours for the specified ScrollList.
   --
   procedure Get_Colour (Window    : in Object_ID;
                         Component : in Component_ID;
                         Foreground: out integer;
                         Background: out integer;
                         Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the font for the ScrollList.
   --
   procedure Set_Font (Window    : in Object_ID;
                    Component : in Component_ID;
                    Font_Handle : in Font_Handle_Type;
                    Font_Height : in integer:=12;
                    Font_Width  : in integer:=12;
                    Flags       : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the text of the item in the ScrollList.
   --
   function Get_ItemText (Window    : in Object_ID;
                          Component : in Component_ID;
                          Item      : in integer;
                          Flags     : in System.Unsigned_Types.Unsigned := 0) return String;

   --
   -- Set the text of the item.
   --
   procedure Set_ItemText(Window    : in Object_ID;
                          Component : in Component_ID;
                          Index     : in Integer;
                          Text      : in String;
                          Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the heading of a scrolllist.
   --
   procedure Set_Heading (Window       : in Object_ID;
                       Component    : in Component_ID;
                       Index        : in integer;
                       Text         : in String;
                       Flags        : in System.Unsigned_Types.Unsigned := 0);

   --
   --
   --
   procedure Handle (The : in ATEL_Toolbox_ScrollList_Selection) is abstract;

end RASCAL.ToolboxScrollList;
