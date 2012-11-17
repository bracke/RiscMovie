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

-- @brief Toolbox Window related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with Interfaces.C;            use Interfaces.C;
with System.Unsigned_Types;   use System.Unsigned_Types;
with System;                  use System;

with RASCAL.Toolbox;          use RASCAL.Toolbox;
with RASCAL.OS;               use RASCAL.OS;

package RASCAL.ToolboxWindow is

   No_Toolbox_Window          : Exception;
   Enumeration_Buffer_Overrun : Exception;

   type Gadget_ID_Type is new Integer;

   ActionButton_Base  : constant Gadget_ID_Type :=  128;
   AdjusterArrow_Base : constant Gadget_ID_Type :=  768;
   Button_Base        : constant Gadget_ID_Type :=  960;
   DisplayField_Base  : constant Gadget_ID_Type :=  448;
   Draggable_Base     : constant Gadget_ID_Type :=  640;
   Label_Base         : constant Gadget_ID_Type :=  320;
   LabelledBox_Base   : constant Gadget_ID_Type :=  256;
   NumberRange_Base   : constant Gadget_ID_Type :=  832;
   OptionButton_Base  : constant Gadget_ID_Type :=  192;
   PopUp_Base         : constant Gadget_ID_Type :=  704;
   RadioButton_Base   : constant Gadget_ID_Type :=  384;
   ScrollList_Base    : constant Gadget_ID_Type :=  16410;
   Slider_Base        : constant Gadget_ID_Type :=  576;
   StringSet_Base     : constant Gadget_ID_Type :=  896;
   TextArea_Base      : constant Gadget_ID_Type :=  16408;
   WritableField_Base : constant Gadget_ID_Type :=  512;

   type Gadget_Header_Type is
   record
   Flags          : System.Unsigned_Types.Unsigned;
   Gadget_Kind    : Gadget_ID_Type;
   BBox           : Toolbox_BBox_Type;
   Id             : Component_ID;
   Max_Help       : integer;
   Help           : Address;
   end record;
   pragma Convention (C, Gadget_Header_Type);

   type Gadget_Button_Type is
   record
   Header         : Gadget_Header_Type;
   Button_Flags   : integer;
   Value          : Address;
   Max_Value      : integer;
   Validation     : Address;
   Max_Validation : integer;
   end record;
   pragma Convention (C, Gadget_Button_Type);

   type Gadget_ActionButton_Type is
   record
   Header         : Gadget_Header_Type;
   Max_Text       : integer;
   Text           : Address;
   Click_Show     : Address;
   Event          : Toolbox_Event_Code_Type;
   end record;
   pragma Convention (C, Gadget_ActionButton_Type);

   type Gadget_OptionButton_Type is
   record
   Header         : Gadget_Header_Type;
   Label          : Address;
   Max_Label      : integer;
   Event          : Toolbox_Event_Code_Type;
   end record;
   pragma Convention (C, Gadget_OptionButton_Type);

   type Gadget_LabelledBox_Type is
   record
   Header         : Gadget_Header_Type;
   Label          : Address;
   end record;
   pragma Convention (C, Gadget_LabelledBox_Type);

   type Gadget_Label_Type is
   record
   Header         : Gadget_Header_Type;
   Label          : Address;
   end record;
   pragma Convention (C, Gadget_Label_Type);

   type Gadget_RadioButton_Type is
   record
   Header         : Gadget_Header_Type;
   Group_Number   : integer;
   Label          : Address;
   Max_Label      : integer;
   Event          : Toolbox_Event_Code_Type;
   end record;
   pragma Convention (C, Gadget_RadioButton_Type);

   type Gadget_DisplayField_Type is
   record
   Header         : Gadget_Header_Type;
   Text           : Address;
   Max_Text       : integer;
   end record;
   pragma Convention (C, Gadget_DisplayField_Type);

   type Gadget_WritableField_Type is
   record
   Header         : Gadget_Header_Type;
   Text           : Address;
   Max_Text       : integer;
   Allowable      : Address;
   Max_Allowable  : integer;
   Before         : Component_ID;
   After          : Component_ID;
   end record;
   pragma Convention (C, Gadget_WritableField_Type);

   type Gadget_Slider_Type is
   record
   Header         : Gadget_Header_Type;
   Lower_Bound    : integer;
   Upper_Bound    : integer;
   Step_Size      : integer;
   Initial_Value  : integer;
   end record;
   pragma Convention (C, Gadget_Slider_Type);

   type Gadget_Draggable_Type is
   record
   Header         : Gadget_Header_Type;
   Text           : Address;
   Max_Text       : integer;
   Sprite         : Address;
   Max_Sprite     : integer;
   end record;
   pragma Convention (C, Gadget_Draggable_Type);

   type Gadget_PopUp_Type is
   record
   Header         : Gadget_Header_Type;
   Menu           : Address;
   end record;
   pragma Convention (C, Gadget_PopUp_Type);

   type Gadget_Adjuster_Type is
   record
   Header         : Gadget_Header_Type;
   end record;
   pragma Convention (C, Gadget_Adjuster_Type);

   type Gadget_NumberRange_Type is
   record
   Header         : Gadget_Header_Type;
   Lower_Bound    : integer;
   Upper_Bound    : integer;
   Step_Size      : integer;
   Initial_Value  : integer;
   Precision      : integer;
   Before         : Component_ID;
   After          : Component_ID;
   Display_Length : integer;
   end record;
   pragma Convention (C, Gadget_NumberRange_Type);

   type Gadget_StringSet_Type is
   record
   Header         : Gadget_Header_Type;
   StringSet      : Address;
   Title          : Address;
   Initial_Selected : Address;
   Max_Selected   : integer;
   Allowable      : Address;
   Max_Allowable  : integer;
   Before         : Component_ID;
   After          : Component_ID;
   end record;
   pragma Convention (C, Gadget_StringSet_Type);

   type Gadget_List_Type is array(integer range <>) of Component_ID;
   type Gadget_List_Pointer is access Gadget_List_Type;

   type Icon_List_Type is array(integer range <>) of Icon_Handle_Type;
   type Icon_List_Pointer is access Icon_List_Type;

   type Toolbox_Toolbar_Type is (Internal_Bottom_Left, Internal_Top_Left, External_Bottom_Left, External_Top_Left);

   type Toolbox_Pointer_Info_Type is
   record
      X_Pos    : integer;
      Y_Pos    : integer;
      Buttons  : integer;
   end record;

   --
   -- This event is raised just before the Window object is shown.
   --Type lacks union.
   --
   type Toolbox_Window_AboutToBeShown is
   record
   Header    : Toolbox_Event_Header;
   Show_Type : Integer;
   end record;
   pragma Convention (C, Toolbox_Window_AboutToBeShown);

   type Toolbox_Window_AboutToBeShown_Pointer is access Toolbox_Window_AboutToBeShown;

   type ATEL_Toolbox_Window_AboutToBeShown is abstract new Toolbox_EventListener(Toolbox_Event_Window_AboutToBeShown,-1,-1) with
   record
   Event : Toolbox_Window_AboutToBeShown_Pointer;
   end record;

   --
   -- This event is raised after the Window object has been hidden.
   --
   type Toolbox_Window_HasBeenHidden is
   record
   Header  : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_Window_HasBeenHidden);

   type Toolbox_Window_HasBeenHidden_Pointer is access Toolbox_Window_HasBeenHidden;

   type ATEL_Toolbox_Window_HasBeenHidden is abstract new Toolbox_EventListener(Toolbox_Event_Window_HasBeenHidden,-1,-1) with
   record
   Event : Toolbox_Window_HasBeenHidden_Pointer;
   end record;

   --
   -- Returns the size of the gadget's text buffer. 
   --
   function Gadget_Get_BufferSize (Object    : in Object_ID;
                                   Component : in Component_ID) return Integer;

   --
   -- Generic method to set the text value of a gadget.
   --(WritableField,DisplayField,ActionButton,OptionButton,RadioButton).
   --
   procedure Gadget_SetValue (Window    : in Object_ID;
                              Component : in Component_ID;
                              Value     : in String;
                              Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the bounding box of the gadget.
   --
   function Gadget_GetBBox (Window    : in Object_ID;
                            Component : in Component_ID;
                            Flags     : in System.Unsigned_Types.Unsigned := 0) return Toolbox_BBox_Type;

   --
   -- Returns the flags of the specified gadget.
   --
   function Gadget_GetFlags (Window    : in Object_ID;
                             Component : in Component_ID;
                             Flags     : in System.Unsigned_Types.Unsigned := 0) return integer;

   --
   -- Returns the type of the gadget.
   --
   function Get_Type (Window    : in Object_ID;
                      Component : in Component_ID;
                      Flags     : in System.Unsigned_Types.Unsigned := 0) return Gadget_ID_Type;

   --
   -- Returns the interactive help message of the gadget.
   --
   function Gadget_Get_Help (Window    : in Object_ID;
                             Component : in Component_ID;
                             Flags     : in System.Unsigned_Types.Unsigned := 0) return String;

   --
   -- Returns the list of Wimp icons making up the gadget.
   --
   function Get_Icon_List (Window    : in Object_ID;
                           Component : in Component_ID;
                           Flags     : in System.Unsigned_Types.Unsigned := 0) return Icon_List_Type;

   --
   -- Sets the flags of the gadget.
   --
   procedure Gadget_SetFlags (Window    : in Object_ID;
                              Component : in Component_ID;
                              New_Flags : in System.Unsigned_Types.Unsigned;
                              Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Fade gadget.
   --
   procedure Gadget_Fade (Window    : in Object_ID;
                          Component : in Component_ID;
                          Flags     : in System.Unsigned_Types.Unsigned := 0);
   
   --
   -- Unfade gadget.
   --
   procedure Gadget_UnFade (Window    : in Object_ID;
                            Component : in Component_ID;
                            Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the focus for the window.
   --Component = -1 means no focus.
   --Component = -2 means invisible caret.
   --
   procedure Set_Focus (Window    : in Object_ID;
                        Component : in Component_ID;
                        Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the interactive help message for the gadget.
   --
   procedure Gadget_Set_Help (Window    : in Object_ID;
                              Component : in Component_ID;
                              Help      : in String;
                              Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Moves an already created gadget to another location.
   --
   procedure Move_Gadget  (Window    : in Object_ID;
                           Component : in Component_ID;
                           BBox      : in Toolbox_BBox_Type;
                           Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns a pointer to a block of memory suitable for use with Window_AddGadget.
   --
   procedure Extract_GadgetInfo (Template : in Address;
                                 Gadget   : in Component_ID;
                                 Block    : out Address;
                                 BlockSize: out Integer;
                                 Flags    : in System.Unsigned_Types.Unsigned := 0);
                                 
   --
   -- Provides the icon numbers for all Toolbox components and sub icons of one component.
   --
   function Enumerate_Gadgets (Window : in Object_ID;
                               Flags  : in System.Unsigned_Types.Unsigned := 0) return Gadget_List_Type;

   --
   -- Returns the list if Wimp icons making up the gadget.
   --
   function Gadget_Get_Icon_List (Window    : in Object_ID;
                                  Component : in Component_ID;
                                  Flags     : in System.Unsigned_Types.Unsigned := 0) return Icon_List_Type;

--   --
--   -- Adds the gadget to the window.
--   --
--   procedure Add_Gadget (Window : in Object_ID;
--                         Gadget : in Gadget_Type;
--                         Flags  : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Adds the gadget to the window.
   --
   procedure Add_Gadget (Window : in Object_ID;
                         Gadget : in Address;
                         Flags  : in System.Unsigned_Types.Unsigned := 0);


   --
   -- Plots a gadget in a redraw loop.
   --
   procedure Plot_Gadget (Gadget : in Address;
                          Flags  : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Removes the gadget from the window.
   --
   procedure Remove_Gadget (Window    : in Object_ID;
                            Component : in Component_ID;
                            Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the objectId of the Menu that will be displayed when MENU is pressed over the window.
   --
   procedure Set_Menu (Window  : in Object_ID;
                       Menu    : in Object_ID;
                       Flags   : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the objectId of the Menu that will be displayed when MENU is pressed over the window.
   --
   function Get_Menu (Window : in Object_ID;
                      Flags  : in System.Unsigned_Types.Unsigned := 0) return Object_ID;

   --
   -- Sets the name of the pointer sprite to be used in the specified window.
   --
   procedure Set_Pointer (Window  : in Object_ID;
                          Sprite  : in string;
                          X_Spot  : in integer;
                          Y_Spot  : in integer;
                          Flags   : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the name of the pointer sprite to be used in the specified window.
   --
   function Get_Pointer (Window : in Object_ID;
                         X_Spot : in integer;
                         Y_Spot : in integer;
                         Flags  : in System.Unsigned_Types.Unsigned := 0) return String;

   --
   -- Sets the interactive help message of the specified window object.
   --
   procedure Set_Help  (Window : in Object_ID;
                        Help   : in String;
                        Flags  : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the interactive help message of the specified window object.
   --
   function Get_Help (Window : in Object_ID;
                      Flags: in System.Unsigned_Types.Unsigned := 0) return String;

   --
   -- Sets the title of the window.
   --
   procedure Set_Title  (Window : in Object_ID;
                         Title  : in String;
                         Flags  : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the title of the window.
   --
   function Get_Title (Window : in Object_ID;
                       Flags  : in System.Unsigned_Types.Unsigned := 0) return String;

   --
   -- Set the ComponentID of the gadget which the cursor appears in when the specified window is opened.
   --
   procedure Set_Default_Focus (Window    : in Object_ID;
                                Component : in Component_ID;
                                Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the ComponentID of the gadget which the cursor appears in when the specified window is opened.
   --
   function Get_Default_Focus (Window : in Object_ID;
                               Flags  : in System.Unsigned_Types.Unsigned := 0) return Component_ID;

   --
   -- Sets the size of the specified window.
   --
   procedure Set_Extent (Window : in Object_ID;
                         BBox   : in Toolbox_BBox_Type;
                         Flags: in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the size of the specified window.
   --
   function Get_Extent (Window : in Object_ID;
                        Flags  : in System.Unsigned_Types.Unsigned := 0) return Toolbox_BBox_Type;

   --
   -- Forces a redraw of the area of the window given by the bounding box.
   --
   procedure Force_Redraw (Window : in Object_ID;
                           BBox   : in Toolbox_BBox_Type;
                           Flags  : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the ObjectIDs of the toolbars that are attached to the specified window.
   --
   procedure Set_Toolbars (Window : in Object_ID;
                           Toolbar: in Object_ID;
                           Bar_Type: in Toolbox_Toolbar_Type);

   --
   -- Returns the ObjectIDs of the toolbars that are attached to the specified window.
   --
   function Get_Toolbars (Window : in Object_ID;
                          Bar_Type: in Toolbox_Toolbar_Type) return Object_ID;

   --
   -- Returns true if the window is open.
   --
   function Is_Open (Window : in Object_ID) return Boolean;

   --
   -- Returns the ObjectID and ComponentId if the pointer is over a Toolbox window.
   --
   function Get_Pointer_Info(Flags: in System.Unsigned_Types.Unsigned := 0) return Toolbox_Pointer_Info_Type;


   --
   -- Return Top_left coordinates of window in OS units.
   --
   procedure Get_WindowPosition (Window : in Object_ID;
                                 X_Pos  : out Integer;
                                 Y_Pos  : out Integer);
                                 
   --
   -- Returns the WIMP handle of the toolbox window.
   --
   function Get_Wimp_Handle (Window : in Object_ID;
                             Flags  : in System.Unsigned_Types.Unsigned := 0)

                                      return Wimp_Handle_Type;

   --
   -- Returns the ObjectID and ComponentID of the object that contains the icon specified.
   --
   procedure Wimp_To_Toolbox (Window    : in Wimp_Handle_Type;
                              Icon      : in Icon_Handle_Type;
                              Object    : out Object_ID;
                              Component : out Component_ID;
                              Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_Window_AboutToBeShown) is abstract;

   --
   --
   --
   procedure Handle(The : in ATEL_toolbox_Window_HasBeenHidden) is abstract;

end RASCAL.ToolboxWindow;
