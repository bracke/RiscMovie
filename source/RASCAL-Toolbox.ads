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

-- @brief Toolbox related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with System.Unsigned_Types;   use System.Unsigned_Types;
with System;                  use System;
with Interfaces.C;            use Interfaces.C;
with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

with RASCAL.Memory;           use RASCAL.Memory;
with RASCAL.OS;               use RASCAL.OS;

package RASCAL.Toolbox is
      
   type Object_Class is new Integer;

   Max_Object_Name : Integer := 12;

   type Descriptor_Block_Type is new Address;

   type Object_State is (Hidden, Showing);

   type Toolbox_SysInfo_Type is (Task_Name,
                                 Messages_File_Descriptor,
                                 Ressource_Directory,
                                 Wimp_Task_Handle,
                                 Sprite_Area);

   type Object_ShowType is (Default,
                            FullSpec,
                            TopLeft,
                            Centre,
                            AtPointer);

   type Creation_Type is (From_Template,
                          From_Memory);

   type Top_Left_Type is
   record
   X   : Integer;
   Y   : Integer;
   end record;
   pragma Convention (C, Top_Left_Type);

   type Template_Header is
   record
   Class      : Object_Class;
   Flags      : Integer;
   Version    : Integer;
   Name       : Char_Array (1..12);
   Total_Size : Integer;
   Body_Ptr   : System.Address;
   Body_Size  : Integer;
   end record;
   pragma Convention (C, Template_Header);

   --
   -- Type used by Window class for window sizing.
   --
   type Toolbox_BBox_Type is
   record
   xmin : Integer;
   ymin : Integer;
   xmax : Integer;
   ymax : Integer;
   end record;
   pragma Convention (C, Toolbox_BBox_Type);   

   type Toolbox_EventObject_Type is
   record
   Header       : Toolbox_Event_Header;
   
   end record;
   pragma Convention (C, Toolbox_EventObject_Type);

   type Toolbox_MessageEvent_Type is
   record
   Self         : Object_ID;
   Component    : Component_ID;
   Event        : Toolbox_EventObject_Type;
   end record;
   pragma Convention (C, Toolbox_MessageEvent_Type);


   type ResourceFile_Header_Type is
   record
   File_ID         : Integer;
   Version_Number  : Integer;
   Object_Offset   : Integer;
   end record;
   pragma Convention (C, ResourceFile_Header_Type);

   type RelocationTable_Type is
   record
   Num_Relocations : Integer;
   Relocations     : Integer;
   end record;
   pragma Convention (C, RelocationTable_Type);

   type Relocation_Type is
   record
   Word_To_Relocate : Integer;
   Directive        : Integer;
   end record;
   pragma Convention (C, Relocation_Type);

   type Event_Interest_Type is
   record
   Code  : Integer;
   Class : Object_Class;
   end record;
   pragma Convention (C, Event_Interest_Type);

   type ResourceFileObject_TemplateHeader_Type is
   record
   String_Table_Offset       : Integer;
   Message_Table_Offset      : Integer;
   Relocation_Table_Offset   : Integer;
   Header                    : Template_Header;
   end record;
   pragma Convention (C, ResourceFileObject_TemplateHeader_Type);

   --
   -- A Toolbox Event. 
   --                   
   type Reason_ToolboxEvent is null record;
   pragma Convention (C, Reason_ToolboxEvent);

   type Reason_ToolboxEvent_Pointer is access Reason_ToolboxEvent;

   type AWEL_Reason_ToolboxEvent is abstract new
        Wimp_EventListener(Reason_Event_ToolboxEvent,-1,-1) with
   record
   Event : Reason_ToolboxEvent_Pointer;
   end record;

   --
   -- Raised if the Toolbox detects an error when it is not processing a Toolbox SWI.
   --
   type Toolbox_Error is
   record
   Header  : Toolbox_Event_Header;
   Number  : Integer;
   Message : Char_Array (1 .. 256 - 20 -
              (Toolbox_Event_Header'Size/CHAR_BIT) -
              (Object_ID'Size/CHAR_BIT) -
              (Component_ID'Size/CHAR_BIT) -
              (Integer'Size/CHAR_BIT));
   end record;
   pragma Convention (C, Toolbox_Error);

   type Toolbox_Error_Pointer is access Toolbox_Error;

   type ATEL_Toolbox_Error is abstract new Toolbox_EventListener(Toolbox_Event_Error,-1,-1) with
   record
   Event : Toolbox_Error_Pointer;
   end record;

   --
   -- Raised after the Toolbox creates objects that have their auto-created attribute set.
   --
   type Toolbox_ObjectAutoCreated is
   record
   Header         : Toolbox_Event_Header;
   Template_Name  : Char_Array (1 .. 256 - 20 -
                     (Toolbox_Event_Header'Size/CHAR_BIT) -
                     (Object_ID'Size/CHAR_BIT) -
                     (Component_ID'Size/CHAR_BIT));
   end record;
   pragma Convention (C, Toolbox_ObjectAutoCreated);

   type Toolbox_ObjectAutoCreated_Pointer is access Toolbox_ObjectAutoCreated;

   type ATEL_Toolbox_ObjectAutoCreated is abstract new
        Toolbox_EventListener(Toolbox_Event_ObjectAutoCreated,-1,-1) with
   record
   Event : Toolbox_ObjectAutoCreated_Pointer;
   end record;

   --
   -- Raised after the Toolbox deletes an object.
   --
   type Toolbox_ObjectDeleted is
   record
   Header  : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_ObjectDeleted);

   type Toolbox_ObjectDeleted_Pointer is access Toolbox_ObjectDeleted;

   type ATEL_Toolbox_ObjectDeleted is abstract new
            Toolbox_EventListener(Toolbox_Event_ObjectDeleted,-1,-1) with
   record
   Event : Toolbox_ObjectDeleted_Pointer;
   end record;

   --
   -- Creates an object from a named template in a res file.
   --
   function Create_Object (Template : in string) return Object_ID;

   --
   -- Creates an object from a template description block in memory.
   --
   function Create_Object (Template : in Address) return Object_ID;

   --
   -- Deletes a given object. By default objects attached to this object are also deleted.
   --
   procedure Delete_Object (Object : in Object_ID;
                            Flags  : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the id of the Ancestor of the given object.
   --
   procedure Get_Ancestor (Object    : in Object_ID;
                           Ancestor  : out Object_ID;
                           Component : out Component_ID;
                           Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the value of the client handle for this object.
   --
   function Get_Client (Object : in Object_ID;
                        Flags  : in System.Unsigned_Types.Unsigned := 0) return Object_ID;

   --
   -- Returns the class of the object.
   --
   function Get_Class (Object : in Object_ID;
                       Flags  : in System.Unsigned_Types.Unsigned := 0) return Object_Class;

   --
   -- Returns information regarding the state of an object.
   --
   function Get_State (Object : in Object_ID;
                       Flags  : in System.Unsigned_Types.Unsigned := 0) return Object_State;

   --
   -- Returns the parent of the object.
   --
   procedure Get_Parent (Object    : in Object_ID;
                         Parent    : out Object_ID;
                         Component : out Component_ID;
                         Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the name of the template used to create the object.
   --
   function Get_Template_Name (Object : in Object_ID;
                               Flags  : in System.Unsigned_Types.Unsigned := 0)

                                        return string;

   --
   -- Hides the object.
   --
   procedure Hide_Object (Object : in Object_ID;
                          Flags  : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Loads the given resource file, and creates any objects which have the auto-create flag set.
   --
   procedure Load_Resources (Filename : in string;
                             Flags : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the value of the client handle for this object.
   --
   procedure Set_Client_Handle
                  (Object : in Object_ID;
                   Client : in Object_ID;
                   Flags : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Shows the given object on screen at specific coordinates.
   --
   procedure Show_Object_At (Object             : in Object_ID;
                             X                  : Integer;
                             Y                  : Integer;
                             Parent_Object      : in Object_ID;
                             Parent_Component   : in Component_ID;
                             Flags : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Shows the given object on screen.
   --
   procedure Show_Object (Object             : in Object_ID;
                          Parent_Object      : in Object_ID         := 0;
                          Parent_Component   : in Component_ID      := 0;
                          Show               : in Object_ShowType   := Default;
                          Flags : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns a pointer to a block suitable to pass to Create_Object.
   --
   function Template_Lookup (Template_Name : in string;
                             Flags         : in System.Unsigned_Types.Unsigned := 0)

                                            return Address;

   --
   -- Raises toolbox event.
   --
   procedure Raise_Event (Object    : in Object_ID;
                          Component : in Component_ID;
                          Event     : in Address;
                          Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Abstract event handler for AWEL_Reason_ToolboxEvent event.
   --
   procedure Handle (The : in AWEL_Reason_ToolboxEvent)       is abstract;

   --
   -- Abstract event handler for the ATEL_Toolbox_Error event.
   --
   procedure Handle (The : in ATEL_Toolbox_Error)             is abstract;

   --
   -- Abstract event handler for the ATEL_Toolbox_ObjectAutoCreated event.
   --
   procedure Handle (The : in ATEL_Toolbox_ObjectAutoCreated) is abstract;

   --
   -- Abstract event handler for the ATEL_Toolbox_ObjectDeleted event.
   --
   procedure Handle (The : in ATEL_Toolbox_ObjectDeleted)     is abstract;

end RASCAL.Toolbox;
