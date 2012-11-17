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

-- @brief Toolbox ProgInfo related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with RASCAL.Toolbox;          use RASCAL.Toolbox;
with RASCAL.OS;               use RASCAL.OS;

with System.Unsigned_Types;   use System.Unsigned_Types;
with Interfaces.C;            use Interfaces.C;

package RASCAL.ToolboxProgInfo is

   type License_Type is (Public_Domain,Single_user,Single_Machine,Site,Network,Authority);

   --
   -- Event is raised just before the Prog Info window is displayed.
   --Type lacks union.
   --
   type Toolbox_ProgInfo_AboutToBeShown is
   record
   Header     : Toolbox_Event_Header;
   Show_Type  : Integer;
   end record;
   pragma Convention (C, Toolbox_ProgInfo_AboutToBeShown);

   type Toolbox_ProgInfo_AboutToBeShown_Pointer is access Toolbox_ProgInfo_AboutToBeShown;

   type ATEL_Toolbox_ProgInfo_AboutToBeShown is abstract new Toolbox_EventListener(Toolbox_Event_ProgInfo_AboutToBeShown,-1,-1) with
   record
   Event : Toolbox_ProgInfo_AboutToBeShown_Pointer;
   end record;

   --
   -- Event is raised after the Prog Info window has been hidden.
   --
   type Toolbox_ProgInfo_DialogueCompleted is
   record
   Header  : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_ProgInfo_DialogueCompleted);

   type Toolbox_ProgInfo_DialogueCompleted_Pointer is access Toolbox_ProgInfo_DialogueCompleted;

   type ATEL_Toolbox_ProgInfo_DialogueCompleted is abstract new Toolbox_EventListener(Toolbox_Event_ProgInfo_DialogueCompleted,-1,-1) with
   record
   Event : Toolbox_ProgInfo_DialogueCompleted_Pointer;
   end record;

   --
   -- Event is raised after the Prog Info Launch Web Page action button is clicked.
   --
   type Toolbox_ProgInfo_LaunchWebPage is
   record
   Header  : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_ProgInfo_LaunchWebPage);

   type Toolbox_ProgInfo_LaunchWebPage_Pointer is access Toolbox_ProgInfo_LaunchWebPage;

   type ATEL_Toolbox_ProgInfo_LaunchWebPage is abstract new Toolbox_EventListener(Toolbox_Event_ProgInfo_LaunchWebPage,-1,-1) with
   record
   Event : Toolbox_ProgInfo_LaunchWebPage_Pointer;
   end record;

   --
   -- Returns the ID of the underlying Window object used for the Prog Info object.
   --
   function Get_Window_ID (ProgInfo : Object_ID;
                           Flags    : in System.Unsigned_Types.Unsigned := 0) return Object_ID;

   --
   -- Returns the URI launched by the web page button.
   --
   function Get_URI (ProgInfo : Object_ID;
                     Flags: in System.Unsigned_Types.Unsigned := 0) return string;

   --
   -- Returns the window title being used in the Prog Info object.
   --
   function Get_Title (ProgInfo : Object_ID;
                       Flags    : in System.Unsigned_Types.Unsigned := 0) return string;

   --
   -- Sets the URI to be launched by the web page button.
   --
   procedure Set_URI (ProgInfo      : in Object_ID;
                      URI           : in string;
                      Flags         : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the window title to be used in the Prog Info object.
   --
   procedure Set_Title (ProgInfo      : in Object_ID;
                        Title     : in string;
                        Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the license type used in the Prog Info object.
   --
   function Get_License_Type (ProgInfo : Object_ID;
                              Flags    : in System.Unsigned_Types.Unsigned := 0) return License_Type;

   --
   -- Sets the license type to be used in the Prog Info object.
   --
   procedure Set_License_Type (ProgInfo : in Object_ID;
                               License  : in License_Type;
                               Flags    : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the version string used in the Prog Info object.
   --
   function Get_Version (ProgInfo : Object_ID;
                         Flags    : in System.Unsigned_Types.Unsigned := 0) return string;

   --
   -- Sets the version string used in the Prog Info object.
   --
   procedure Set_Version (ProgInfo      : in Object_ID;
                          Version     : in string;
                          Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the event generated by a click on the web page button.
   --
   function Get_Web_Event (ProgInfo : Object_ID;
                           Flags    : in System.Unsigned_Types.Unsigned := 0) return Toolbox_Event_Code_Type;

   --
   -- Sets the event generated by a click on the web page button.
   --
   procedure Set_Web_Event (ProgInfo : in Object_ID;
                            Event    : in Toolbox_Event_Code_Type;
                            Flags    : in System.Unsigned_Types.Unsigned := 0);

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_ProgInfo_AboutToBeShown) is abstract;

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_ProgInfo_DialogueCompleted) is abstract;

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_ProgInfo_LaunchWebPage) is abstract;

end RASCAL.ToolboxProgInfo;
