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

-- @brief Toolbox FileInfo related types and methods.
-- $Author$
-- $Date$
-- $Revision$

with RASCAL.Toolbox;          use RASCAL.Toolbox;
with RASCAL.OS;               use RASCAL.OS;
with RASCAL.Time;             use RASCAL.Time;

with System.Unsigned_Types;   use System.Unsigned_Types;
with System;                  use System;
with Interfaces.C;            use Interfaces.C;

package RASCAL.ToolboxFileInfo is

   type Licence_Type is (Public_Domain,Single_user,Single_Machine,Site,Network,Authority);

   --
   -- Event is raised just before the FileInfo window is displayed.
   -- Type lacks union.
   --
   type Toolbox_FileInfo_AboutToBeShown is
   record
   Header     : Toolbox_Event_Header;
   Show_Type  : Integer;
   end record;
   pragma Convention (C, Toolbox_FileInfo_AboutToBeShown);

   type Toolbox_FileInfo_AboutToBeShown_Pointer is access Toolbox_FileInfo_AboutToBeShown;

   type ATEL_Toolbox_FileInfo_AboutToBeShown is abstract new Toolbox_EventListener(Toolbox_Event_FileInfo_AboutToBeShown,-1,-1) with
   record
   Event : Toolbox_FileInfo_AboutToBeShown_Pointer;
   end record;

   --
   -- Event is raised after the FileInfo window is hidden.
   --
   type Toolbox_FileInfo_DialogueCompleted is
   record
   Header  : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_FileInfo_DialogueCompleted);

   type Toolbox_FileInfo_DialogueCompleted_Pointer is access Toolbox_FileInfo_DialogueCompleted;

   type ATEL_Toolbox_FileInfo_DialogueCompleted is abstract new Toolbox_EventListener(Toolbox_Event_FileInfo_DialogueCompleted,-1,-1) with
   record
   Event : Toolbox_FileInfo_DialogueCompleted_Pointer;
   end record;
   
   --
   -- Returns the date string to be displayed in FileInfo dialogue.
   --
   function Get_Date (FileInfo : Object_ID;
                      Flags    : in System.Unsigned_Types.Unsigned := 0) return Address;

   --
   -- Returns the filename displayed in the FileInfo dialogue.
   --
   function Get_FileName (FileInfo : Object_ID;
                          Flags    : in System.Unsigned_Types.Unsigned := 0) return string;

   --
   -- Returns the size of the file to be displayed in the FileInfo dialogue.
   --
   function Get_FileSize (FileInfo : Object_ID;
                           Flags    : in System.Unsigned_Types.Unsigned := 0) return integer;

   --
   -- Returns the filetype of the file to be displayed in the FileInfo dialogue.
   --
   function Get_FileType (FileInfo : in Object_ID;
                          Flags    : in System.Unsigned_Types.Unsigned := 0) return integer;

   --
   -- Returns true if the file has been modified.
   --
   function Get_Modified (FileInfo : Object_ID;
                          Flags      : in System.Unsigned_Types.Unsigned := 0) return boolean;

   --
   -- Returns the title of the FileInfo dialogue.
   --
   function Get_Title (FileInfo : Object_ID;
                       Flags    : in System.Unsigned_Types.Unsigned := 0) return string;

   --
   -- Returns the id of the underlying Wimp window of the FileInfo dialogue.
   --
   function Get_Window_ID (FileInfo : Object_ID;
                           Flags    : in System.Unsigned_Types.Unsigned := 0) return Object_ID;

   --
   -- Sets the date string displayed in the FileInfo dialogue.
   --
   procedure Set_Date (FileInfo  : in Object_ID;
                       Date      : in UTC_Time_Type;
                       Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the filename to be displayed in the FileInfo dialogue.
   --
   procedure Set_FileName (FileInfo  : in Object_ID;
                           FileName  : in string;
                           Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the filetype of the file to be displayed in the FileInfo dialogue.
   --
   procedure Set_FileType (FileInfo : in Object_ID;
                           FileType : in integer;
                           Flags    : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the size of the file to be displayed in the FileInfo dialogue.
   --
   procedure Set_FileSize (FileInfo : in Object_ID;
                           FileSize : in integer;
                           Flags    : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the 'modified' flag of the FileInfo dialogue.
   --
   procedure Set_Modified (FileInfo   : in Object_ID;
                           Modified   : in boolean;
                           Flags      : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the title of the FileInfo dialogue.
   --
   procedure Set_Title (FileInfo  : in Object_ID;
                        Title     : in string;
                        Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_FileInfo_AboutToBeShown) is abstract;

   --
   --
   --
   procedure Handle(The : in ATEL_Toolbox_FileInfo_DialogueCompleted) is abstract;

end RASCAL.ToolboxFileInfo;
