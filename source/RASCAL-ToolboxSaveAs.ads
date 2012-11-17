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

-- @brief ToolboxSaveAs types and methods.
-- $Author$
-- $Date$
-- $Revision$

with RASCAL.Toolbox;          use RASCAL.Toolbox;
with RASCAL.OS;               use RASCAL.OS;

with System.Unsigned_Types;   use System.Unsigned_Types;
with System;                  use System;
with Interfaces.C;            use Interfaces.C;

package RASCAL.ToolboxSaveAs is

   --
   -- Event is raised just before the SaveAs dialogue is displayed.
   --Type lacks union.
   --
   type Toolbox_SaveAs_AboutToBeShown is
   record
   Header     : Toolbox_Event_Header;
   Show_Type  : Integer;
   end record;
   pragma Convention (C, Toolbox_SaveAs_AboutToBeShown);

   type Toolbox_SaveAs_AboutToBeShown_Pointer is access Toolbox_SaveAs_AboutToBeShown;

   type ATEL_Toolbox_SaveAs_AboutToBeShown is abstract new Toolbox_EventListener(Toolbox_Event_SaveAs_AboutToBeShown,-1,-1) with
   record
   Event : Toolbox_SaveAs_AboutToBeShown_Pointer;
   end record;

   --
   -- Event is raised after the SaveAs object has been hidden.
   --
   type Toolbox_SaveAs_DialogueCompleted is
   record
   Header  : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_SaveAs_DialogueCompleted);

   type Toolbox_SaveAs_DialogueCompleted_Pointer is access Toolbox_SaveAs_DialogueCompleted;

   type ATEL_Toolbox_SaveAs_DialogueCompleted is abstract new Toolbox_EventListener(Toolbox_Event_SaveAs_DialogueCompleted,-1,-1) with
   record
   Event : Toolbox_SaveAs_DialogueCompleted_Pointer;
   end record;

   --
   -- Event is raised to request that the client shoudl save its data to the given filename.
   --
   type Toolbox_SaveAs_SaveToFile is
   record
   Header   : Toolbox_Event_Header;
   Filename : Char_Array (1..212);
   end record;
   pragma Convention (C, Toolbox_SaveAs_SaveToFile);

   type Toolbox_SaveAs_SaveToFile_Pointer is access Toolbox_SaveAs_SaveToFile;

   type ATEL_Toolbox_SaveAs_SaveToFile is abstract new Toolbox_EventListener(Toolbox_Event_SaveAs_SaveToFile,-1,-1) with
   record
   Event : Toolbox_SaveAs_SaveToFile_Pointer;
   end record;

   --
   -- Event is raised to request that the client should fill the given buffer.
   --
   type Toolbox_SaveAs_FillBuffer is
   record
   Header  : Toolbox_Event_Header;
   Size    : Integer;
   Buffer  : Address;
   No_Bytes: Integer;
   end record;
   pragma Convention (C, Toolbox_SaveAs_FillBuffer);

   type Toolbox_SaveAs_FillBuffer_Pointer is access Toolbox_SaveAs_FillBuffer;

   type ATEL_Toolbox_SaveAs_FillBuffer is abstract new Toolbox_EventListener(Toolbox_Event_SaveAs_FillBuffer,-1,-1) with
   record
   Event : Toolbox_SaveAs_FillBuffer_Pointer;
   end record;

   --
   -- Event is raised when the save is successfully completed.
   --
   type Toolbox_SaveAs_SaveCompleted is
   record
   Header   : Toolbox_Event_Header;
   Message  : Integer;
   FileName : Char_Array (1..208);
   end record;
   pragma Convention (C, Toolbox_SaveAs_SaveCompleted);

   type Toolbox_SaveAs_SaveCompleted_Pointer is access Toolbox_SaveAs_SaveCompleted;

   type ATEL_Toolbox_SaveAs_SaveCompleted is abstract new Toolbox_EventListener(Toolbox_Event_SaveAs_SaveCompleted,-1,-1) with
   record
   Event : Toolbox_SaveAs_SaveCompleted_Pointer;
   end record;

   --
   -- Returns the Wimp id of the underlying window.
   --
   function Get_Window_ID (SaveAs : in Object_ID;
                           Flags  : in System.Unsigned_Types.Unsigned := 0) return Object_ID;

   --
   -- Returns the title of the SaveAs window.
   --
   function Get_Title (SaveAs : in Object_ID;
                       Flags  : in System.Unsigned_Types.Unsigned := 0) return string;

   --
   -- Sets the title of the SaveAs window.
   --
   procedure Set_Title (SaveAs : in Object_ID;
                        Title  : in string;
                        Flags  : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the filename being used in the saveAs dialogue.
   --
   function Get_FileName (SaveAs : in Object_ID;
                         Flags   : in System.Unsigned_Types.Unsigned := 0) return string;

   --
   -- Sets the filename being used in the saveAs dialogue.
   --
   procedure Set_FileName (SaveAs   : in Object_ID;
                           FileName : in string;
                           Flags    : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the file size (bytes) for the SaveAs dialogue.
   --
   function Get_File_Size (SaveAs : in Object_ID;
                           Flags  : in System.Unsigned_Types.Unsigned := 0) return integer;

   --
   -- Sets the file size (bytes) for the SaveAs dialogue.
   --
   procedure Set_File_Size (SaveAs    : in Object_ID;
                            File_Size : in integer;
                            Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the file type for the SaveAs dialogue.
   --
   function Get_File_Type (SaveAs : in Object_ID;
                           Flags  : in System.Unsigned_Types.Unsigned := 0) return integer;

   --
   -- Sets the file type for the SaveAs dialogue.
   --
   procedure Set_File_Type (SaveAs    : in Object_ID;
                            File_Type : in integer;
                            Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Report whether an attempt to save data to a file was successful or not.
   --
   procedure Save_Completed (SaveAs   : in Object_ID;
                             FileName : in string;
                             Flags    : in System.Unsigned_Types.Unsigned := 1);

   --
   -- indicates the address of the block of memory containg the data to be saved.
   --
   procedure Set_Data_Address (SaveAs         : in Object_ID;
                               Data           : in Address;
                               Data_Size      : in integer;
                               Selection      : in Address;
                               Selection_Size : in integer;
                               Flags          : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Confirms that the rquested buffer fill has taken place.
   --
   procedure Buffer_Filled  (SaveAs      : in Object_ID;
                             Buffer      : in Address;
                             Buffer_Size : in integer;
                             Flags       : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Indicates whether there is a current selection.
   --
   procedure Selection_Available  (SaveAs      : in Object_ID;
                                   Selection   : in boolean;
                                   Flags       : in System.Unsigned_Types.Unsigned := 0);

   --
   -- 
   --
   procedure Handle (The : in ATEL_Toolbox_SaveAs_AboutToBeShown) is abstract;

   --
   --
   --
   procedure Handle (The : in ATEL_Toolbox_SaveAs_DialogueCompleted) is abstract;

   --
   --
   --
   procedure Handle (The : in ATEL_Toolbox_SaveAs_SaveToFile) is abstract;

   --
   --
   --
   procedure Handle (The : in ATEL_Toolbox_SaveAs_FillBuffer) is abstract;

   --
   --
   --
   procedure Handle (The : in ATEL_Toolbox_SaveAs_SaveCompleted) is abstract;

end RASCAL.ToolboxSaveAs;
