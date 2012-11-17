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

-- @brief ToolboxTextArea types and methods.
-- $Author$
-- $Date$
-- $Revision$

with System;                     use System;
with System.Unsigned_Types;      use System.Unsigned_Types;

with RASCAL.Toolbox;             use RASCAL.Toolbox;
with RASCAL.OS;                  use RASCAL.OS;

package RASCAL.ToolboxTextArea is

   --
   -- Returns the state (flags) of the TextArea.
   --
   function Get_State (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) return integer;

   --
   -- Sets the state (flags) of the textarea.
   --
   procedure Set_State (Window    : in Object_ID;
                         Component : in Component_ID;
                         State     : in integer;
                         Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the text of the TextArea.
   --
   procedure Set_Text (Window    : in Object_ID;
                       Component : in Component_ID;
                       Text      : in string;
                       Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the text of the TextArea.
   --
   function Get_Text (Window    : in Object_ID;
                      Component : in Component_ID) return String;

   --
   -- Inserts text into TexTArea at specified position.
   --
   procedure Insert_Text (Window    : in Object_ID;
                          Component : in Component_ID;
                          Text      : in string;
                          Index     : in integer := 0;
                          Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Replaces text at the specified position in TextArea.
   --
   procedure Replace_Text (Window       : in Object_ID;
                           Component    : in Component_ID;
                           Text         : in string;
                           End_Index    : in integer := 0;
                           Start_Index  : in integer := 0;
                           Flags        : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the selected text in the TextArea.
   --
   function Get_Selection (Window    : in Object_ID;
                          Component : in Component_ID) return String;

   --
   -- Returns the index values of the selections start and end in the TextArea.
   --
   procedure Get_Selection_Index (Window     : in Object_ID;
                                  Component  : in Component_ID;
                                  Start_Index: out integer;
                                  End_Index  : out integer);

   --
   -- Selects text by the TextArea.
   --
   procedure Set_Selection (Window      : in Object_ID;
                        Component   : in Component_ID;
                        End_Index   : in integer;
                        Start_Index : in integer :=0;                        
                        Flags       : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the font used in by the TextArea.
   --
   procedure Set_Font (Window       : in Object_ID;
                       Component    : in Component_ID;
                       Font         : in String;
                       Font_Width   : in integer := 12;
                       Font_Height  : in integer := 12;
                       Flags        : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the foreground and background colour used by the TextArea.
   --
   procedure Set_Colour (Window     : in Object_ID;
                         Component  : in Component_ID;
                         Foreground : in integer := 12;
                         Background : in integer := 12;
                         Flags      : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Returns the background and foreground colours of the TextArea.
   --
   procedure Get_Colour (Window    : in Object_ID;
                         Component : in Component_ID;
                         Foreground: out integer;
                         Background: out integer;
                         Flags     : in System.Unsigned_Types.Unsigned := 0);
      
end RASCAL.ToolboxTextArea;
