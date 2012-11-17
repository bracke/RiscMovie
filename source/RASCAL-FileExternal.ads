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

-- @brief External file handling.
-- $Author$
-- $Date$
-- $Revision$

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System.Unsigned_Types; use System.Unsigned_Types;

with RASCAL.Time;           use RASCAL.Time;
with RASCAL.Utility;        use RASCAL.Utility;

package RASCAL.FileExternal is

   type File_Object_Type is (Not_Found,File_Object,Dir,Image);

   Attribute_Owner_Read      : constant System.Unsigned_Types.Unsigned :=         2#1#;
   Attribute_Owner_Write     : constant System.Unsigned_Types.Unsigned :=        2#10#;
   Attribute_Locked          : constant System.Unsigned_Types.Unsigned :=      2#1000#;
   Attribute_Public_Read     : constant System.Unsigned_Types.Unsigned :=     2#10000#;
   Attribute_Public_Write    : constant System.Unsigned_Types.Unsigned :=    2#100000#;
   Attribure_Hidden          : constant System.Unsigned_Types.Unsigned :=   2#1000000#;
   Attribute_Application     : constant System.Unsigned_Types.Unsigned :=  2#10000000#;

   Copy_Option_Recurse       : constant System.Unsigned_Types.Unsigned :=                 2#1#;
   Copy_Option_Force         : constant System.Unsigned_Types.Unsigned :=                2#10#;
   Copy_Option_Given_Times   : constant System.Unsigned_Types.Unsigned :=               2#100#;
   Copy_Option_Confirm       : constant System.Unsigned_Types.Unsigned :=              2#1000#;
   Copy_Option_Verbose       : constant System.Unsigned_Types.Unsigned :=             2#10000#;
   Copy_Option_Quick         : constant System.Unsigned_Types.Unsigned :=            2#100000#;
   Copy_Option_Prompt        : constant System.Unsigned_Types.Unsigned :=           2#1000000#;
   Copy_Option_Delete        : constant System.Unsigned_Types.Unsigned :=          2#10000000#;
   Copy_Option_PrintProgress : constant System.Unsigned_Types.Unsigned :=         2#100000000#;
   Copy_Option_Access        : constant System.Unsigned_Types.Unsigned :=        2#1000000000#;
   Copy_Option_Stamp         : constant System.Unsigned_Types.Unsigned :=       2#10000000000#;
   Copy_Option_StructureOnly : constant System.Unsigned_Types.Unsigned :=      2#100000000000#;
   Copy_Option_Newer         : constant System.Unsigned_Types.Unsigned :=     2#1000000000000#;
   Copy_Option_UseDescriptor : constant System.Unsigned_Types.Unsigned :=    2#10000000000000#;
   Copy_Option_LookFirst     : constant System.Unsigned_Types.Unsigned :=   2#100000000000000#;

   type Directory_Type is array (natural range <>) of UString;
   type Directory_Pointer is access Directory_Type;

   --
   -- Closes all files in path.
   --
   procedure Close_AllInPath (path : in String);

   --
   -- Find a filename which has not been used yet in 'path'. Returns only name not path.
   --
   function Get_UnUsed_FileName (Path : in String) return String;

   --
   -- Returns true if the object, pointed to by 'Filename', exists.
   --
   function Exists (Filename : in string) return boolean;

   --
   -- Is the path valid for writing to ?
   --
   function Is_Valid (Path     : in String;
                      FileSize : in Natural) return Boolean;

   --
   -- Deletes the file pointed to by 'Filename'.
   --
   procedure Delete_File (Filename : in string);

   --
   -- Rename file.
   --
   procedure Rename (Source : in string;
                     Target : in string);

   --
   -- Copies an object from 'Source' to 'Target'.
   --
   procedure Copy(Source : in string; Target : in string;
                  Flags  : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Moves an object from 'Source' to 'Target'.
   --
   procedure Move(Source : in string; Target : in string;
                  Flags  : in System.Unsigned_Types.Unsigned := Copy_Option_Delete);

   --
   -- Deletes file/directory.
   --
   procedure Wipe(Path : in string);

   --
   -- Creates a file at 'Filename'.
   --
   procedure Create_File (Filename : in string;
                          Length   : in integer := 0;
                          Filetype : in integer := 16#FFD#);

   --
   -- Creates a directory at 'Dirname'. It is not an error if the directory already exists.
   --
   procedure Create_Directory (Dirname : in string);

   --
   -- Returns a string containing a commaseperated list of the names of all files in 'Path'.
   --
   function Get_FileNames (Path : in string) return string;

   --
   -- Returns an array of names of the files in 'Path'.
   --
   function Get_Directory_List (Path       : in String;
                                Count_Dirs : in Boolean :=false) return Directory_Type;

   --
   -- Enumerates through the files in 'Path'.
   --
   procedure Read_Dir(Path  : in  string;
                      Index : in out integer;
                      Name  : out Unbounded_String);

   --
   -- Returns the number of files in 'Path'.
   --
   function Nr_Of_Files (Path : in string;
                         Recursive : in boolean := false;
                         Count_Dirs: in boolean := false) return integer;

   --
   -- Enumerates through the files in 'Directory', while providing extensive information on every file found.
   --
   procedure Get_Directory_Entry (Directory  : in string;
                                  Itemnr     : in out Integer;
                                  Itemname   : out Unbounded_String;
                                  Loadadr    : out Integer;
                                  Execadr    : out Integer;
                                  Length     : out Integer;
                                  Attributes : out Integer;
                                  Itemtype   : out Integer);

   --
   -- Returns information regarding the file 'Filename'.
   --
   procedure Get_File_Information (Filepath   : in string;
                                   Loadadr    : out integer;
                                   Execadr    : out integer;
                                   Length     : out integer;
                                   Attributes : out integer;
                                   Itemtype   : out File_Object_Type);

   --
   -- Sets the filetype if 'Filename'.
   --
   procedure Set_File_Type (Filename : in string;
                            Filetype : in integer);

   --
   -- Finds the filetype of 'Filename'.
   --
   function Get_File_Type (Filename : in string) return integer;

   --
   -- Finds the object type of 'Filename'.
   --
   function Get_Object_Type (Filename : in string) return File_Object_Type;


   --
   --
   --
   function Filetype_To_Hex (Loadadr : in integer) return string;

   --
   --
   --
   function Filetype_To_Number (FileType : in String) return Integer;

   --
   -- Finds the size of 'Filename'.
   --
   function Get_Size (Filename : in string) return integer;

   --
   -- Finds the stamp of 'Directory'.
   --
   procedure Get_Directory_Stamp (Directory : in string;
                                  Loadadr   : out integer;
                                  Execadr   : out integer);

   --
   -- Returns the attributes of 'Path'.
   --
   function Get_Attributes (Path : in string) return integer;

   --
   -- Sets the attributes of 'Path'.
   --
   procedure Set_Attributes (Path : in string; Attributes : in integer);

   --
   -- Returns the stamp of the file pointed to by 'Path'.
   --
   function Get_Stamp (Path : in String) return UTC_Pointer;

end RASCAL.FileExternal;
