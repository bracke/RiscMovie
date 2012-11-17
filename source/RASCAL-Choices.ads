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

-- @brief Subprograms to find choices read and write path. Uses <AppnameChoices$Dir> if it exists and Choices:Appname and <Choices$Write>.Appname otherwise. 
-- $Author$
-- $Date$
-- $Revision$

with RASCAL.Utility;        use RASCAL.Utility;
with Ada.Finalization;

package RASCAL.Choices is

   Unable_To_Create_Choices : exception;

   type UString_Ptr is access UString;

   type Choices_Type(AppName : UString_Ptr) is 
                     new Ada.Finalization.Limited_Controlled with private;

   --
   -- Returns a read-path.
   --
   function Get_Read_Path (Choice : in Choices_Type) return String;

   --
   -- Returns a write-path.
   --
   function Get_Write_Path (Choice : in Choices_Type) return String;

private

   procedure Initialize (The : in out Choices_Type);

   type Choices_Type(AppName : UString_Ptr) is
        new Ada.Finalization.Limited_Controlled with
   record
   App   : UString_Ptr := AppName;
   Read  : UString;
   Write : Ustring;
   end record;

end RASCAL.Choices;
