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

-- @brief System variable related types and methods.
-- $Author$
-- $Date$
-- $Revision$

package RASCAL.Variable is

   type Variable_Type is (GS_String,Number,Macro,Expanded,Literal);

   --
   -- Returns true if the system variable exists.
   --
   function Exists (Name : in String) return Boolean;

   --
   -- Sets the system variable 'Name' to 'New_Value'.
   --
   procedure Set_Value (Name      : string;
                        New_Value : string;
                        Var_Type  : Variable_Type := Literal);

   --
   -- Unsets (deletes) the variable Name.
   --
   procedure UnSet (Name : string);

   --
   -- Returns the value of the system variable 'Variable'.
   --
   function Get_Value (Variable : in String) return String;

   --
   -- Translates possible system variables etc. in string.
   --
   function Translate (Str : in String) return String;

end RASCAL.Variable;
