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

-- $Author$
-- $Date$
-- $Revision$

with RASCAL.Variable;

package body RASCAL.Choices is

   package Variable renames RASCAL.Variable;
   
   --

   function Get_Read_Path (Choice : Choices_Type) return String is
   begin
      return S(Choice.Read);
   end Get_Read_Path;

   --

   function Get_Write_Path (Choice : Choices_Type) return String is
   begin
      return S(Choice.Write);
   end Get_Write_Path;

   --

   procedure Initialize (The : in out Choices_Type) is

      AppName       : String := S(The.App.all);
      ChoicesRead   : String := Variable.Get_Value("Choices$Path");
      ChoicesWrite  : String := Variable.Get_Value("Choices$Write");
      ChoicesApp    : String := Variable.Get_Value(AppName & "$Dir");
      ChoicesAppVar : String := Variable.Get_Value(AppName & "Choices$Dir");
   begin
      if ChoicesAppVar'Length = 0 then
         if ChoicesRead'Length = 0 or ChoicesWrite'Length = 0 then
            if ChoicesApp'Length = 0 then
               raise Unable_To_Create_Choices;
            end if;
            The.Write := U("<" & AppName & "$Dir>.Choices.");
            The.Read  := U("<" & AppName & "$Dir>.Choices.");
         else
            The.Write := U("<Choices$Write>." & AppName & ".");
            The.Read  := U("Choices:" & AppName & ".");
         end if;
      else
         The.Write := U("<" & AppName & "Choices$Dir>.");
         The.Read  := U("<" & AppName & "Choices$Dir>.");
      end if;
   end Initialize;

   --

end RASCAL.Choices;