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

with RASCAL.Utility;             use RASCAL.Utility;

with Ada.Unchecked_Deallocation;
with Reporter;

package body RASCAL.DragNDrop is

   --

   function Empty return boolean is
   begin
      return Head_Object = null;
   end Empty;

   --

   procedure Push_DragObject(Path   : in Unbounded_String;
                             Window : in Wimp_Handle_Type := 0;
                             Icon   : in Icon_Handle_Type := 0) is
   begin
      if Empty then
         Head_Object := new DragObject'(Window,Icon,Path,Head_Object);
      else
         Current_DragObject := new DragObject'(Window,Icon,Path,Head_Object);
         Head_Object        := Current_DragObject;
      end if;
   end Push_DragObject;

   --

   procedure Remove_DragObject is new Ada.Unchecked_Deallocation(DragObject,DragObject_Pointer);

   --

   procedure Pop_DragObject (Path   : out Unbounded_String;
                             Window : out Wimp_Handle_Type;
                             Icon   : out Icon_Handle_Type) is
   begin
      if not Empty then
         Path   := Head_Object.Filename;
         Window := Head_Object.Window;
         Icon   := Head_Object.Icon;

         Current_DragObject := Head_Object;
         Head_Object        := Current_DragObject.Next;
         Remove_DragObject (Current_DragObject);
      end if;
   end Pop_DragObject;

   --

end RASCAL.DragNDrop;
