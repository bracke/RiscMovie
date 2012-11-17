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

-- @brief Reporter bindings for Ada 95
-- $Author$
-- $Date$
-- $Revision$

with Interfaces.C;
with System;

package Reporter is

   --
   -- Report 'Text'.
   --
   procedure Report (Text : in String);

   --
   -- Clear Reporter log.
   --
   procedure Clear (Text : in String := "");

   --
   -- Switch general reporting on.
   --
   procedure On (Text : in String := "");

   --
   -- Switch general reporting off.
   --
   procedure Off (Text : in String := "");

   --
   -- Closes the Reporter log window.
   --
   procedure Close;

   --
   -- Quits the !Reporter application.
   --
   procedure Quit;

   --
   -- Displays the address of the last abort, and if it is in a module the module name and offset within thr module.
   --
   procedure Where;

   --
   -- Report 'Text'.
   --
   procedure Text (Text : in String := "");

   --
   -- Displays the time, the value of a Wimp Poll reason code and the textual description.
   --
   procedure Poll (Reason : in Natural := 0);

   --
   -- Displays the registers and an optional text heading.
   --
   procedure Registers (Text : in String := "");

   --
   -- Dispays the storage from 'Start' for 'Length' b<tes with display 'width' in hexadecimal and character format.
   --
   procedure Dump (Start : in System.Address;
                   Length: in Natural;
                   Width : in Positive);
                   
   --
   -- Save Reporter log to RAM disc.
   --
   procedure Save (Text : in String := "");

end Reporter;
