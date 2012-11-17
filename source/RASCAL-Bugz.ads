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

-- @brief Event definitions to handle messages from !Bugz.
-- $Author$
-- $Date$
-- $Revision$

with Interfaces.C;             use Interfaces.C;
with System;                   use System;

with RASCAL.OS;                use RASCAL.OS;

package RASCAL.Bugz is

   type Bugz_Query_Type is (File_Request);
   
   --
   -- This message is sent by Bugz, or other Bug reporting or interested application to gain information about the application.
   --It may be extended at some future date to include email addresses, version strings or other application specific debugging information.
   --Applications should identify the type of the query and only respond if it understands it.
   --
   type Message_Bugz_Query is
   record
   Header         : Message_Event_Header;
   Query_Type     : Bugz_Query_Type;
   end record;
   pragma Convention (C, Message_Bugz_Query);

   type Message_Bugz_Query_Pointer is access Message_Bugz_Query;

   type AMEL_Message_Bugz_Query is abstract new
        Message_EventListener(Message_Event_Bugz_Query) with
   record
   Event : Message_Bugz_Query_Pointer;
   end record;

   --
   -- This message is sent by a Bugz aware program to ask Bugz to open a report window on the Bugz file named in the block.
   --Bugz will acknowledge the message, therefore if the message bounces you may wish to either notify the user or run Bugz manually.
   --Bugz is identified with \<Bugz$Dir\> being set to the application directory.
   --This message is the usual response to Bugz file requests.
   --
   type Message_Bugz_BugzFile is
   record
   Header         : Message_Event_Header;
   Filename       : Char_Array (1..208);
   end record;
   pragma Convention (C, Message_Bugz_BugzFile);

   type Message_Bugz_BugzFile_Pointer is access Message_Bugz_BugzFile;

   type AMEL_Message_Bugz_BugzFile is abstract new
        Message_EventListener(Message_Event_Bugz_BugzFile) with
   record
   Event : Message_Bugz_BugzFile_Pointer;
   end record;

end RASCAL.Bugz;