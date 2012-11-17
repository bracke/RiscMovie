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

with Kernel;            use Kernel;
with Interfaces.C;      use Interfaces.C;
with Reporter;

with RASCAL.Utility;    use RASCAL.Utility;
with RASCAL.Memory;     use RASCAL.Memory;
with RASCAL.OS;         use RASCAL.OS;

package body RASCAL.Wimp is

   Wimp_Initialise     : constant := 16#400C0#;
   Wimp_CloseDown      : constant := 16#400DD#;
   
   Wimp_Poll           : constant := 16#400C7#;
   Wimp_PollIdle       : constant := 16#400E1#;
   
   
   Wimp_ProcessKey     : constant := 16#400DC#;
   
   
   Wimp_SendMessage    : constant := 16#400E7#;
   
   Wimp_SlotSize       : constant := 16#400EC#;
   Wimp_TransferBlock  : constant := 16#400F1#;
   
   Wimp_SpriteOp       : constant := 16#400E9#;

   number_of_messages   : constant := 10;
   
   type messagestyp is array (0..number_of_messages) of Integer;
   
   messages_to_receive : messagestyp;
   reasoncode          : reasoncodetyp;
   --currentosversion    : Integer;
   --wimp_appname : Ustring;

end RASCAL.Wimp;