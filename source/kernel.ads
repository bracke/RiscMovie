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

-- @brief Interface to host RISC OS
-- $Author$
-- $Date$
-- $Revision$

--  --------------------------------------------------------------------------
--  THIS FILE AND ANY ASSOCIATED DOCUMENTATION IS PROVIDED "AS IS" WITHOUT
--  WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
--  TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
--  PARTICULAR PURPOSE.  The user assumes the entire risk as to the accuracy
--  and the use of this file.
--
--  Ada version Copyright (c) P.J.Burwood, 1996
--  Royalty-free, unlimited, worldwide, non-exclusive use, modification,
--  reproduction and further distribution of this Ada file is permitted.
--
--  C version contains additional copyrights, see below
--  --------------------------------------------------------------------------

with Interfaces.C;
with System;

package Kernel is

   subtype void         is System.Address;
   subtype void_ptr     is System.Address;
   subtype void_ptr_ptr is System.Address;

   --
   --  kernel.h:18
   --
   type vector_of_c_signed_int is
      array (integer range <>) of Interfaces.C.int;

   --
   --  only r0 - r9 matter for swi's
   --  kernel.h:19
   --
   type SWI_Regs is                          
      record
         R : vector_of_c_signed_int (0 .. 9); 
      end record;
   pragma Convention (C,  SWI_Regs);

   --
   --  error number
   --  error message (zero terminated)
   --  kernel.h:36
   --
   type oserror is                                  
      record
         ErrNum : Interfaces.C.int; 
         ErrMess : Interfaces.C.char_array (0 .. 251); 
      end record;
   pragma Convention (C,  oserror);
   type oserror_access is access all oserror;

   --  kernel.h:83
   kernel_NONX : constant Interfaces.C.unsigned := 16#80000000#;

   --
   --  Generic SWI interface.  Returns NULL if there was no error.
   --  The SWI called normally has the X bit set.  To call a non-X bit set SWI,
   --  kernel_NONX must be orred into no (in which case, if an error occurs,
   --  swi does not return).
   --
   function  swi (no    : Interfaces.C.unsigned;
                  r_in  : access SWI_Regs;
                  r_out : access SWI_Regs)
                          return oserror_access;

   procedure swi (no    : Interfaces.C.unsigned;
                  r_in  : access SWI_Regs;
                  r_out : access SWI_Regs);

   --
   --  As swi, but for use with SWIs which return status in the C flag.
   --  The int to which carry points is set
   --  to reflect the state of the C flag on
   --  exit from the SWI.
   --
   function swi_c (no    : Interfaces.C.unsigned;
                   r_in  : access SWI_Regs;
                   r_out : access SWI_Regs;
                   carry : access Interfaces.C.int)
                           return oserror_access;

   procedure swi_c (no    : Interfaces.C.unsigned;
                    r_in  : access SWI_Regs;
                    r_out : access SWI_Regs;
                    carry : access Interfaces.C.int);

   --
   --  Returns a pointer to an error block describing the last os error since
   --  last_oserror was last called (or since the program started if there has
   --  been no such call).  If there has been no os error, returns a null
   --  pointer.  Note that occurrence of a further error may overwrite the
   --  contents of the block.
   --  If swi caused the last os error, the error already returned by that call
   --  gets returned by this too.
   --
   function last_oserror return oserror_access; --  kernel.h:199

private

   --  kernel.h:84
   pragma Import (C, swi, "_kernel_swi");

   --  kernel.h:93
   pragma Import (C, swi_c, "_kernel_swi_c");

   --  kernel.h:199
   pragma Import (C, last_oserror, "_kernel_last_oserror");

   --
   --  Interface to host OS.
   --  Copyright (C) Acorn Computers Ltd., 1990
   --

end Kernel;
