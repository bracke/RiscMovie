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

-- @brief List types and methods.
-- $Author$
-- $Date$
-- $Revision$

with HandlerList; use HandlerList;

package List is

   type Position is private;
   type List is limited private;
   type ListPointer is access List;

   -- raised if no space left for a new node
   OutOfSpace : exception;

   -- raised if a Position is past the end
   PastEnd   : exception;

   -- raised if a Position is before the begin
   PastBegin : exception;
   
   EmptyList : exception;

   --
   --  Pre:  L and X are defined
   --  Post: a node containing X is inserted
   --   at the front or rear of L, respectively
   --
   procedure AddToRear (L : in out List; X : HandlerList.ListPointer);
   
   function First (L : List) return Position;

   --
   --  Pre:    L and P are defined; P designates a node in L
   --  Post:   returns the value of the element at position P
   --  Raises: EmptyList if L is empty
   --         PastBegin if P points before the beginning of L
   --         PastEnd   if P points beyond the end of L
   --
   function Retrieve (L : in List; P : in Position)
                            return HandlerList.ListPointer;

   --
   --  Pre:    L and P are defined; P designates a node in L
   --  Post:   the node at position P of L is deleted
   --  Raises: EmptyList if L is empty
   --         PastBegin if P is NULL
   --
   procedure Delete (L : in out List; P : Position);
   

   --
   --  Pre:    L and P are defined; P designates a node in L
   --  Post:   P is advanced to designate the next node of L
   --  Raises: EmptyList if L is empty
   --         PastEnd   if P points beyond the end of L
   --
   procedure GoAhead (L : List; P : in out Position);
   

   --
   --  Pre:    L and P are defined; P designates a node in L
   --  Post:   P is moved to designate the previous node of L
   --  Raises: EmptyList if L is empty
   --         PastBegin if P points beyond the end of L
   --
   procedure GoBack    (L : List; P : in out Position);

   function  IsEmpty   (L : List) return Boolean;
   function  IsFirst   (L : List; P : Position) return Boolean;
   function  IsLast    (L : List; P : Position) return Boolean;
   function  IsPastEnd (L : List; P : Position) return Boolean;

   --
   --  Pre:    L and P are defined
   --  Post:   return True iff the condition is met; False otherwise
   --
   function  IsPastBegin (L : List; P : Position) return Boolean;
   

private

   type Node;
   type Position is access Node;
   
   type Node is record
     Info : HandlerList.Listpointer;
     Link : Position;
   end record;
   
   type List is record
     Head : Position;
     Tail : Position;
   end record;

------------------------------------------------------------------------
--  | Generic ADT for one-way linked lists
--  | Author: Michael B. Feldman, The George Washington University
--  | Last Modified: January 1996
------------------------------------------------------------------------

end List;
