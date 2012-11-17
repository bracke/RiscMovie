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

with Unchecked_Deallocation;

package body HandlerList is

   procedure Dispose is
              new Unchecked_Deallocation (Object => Node, Name => Position);

   --
   
   function Allocate (X : Event_Pointer; P : Position)
                                              return Position;

   function Allocate (X : Event_Pointer; P : Position)
                                              return Position is
      Result : Position;
     
   begin

      Result := new Node'(Info => X, Link => P);
      return Result;
      
      exception
      
         when Storage_Error =>
            raise OutOfSpace;

   end Allocate;

   --

   procedure Deallocate (P : in out Position);

   --

   procedure Deallocate (P : in out Position) is
   begin
      Dispose (X => P);
   end Deallocate;

   --
         
   procedure AddToRear (L : in out List; X : Event_Pointer) is

      P : Position;

   begin

      P := Allocate (X, null);
      if L.Head = null then
         L.Head := P;
      else
         L.Tail.Link := P;
      end if;
      L.Tail := P;
   end AddToRear;

   --

   function IsEmpty (L : List) return Boolean is
   begin
      return L.Head = null;
   end IsEmpty;

   --
   
   function IsFirst (L : List; P : Position) return Boolean is
   begin
      return (L.Head /= null) and (P = L.Head);
   end IsFirst;

   --

   function IsLast (L : List; P : Position) return Boolean is
   begin
      return (L.Tail /= null) and (P = L.Tail);
   end IsLast;

   --

   function IsPastEnd (L : List; P : Position) return Boolean is
   begin
      return P = null;
   end IsPastEnd;

   --

   function IsPastBegin (L : List; P : Position) return Boolean is
   begin
      return P = null;
   end IsPastBegin;

   --

   function First (L : List) return Position is
   begin
      return L.Head;
   end First;

   --

   function Retrieve (L : in List; P : in Position)
                              return Event_Pointer is
   begin
      if IsEmpty (L) then
         raise EmptyList;
      elsif IsPastBegin (L, P) then
         raise PastBegin;
      elsif IsPastEnd (L, P) then
         raise PastEnd;
      else
         return P.Info;
      end if;
   end Retrieve;

   --

   procedure GoAhead (L : List; P : in out Position) is
   begin
      if IsEmpty (L) then
         raIse EmptyList;
      elsif IsPastEnd (L, P) then
         raise PastEnd;
      else
         P := P.Link;
      end if;
   end GoAhead;

   --

   procedure GoBack (L : List; P : in out Position) is
      Current : Position;
   begin

      if IsEmpty (L) then
         raise EmptyList;
      elsif IsPastBegin (L, P) then
         raise PastBegin;
      elsif IsFirst (L, P) then
         P := null;
      else                    --  see whether P is in the list
         Current := L.Head;
         while (Current /= null) and then (Current.Link /= P) loop
            Current := Current.Link;
         end loop;
      
         if Current = null then --  P was not in the list
            raise PastEnd;
         else
            P := Current;        --  return predecessor pointer
         end if;
      end if;
   end GoBack;

   --
      
   procedure Delete (L : in out List; P : Position) is
      Previous : Position;
      Current  : Position;
   begin
      Current := P;
      if IsEmpty (L) then
         raise EmptyList;
      elsif IsPastBegin (L, Current) then
         raise PastBegin;
      elsif IsFirst (L, Current) then  --  must adjust list header
         L.Head := Current.Link;
         if L.Head = null then         --  deleted the only node
            L.Tail := null;
         end if;
      else                            --  "normal" situation
         Previous := Current;
         GoBack (L, Previous);
         Previous.Link := Current.Link;
         if IsLast (L, Current) then     --  deleted the last node
            L.Tail := Previous;
         end if;
      end if;
      
      Deallocate (Current);
   end Delete;

   --

   function Get_Event_Code (L : in List) return integer is
   begin
   
      return L.Code;
   
   end Get_Event_Code;

   --
     
end HandlerList;
