with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Movies is

   type Movie_Type is
   record
   Title_1   : Unbounded_String;
   Title_2   : Unbounded_String;
   Title_3   : Unbounded_String;
   Categories: Unbounded_String;
   Media     : Unbounded_String;
   Languages : Unbounded_String;
   Subtitles : Unbounded_String;
   Notes     : Unbounded_String;
   ID        : Natural := 0;
   end record;

   type Element_Pointer is access Movie_Type;

   type Node is private;
   type Position is access Node;

   type List        is limited private;
   type ListPointer is access List;

   --
   -- List
   --
   
   OutOfSpace : exception; -- raised if no space left for a new node
   PastEnd    : exception; -- raised if a Position is past the end
   PastBegin  : exception; -- raised if a Position is before the begin
   EmptyList  : exception;

   --
   -- Remove element from List
   --
   procedure Remove_Element (Section : in out Movies.List;
                             Item_ID : in Natural);

   --
   -- Return element from List
   --
   function Get_Element (Section : in Movies.List;
                         Item_ID : in Natural) return Element_Pointer;

   --
   -- Delete all elements in list
   --
   procedure Delete_List (L : in out List);

   --
   --  Pre:  L and X are defined
   --  Post: a node containing X is inserted
   --        at the front or rear of L, respectively
   --
   procedure AddToRear (L : in out List; X : Movies.Element_Pointer);


   --
   --
   --
   --
   procedure Swap (L : in out List;
                   X : in Position;
                   Y : in Position);

   function First (L : List) return Position;

   --
   --  Pre:    L and P are defined; P designates a node in L
   --  Post:   returns the value of the element at position P
   --  Raises: EmptyList if L is empty
   --          PastBegin if P points before the beginning of L
   --          PastEnd   if P points beyond the end of L
   --
   function Retrieve (L : in List; P : in Position)
                           return Movies.Element_Pointer;

   --
   --  Pre:    L and P are defined; P designates a node in L
   --  Post:   the node at position P of L is deleted
   --  Raises: EmptyList if L is empty
   --          PastBegin if P is NULL
   --
   procedure Delete (L : in out List; P : Position);

   --
   --  Pre:    L and P are defined; P designates a node in L
   --  Post:   P is advanced to designate the next node of L
   --  Raises: EmptyList if L is empty
   --          PastEnd   if P points beyond the end of L
   --
   procedure GoAhead (L : List; P : in out Position);

   --
   --  Pre:    L and P are defined; P designates a node in L
   --  Post:   P is moved to designate the previous node of L
   --  Raises: EmptyList if L is empty
   --          PastBegin if P points beyond the end of L
   --
   procedure GoBack    (L : List; P : in out Position);
   
   function  IsEmpty   (L : List) return Boolean;
   function  IsFirst   (L : List; P : Position) return Boolean;
   function  IsLast    (L : List; P : Position) return Boolean;
   function  IsPastEnd (L : List; P : Position) return Boolean;

   --
   --  Pre:    L and P are defined
   --  Post:   return True if the condition is met; False otherwise
   --
   function  IsPastBegin (L : List; P : Position) return Boolean;


private

   type Node is record
     Info : Movies.Element_Pointer := null;
     Link : Position := null;
   end record;
   
   type List is record
     Head : Position := null;
     Tail : Position := null;
   end record;

------------------------------------------------------------------------
--  | Generic ADT for one-way linked lists
--  | Author: Michael B. Feldman, The George Washington University
--  | Last Modified: January 1996
------------------------------------------------------------------------

end Movies;
