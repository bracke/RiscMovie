with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Movies;                use Movies;

with RASCAL.Utility;        use RASCAL.Utility;
with RASCAL.OS;             use RASCAL.OS;

package Model is

   type Position is private;
   type Instances is limited private;
   type InstancesPointer is access Instances;

   type Movies_Instance_Type is
   record
   Movie_List          : Movies.ListPointer := new List;
   Main_ID             : Object_ID;
   DCS_ID              : Object_ID                 := 0;
   Path                : Unbounded_String          := U("");
   Modified            : Boolean                   := false;
   Format              : Unbounded_String          := U("");
   Creator             : Unbounded_String          := U("");
   Version             : Unbounded_String          := U("");
   Date                : Unbounded_String          := U("");
   end record;

   type Movies_Instance_Pointer is access Movies_Instance_Type;

   --
   -- Returns the position of the mode in the model.
   --
   function Find_Movies (Model    : in Instances;
                         Movies_P : in Movies.ListPointer) return Position;

   --
   -- Deletes the mode.
   --
   procedure Delete_Movies (Model    : in out Instances;
                            Movies_P : in out Movies.ListPointer);

   --
   -- Deletes the mode instance with Main_ID as a main window.
   --
   procedure Delete_Movies (Model   : in out Instances;
                            Main_ID : in Object_ID);

   --
   -- Returns the mode instance which the DCS object belongs to.
   --
   function Get_Closed (Model : in Instances;
                        DCD   : in Object_ID) return Movies_Instance_Pointer;

   --
   -- Returns the first mode instance containing unsaved data. If all data has been saved null is returned.
   --
   function Get_Changed(Model   : in Instances) return Movies_Instance_Pointer;

   --
   -- Searches the model for a mode instance with Main_ID as a main window.
   --
   function Get_Instance (Model   : in Instances;
                          Main_ID : in Object_ID) return Movies_Instance_Pointer;


   OutOfSpace      : exception; -- raised if no space left for a new node
   PastEnd         : exception; -- raised if a Position is past the end
   PastBegin       : exception; -- raised if a Position is before the begin
   EmptyInstances  : exception;

   --
   --  Pre:  L and X are defined
   --  Post: a node containing X is inserted
   --        at the front or rear of L, respectively
   --
   procedure AddToRear (L : in out Instances; X : Movies_Instance_Pointer);

   function First (L : Instances) return Position;

   --
   --  Pre:    L and P are defined; P designates a node in L
   --  Post:   returns the value of the element at position P
   --  Raises: EmptyInstances if L is empty
   --          PastBegin if P points before the beginning of L
   --          PastEnd   if P points beyond the end of L
   --
   function Retrieve (L : in Instances; P : in Position)
                           return Movies_Instance_Pointer;

   --
   --  Pre:    L and P are defined; P designates a node in L
   --  Post:   the node at position P of L is deleted
   --  Raises: EmptyInstances if L is empty
   --          PastBegin if P is NULL
   --
   procedure Delete (L : in out Instances; P : Position);

   --
   --  Pre:    L and P are defined; P designates a node in L
   --  Post:   P is advanced to designate the next node of L
   --  Raises: EmptyInstances if L is empty
   --          PastEnd   if P points beyond the end of L
   --
   procedure GoAhead (L : Instances; P : in out Position);

   --
   --  Pre:    L and P are defined; P designates a node in L
   --  Post:   P is moved to designate the previous node of L
   --  Raises: EmptyInstances if L is empty
   --          PastBegin if P points beyond the end of L
   --
   procedure GoBack    (L : Instances; P : in out Position);
   
   function  IsEmpty   (L : Instances) return Boolean;
   function  IsFirst   (L : Instances; P : Position) return Boolean;
   function  IsLast    (L : Instances; P : Position) return Boolean;
   function  IsPastEnd (L : Instances; P : Position) return Boolean;

   --
   --  Pre:    L and P are defined
   --  Post:   return True if the condition is met; False otherwise
   --
   function  IsPastBegin (L : Instances; P : Position) return Boolean;


private

   type Node;
   type Position is access Node;
   
   type Node is record
     Info : Movies_Instance_Pointer;
     Link : Position;
   end record;
   
   type Instances is record
     Head : Position;
     Tail : Position;
   end record;

------------------------------------------------------------------------
--  | Generic ADT for one-way linked lists
--  | Author: Michael B. Feldman, The George Washington University
--  | Last Modified: January 1996
------------------------------------------------------------------------

end Model;
