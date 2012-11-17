with RASCAL.Toolbox;             use RASCAL.Toolbox;
with RASCAL.WimpTask;            use RASCAL.WimpTask;
with RASCAL.Hourglass;

with Main;                       use Main;
with Unchecked_Deallocation;
with Reporter;

package body Model is

   --

   package Toolbox   renames RASCAL.Toolbox;
   package WimpTask  renames RASCAL.WimpTask; 
   package Hourglass renames RASCAL.Hourglass;
                                     
   --

   function Find_Movies (Model   : in Instances;
                         Movies_P: in Movies.ListPointer) return Position is
      i      : Position;
      Movies : Movies_Instance_Pointer;
   begin
      if not isEmpty(Model) then
         i := First(Model);
         loop
            Movies := Retrieve (Model,i);
            if Movies.all.Movie_List = Movies_P then
               exit;
            end if;
            exit when IsLast(Model,i);
            GoAhead(Model,i);
         end loop;
      end if;
      return i;
   end Find_Movies;

   --

   procedure Delete_Movies (Model    : in out Instances;
                            Movies_P : in out Movies.ListPointer) is
   begin
      Delete(Model,Find_Movies(Model,Movies_P));
   end Delete_Movies;

   --

   procedure Delete_Movies (Model   : in out Instances;
                            Main_ID : in Object_ID) is
      i    : Position;
      Movies_P : Movies_Instance_Pointer;
   begin
      if WimpTask.Get_Status (Wimp_Task_Class(Main_task)) then
         Hourglass.Start(1);
      end if;
      if not isEmpty(Model) then
         i := First(Model);
         loop
            Movies_P := Retrieve (Model,i);
            if Movies_P.all.Main_ID = Main_ID then
               exit;
            end if;
            exit when IsLast(Model,i);
            GoAhead(Model,i);
         end loop;
      end if;
      if Movies_P.all.Main_ID = Main_ID then
         Toolbox.Hide_Object(Movies_P.all.Main_ID);
         Toolbox.Delete_Object(Movies_P.all.Main_ID);
         Movies.Delete_List(Movies_P.all.Movie_List.all);
         Delete(Model,i);
      end if;
      if WimpTask.Get_Status (Wimp_Task_Class(Main_task)) then
         Hourglass.Off;
      end if;
   exception
      when others =>
                     if WimpTask.Get_Status (Wimp_Task_Class(Main_task)) then
                        Hourglass.Off;
                     end if;   
   end Delete_Movies;

   --

   function Get_Closed (Model   : in Instances;
                        DCD     : in Object_ID) return Movies_Instance_Pointer is

      i        : Position;
      Movies     : Movies_Instance_Pointer;
   begin
      if not isEmpty(Model) then
         i := First(Model);
         loop
            Movies := Retrieve (Model,i);
            if Movies.all.DCS_ID = DCD then
               return Movies;
            end if;
            exit when IsLast(Model,i);
            GoAhead(Model,i);
         end loop;
      end if;
      return null;      
   end Get_Closed;

   --
   
   function Get_Changed(Model   : in Instances) return Movies_Instance_Pointer is

      i        : Position;
      Movies     : Movies_Instance_Pointer;
   begin
      if not isEmpty(Model) then
         i := First(Model);
         loop
            Movies := Retrieve (Model,i);
            if Movies.all.Modified then
               return Movies;
            end if;
            exit when IsLast(Model,i);
            GoAhead(Model,i);
         end loop;
      end if;
      return null;
   end Get_Changed;

   --

   function Get_Instance (Model   : in Instances;
                          Main_ID : in Object_ID) return Movies_Instance_Pointer is
      Start : Position;
      i     : Position;
      Movies  : Movies_Instance_Pointer;
   begin
      if not isEmpty(Model) then
         Start := First(Model);
         i     := Start;
         loop
            Movies := Retrieve (Model,i);
            if Movies.all.Main_ID = Main_ID then
               return Movies;
            end if;
            exit when IsLast(Model,i);
            GoAhead(Model,i);
         end loop;
      end if;
      return null;
   end Get_Instance;

   --
   
   procedure Dispose is
              new Unchecked_Deallocation (Object => Node, Name => Position);

   --
   
   function Allocate (X : Movies_Instance_Pointer; P : Position)
                                              return Position;

   function Allocate (X : Movies_Instance_Pointer; P : Position)
                                              return Position is
      Result : Position;
   begin
      Result := new Node'(Info => X, Link => P);
      return Result;
      exception
         when Storage_Error => raise OutOfSpace;
   end Allocate;

   --

   procedure Deallocate (P : in out Position);

   --

   procedure Deallocate (P : in out Position) is
   begin
      Dispose (X => P);
   end Deallocate;

   --
         
   procedure AddToRear (L : in out Instances; X : Movies_Instance_Pointer) is

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

   function IsEmpty (L : Instances) return Boolean is
   begin
      return L.Head = null;
   end IsEmpty;

   --
   
   function IsFirst (L : Instances; P : Position) return Boolean is
   begin
      return (L.Head /= null) and (P = L.Head);
   end IsFirst;

   --

   function IsLast (L : Instances; P : Position) return Boolean is
   begin
      return (L.Tail /= null) and (P = L.Tail);
   end IsLast;

   --

   function IsPastEnd (L : Instances; P : Position) return Boolean is
   begin
      return P = null;
   end IsPastEnd;

   --

   function IsPastBegin (L : Instances; P : Position) return Boolean is
   begin
      return P = null;
   end IsPastBegin;

   --

   function First (L : Instances) return Position is
   begin
      return L.Head;
   end First;

   --

   function Retrieve (L : in Instances; P : in Position)
                              return Movies_Instance_Pointer is
   begin
      if IsEmpty (L) then
         raise EmptyInstances;
      elsif IsPastBegin (L, P) then
         raise PastBegin;
      elsif IsPastEnd (L, P) then
         raise PastEnd;
      else
         return P.Info;
      end if;
   end Retrieve;

   --

   procedure GoAhead (L : Instances; P : in out Position) is
   begin
      if IsEmpty (L) then
         raIse EmptyInstances;
      elsif IsPastEnd (L, P) then
         raise PastEnd;
      else
         P := P.Link;
      end if;
   end GoAhead;

   --

   procedure GoBack (L : Instances; P : in out Position) is
      Current : Position;
   begin

      if IsEmpty (L) then
         raise EmptyInstances;
      elsif IsPastBegin (L, P) then
         raise PastBegin;
      elsif IsFirst (L, P) then
         P := null;
      else                    --  see whether P is in the Instances
         Current := L.Head;
         while (Current /= null) and then (Current.Link /= P) loop
            Current := Current.Link;
         end loop;
      
         if Current = null then --  P was not in the Instances
            raise PastEnd;
         else
            P := Current;        --  return predecessor pointer
         end if;
      end if;
   end GoBack;

   --
      
   procedure Delete (L : in out Instances; P : Position) is
      Previous : Position;
      Current  : Position;
   begin
      Current := P;
      if IsEmpty (L) then
         raise EmptyInstances;
      elsif IsPastBegin (L, Current) then
         raise PastBegin;
      elsif IsFirst (L, Current) then  --  must adjust Instances header
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

end Model;