with RASCAL.OS;                   use RASCAL.OS;

package Controller_Find is

   type TEL_Find_Type                      is new Toolbox_UserEventListener(16#52#,-1,-1) with null record;

   --
   -- OK in the find window was pressed.
   --
   procedure Handle (The : in TEL_Find_Type);

end Controller_Find;