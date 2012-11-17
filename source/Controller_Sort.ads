with RASCAL.OS;                   use RASCAL.OS;

package Controller_Sort is

   type TEL_Sort_Type is new Toolbox_UserEventListener(16#55#,-1,-1) with null record;

   --
   -- OK in the Sort window was pressed.
   --
   procedure Handle (The : in TEL_Sort_Type);

end Controller_Sort;