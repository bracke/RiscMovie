with RASCAL.OS; use RASCAL.OS;

package Controller_Dummy is
   
   type TEL_Dummy is new Toolbox_UserEventListener(16#59#,-1,-1) with null record;

   --
   -- This does nothing.
   --
   procedure Handle (The : in TEL_Dummy);

end Controller_Dummy;
