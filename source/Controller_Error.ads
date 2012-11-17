with RASCAL.Toolbox; use RASCAL.Toolbox;

package Controller_Error is

   type TEL_Toolbox_Error is new ATEL_Toolbox_Error with null record;

   --
   -- A Toolbox Error has occurred.
   --
   procedure Handle (The : in TEL_Toolbox_Error);

end Controller_Error;
