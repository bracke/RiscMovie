with RASCAL.OS;                   use RASCAL.OS;

package Controller_Select is

   type TEL_Select_Type   is new Toolbox_UserEventListener(16#57#,-1,-1) with null record;
   type TEL_DeSelect_Type is new Toolbox_UserEventListener(16#58#,-1,-1) with null record;

   --
   -- The user has clicked on the select/deselect button with SELECT.
   --
   procedure Handle (The : in TEL_Select_Type);

   --
   -- The user has clicked on the select/deselect button with ADJUST.
   --
   procedure Handle (The : in TEL_DeSelect_Type);

end Controller_Select;