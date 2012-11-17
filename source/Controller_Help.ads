with RASCAL.OS;                use RASCAL.OS;

package Controller_Help is

   type TEL_ViewManual_Type       is new Toolbox_UserEventListener(16#11#,-1,-1) with null record;
   type TEL_ViewSection_Type      is new Toolbox_UserEventListener(16#20#,-1,-1) with null record;
   type TEL_ViewIHelp_Type        is new Toolbox_UserEventListener(16#17#,-1,-1) with null record;

   --
   -- The user wants to view the manual.
   --
   procedure Handle (The : in TEL_ViewManual_Type);

   --
   -- The user wants to view a particular section of the manual.
   --
   procedure Handle (The : in TEL_ViewSection_Type);

   --
   -- The user wants to use the interactive help - activate or run it if its not loaded already.
   --
   procedure Handle (The : in TEL_ViewIHelp_Type);

end Controller_Help;
