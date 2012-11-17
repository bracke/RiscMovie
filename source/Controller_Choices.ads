with RASCAL.ConfiX;            use RASCAL.ConfiX;
with RASCAL.OS;                use RASCAL.OS;

package Controller_Choices is

   type TEL_ViewChoices_Type is new Toolbox_UserEventListener(16#10#,-1,-1) with null record;
   type MEL_Message_ConfiX   is new AMEL_Message_ConfiX                     with null record;

   --
   -- The user wants to view the choices.
   --
   procedure Handle (The : in TEL_ViewChoices_Type);

   --
   -- User has changed the choices - we should read them.
   --
   procedure Handle (The : in MEL_Message_ConfiX);

end Controller_Choices;
