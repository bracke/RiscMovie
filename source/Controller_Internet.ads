with RASCAL.OS;                   use RASCAL.OS;

package Controller_Internet is

   type TEL_ViewHomePage_Type     is new Toolbox_UserEventListener(16#18#,-1,-1) with null record;
   type TEL_SendEmail_Type        is new Toolbox_UserEventListener(16#19#,-1,-1) with null record;

   --
   -- The user wants to view the homepage.
   --
   procedure Handle (The : in TEL_ViewHomePage_Type);

   --
   -- The user wants to send an email.
   --
   procedure Handle (The : in TEL_SendEmail_Type);

private
end Controller_Internet;
