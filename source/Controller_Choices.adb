with RASCAL.Utility;             use RASCAL.Utility;
with RASCAL.UserMessages;

with Interfaces.C;               use Interfaces.C;
with Main;                       use Main;
with Main.Choices;               use Main.Choices;     
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with View_Main;
with Ada.Exceptions;
with Reporter;

package body Controller_Choices is

   --

   package Utility      renames RASCAL.Utility;
   package UserMessages renames RASCAL.UserMessages;

   --

   procedure Handle (The : in TEL_ViewChoices_Type) is
   begin
      Call_OS_CLI("Filer_Run <RiscMovie$Dir>.!Configure");
   exception
      when ex: others => Report_Error("CONFIGURE",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

   procedure Handle (The : in MEL_Message_ConfiX) is

      Message : String := To_Ada(The.Event.all.Message);
   begin
      if Index(To_Lower(Message),"config") >= Message'First then
         Main.Choices.Read;
         View_Main.Update_AllViews;
      end if;
   exception
      when ex: others => Report_Error("CHOICES",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

end Controller_Choices;
