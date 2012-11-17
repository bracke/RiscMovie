with Main;                       use Main;
with Ada.Exceptions;
with Reporter;

with RASCAL.TaskManager;

package body Controller_Internet is

   --

   package TaskManager renames RASCAL.TaskManager;
   
   --
   
   procedure Handle (The : in TEL_ViewHomePage_Type) is

      Child : Integer;
   begin
      Child := TaskManager.Start_Task ("URIdispatch http://www.arcsite.de/hp/bracke/");
   exception
      when e: others => Report_Error("HOMEPAGE",Ada.Exceptions.Exception_Information (e));
   end Handle;
   
   --
   
   procedure Handle (The : in TEL_SendEmail_Type) is

      Child : Integer;
   begin
      Child := TaskManager.Start_Task ("URIdispatch mailto:bbracke@web.de");
   exception
      when e: others => Report_Error("SENDEMAIL",Ada.Exceptions.Exception_Information (e));            
   end Handle;

   --

end Controller_Internet;
