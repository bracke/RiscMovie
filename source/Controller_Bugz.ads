with RASCAL.ToolboxQuit;       use RASCAL.ToolboxQuit;
with RASCAL.TaskManager;       use RASCAL.TaskManager;
with RASCAL.OS;                use RASCAL.OS;
with RASCAL.Bugz;              use RASCAL.Bugz;

package Controller_Bugz is

   type MEL_Message_Bugz_Query    is new AMEL_Message_Bugz_Query with null record;
   type TEL_CreateReport_Type     is new Toolbox_UserEventListener(16#21#,-1,-1) with null record;

   --
   -- A request from the Bugz application.
   --
   procedure Handle (The : in MEL_Message_Bugz_Query);   

   --
   -- The user wants to create a bug report using !Bugz
   --
   procedure Handle (The : in TEL_CreateReport_Type);

end Controller_Bugz;
