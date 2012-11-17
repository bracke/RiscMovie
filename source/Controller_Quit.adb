with RASCAL.Utility;             use RASCAL.Utility;
with RASCAL.WimpTask;            use RASCAL.WimpTask;
with RASCAL.UserMessages;        use RASCAL.UserMessages;
with RASCAL.TaskManager;

with Reporter;
with Main;                       use Main;
with Ada.Exceptions;

package body Controller_Quit is

   --

   package Utility      renames RASCAL.Utility;
   package WimpTask     renames RASCAL.WimpTask;    
   package UserMessages renames RASCAL.UserMessages;
   package TaskManager  renames RASCAL.TaskManager; 
   package Toolbox      renames RASCAL.Toolbox;
   
   --

   procedure Handle (The : in TEL_Quit_Quit) is

      Movies : Movies_Instance_Pointer := null;
   begin
      Movies := Get_Changed(Main.Model);
      if Movies /= null then
         Shutdown := false;
         Movies.all.DCS_ID := Create_Object("DCS");
         Toolbox.Show_Object(Movies.all.DCS_ID,Movies.all.Main_ID,0,AtPointer);
      else
         Set_Status(Wimp_Task_Class(Main_Task),false);
      end if;
   exception
      when e: others => Report_Error("TQUIT",Ada.Exceptions.Exception_Information (e));
   end Handle;
   
   --

   procedure Handle (The : in MEL_Message_Quit) is

      Movies : Movies_Instance_Pointer := null;
   begin
      Movies := Get_Changed(Main.Model);
      if Movies /= null then
         Shutdown := false;
         Movies.all.DCS_ID := Create_Object("DCS");
         Toolbox.Show_Object(Movies.all.DCS_ID,Movies.all.Main_ID,0,AtPointer);
      else
         Set_Status(Wimp_Task_Class(Main_Task),false);
      end if;
   exception
      when e: others => Report_Error("MQUIT",Ada.Exceptions.Exception_Information (e));      
   end Handle;

   --


   procedure Handle (The : in MEL_Message_PreQuit) is

      Flag   : Integer := The.Event.all.Flag;
      Movies : Movies_Instance_Pointer := null;
   begin
      Movies := Get_Changed(Main.Model);
      if Movies /= null then
         Shutdown := true;
         UserMessages.Acknowledge(The.Event.all'Address);
         Movies.all.DCS_ID := Create_Object("DCS");
         Toolbox.Show_Object(Movies.all.DCS_ID,Movies.all.Main_ID,0,AtPointer);
      end if;
   exception
      when ex: others => Report_Error("HANDLE_PREQUIT",Ada.Exceptions.Exception_Information (ex));      
   end Handle;


end Controller_Quit;
