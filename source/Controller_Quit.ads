with RASCAL.ToolboxQuit;          use RASCAL.ToolboxQuit;
with RASCAL.TaskManager;          use RASCAL.TaskManager;
with RASCAL.Toolbox;              use RASCAL.Toolbox;
with RASCAL.OS;                   use RASCAL.OS;

with Model;                       use Model;

package Controller_Quit is
   
   type TEL_Quit_Quit             is new ATEL_Toolbox_Quit_Quit with null record;
   type MEL_Message_Quit          is new AMEL_Message_Quit      with null record;
   type MEL_Message_PreQuit       is new AMEL_Message_preQuit   with null record;

   --
   -- The system is about to shutdown, do we have any unsaved data ?
   --
   procedure Handle (The : in MEL_Message_PreQuit);

   --
   -- The toolbox wants us to quit.
   --
   procedure Handle (The : in TEL_Quit_Quit);

   --
   -- The Task manager wants us to quit.
   --
   procedure Handle (The : in MEL_Message_Quit);

end Controller_Quit;
