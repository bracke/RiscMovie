with RASCAL.FileExternal;            use RASCAL.FileExternal;
with RASCAL.FileName;                use RASCAL.FileName;
with RASCAL.Utility;                 use RASCAL.Utility;
with RASCAL.WimpTask;                use RASCAL.WimpTask;
with RASCAL.Variable;

with Movies;                         use Movies;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Main;                           use Main;
with Model;                          use Model;
with Ada.Characters.Handling;        use Ada.Characters.Handling;
with Ada.Exceptions;
with Reporter;

package body Controller_NewList is

   --

   package FileExternal renames RASCAL.FileExternal;
   package FileName     renames RASCAL.FileName;    
   package Utility      renames RASCAL.Utility;     
   package WimpTask     renames RASCAL.WimpTask;    
   package Variable     renames RASCAL.Variable;
   package Toolbox      renames RASCAL.Toolbox;
   package ToolboxWindow renames RASCAL.ToolboxWindow;

   --

   procedure Handle (The : in TEL_NewList_Selected) is

      Movies : Movies_Instance_Pointer := new Movies_Instance_Type;
   begin
      Movies.all.Main_ID        := Toolbox.Create_Object("Window");
      AddToRear(Main.Model,Movies);

      Movies.all.Path := Untitled_String;
      ToolboxWindow.Set_Title (Movies.all.Main_ID,S(Movies.all.Path));

      Show_Centered(Movies.all.Main_ID);
      
   exception
      when ex: others => Report_Error("HANDLE_NEWLIST",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

end Controller_NewList;