with Ada.Strings.Maps;           use Ada.Strings.Maps;
with Interfaces.C;               use Interfaces.C;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Main;                       use Main;
with Model;                      use Model;
with Kernel;                     use Kernel;
with Movies;                     use Movies;
with CSV;                        use CSV;
with Ada.Exceptions;
with Reporter;

with RASCAL.Utility;             use RASCAL.Utility;
with RASCAL.WimpTask;            use RASCAL.WimpTask;
with RASCAL.OS;                  use RASCAL.OS;
with RASCAL.Toolbox;             use RASCAL.Toolbox;
with RASCAL.ToolboxWindow;

package body Controller_SaveAs is

   --

   package Utility       renames RASCAL.Utility;
   package WimpTask      renames RASCAL.WimpTask;     
   package OS            renames RASCAL.OS;           
   package Toolbox       renames RASCAL.Toolbox;      
   package ToolboxWindow renames RASCAL.ToolboxWindow;
   package ToolboxSaveAs renames RASCAL.ToolboxSaveAs;
   
   --

   procedure Handle (The : in TEL_Toolbox_SaveAs_SaveToFile) is

      Object    : Object_ID              := Get_Self_Id (Main_Task);
      Ancestor  : Object_ID              := Get_Ancestor_Id (Main_Task);
      Parent    : Object_ID              := Get_Parent_Id (Main_Task);
      Component : Component_ID           := Get_Parent_Component (Main_Task);
      Template  : Unbounded_String       := U(Toolbox.Get_Template_Name (Object));
      Flags     : Integer                := Integer(The.Event.all.Header.Flags);
      Wimp_ProcessKey : constant         := 16#400dc#;
      Register  : aliased Kernel.swi_regs;
      Err       : oserror_access;
      Save      : Object_ID;
      Path      : Unbounded_String;
      Movies    : Movies_Instance_Pointer;
      Toolbar   : Boolean := false;
   begin
      if Template = U("SaveAs") then
         Movies := Get_Instance(Main.Model,Ancestor);
         Path := U(TO_Ada(The.Event.all.Filename));
      else
         Toolbar := true;
         Movies := Get_Instance(Main.Model,Object);
         Path := Movies.all.Path;
      end if;
      if Index(Path,".") > 0 or Index(Path,":") > 0 then
         CSV.Save (Path,Movies,Flags=1);
         Toolbox.Hide_Object(SaveAS_ObjectID);
         ToolboxWindow.Set_Title (Movies.all.Main_ID,S(Path));
         Movies.all.Path := Path;
         Movies.all.Modified := false;
         if Movies.all.DCS_ID /= 0 then
            Template := U(Toolbox.Get_Template_Name(Movies.all.DCS_ID));
            Toolbox.Hide_Object(Movies.all.Main_ID);
            Delete_Movies(Main.Model,Movies.all.Main_ID);                                                          
            if Template = U("DCS") then                                                                        
               Movies := Get_Changed(Main.Model);                                                                
               if Movies /= null then
                  Save := Toolbox.Create_Object("SaveAs");
                  ToolboxSaveAs.Set_FileName(Save,S(Movies.all.Path));                                           
                  Toolbox.Show_Object(Save,Movies.all.Main_ID,0,AtPointer);
               else                                                                                            
                  Set_Status(Wimp_Task_Class(Main_Task),false);                                                                 
               end if;
            end if;                                                                                            
         end if;                                                                                               
         -- continue shutdown                                                                                  
         if Shutdown then
            if Movies /= null then
               Save := Create_Object("SaveAs");                                                  
               ToolboxSaveAs.Set_FileName(Save,S(Movies.all.Path));                                              
               Toolbox.Show_Object(Save,Movies.all.Main_ID,0,AtPointer);
            else                                                                                               
               Set_Status(Wimp_Task_Class(Main.Main_Task),false);                                                               
               Register.R(0) := 508;                                                                           
               Err := Kernel.Swi (Wimp_ProcessKey, Register'Access, Register'Access);                          
               if Err /= null then                                                                             
                  OS.Raise_Error(Err);
               end if;                                                                                         
            end if;
         end if;
         ToolboxSaveAs.Save_Completed(SaveAS_ObjectID,S(Path));         
      else
         if Toolbar then
            Toolbox.Show_Object(Toolbox.Create_Object("SaveAs"),Parent,Component,AtPointer);
         end if;
      end if;
   exception
      when ex: others => Report_Error("HANDLE_SAVETOFILE",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

   procedure Handle (The : in TEL_Toolbox_SaveAs_SaveCompleted) is

      Object   : Object_ID := Get_Self_Id (Main_Task);
      Ancestor : Object_ID := Get_Ancestor_Id (Main_Task);
      Template : String    := Toolbox.Get_Template_Name (Object);
      Movies   : Movies_Instance_Pointer;
   begin
      if Template /= "SaveAs" then
         Ancestor := Object;
      end if;
      Movies := Get_Instance(Main.Model,Ancestor);
      if Movies /= null then
         if Movies.all.Path = U("") or Movies.all.Path = Untitled_String then
            ToolboxWindow.Gadget_Fade(Ancestor,16#247#);
         else
            ToolboxWindow.Gadget_UnFade(Ancestor,16#247#);
         end if;
      end if;
   exception
      when ex: others => Report_Error("HANDLE_SAVECOMPLETED",Ada.Exceptions.Exception_Information (ex));      
   end Handle;

   --
   
end Controller_SaveAs;
