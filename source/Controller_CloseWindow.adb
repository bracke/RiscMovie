with RASCAL.Utility;                    use RASCAL.Utility;
with RASCAL.Error;                      use RASCAL.Error;
with RASCAL.MessageTrans;               use RASCAL.MessageTrans;
with RASCAL.UserMessages;               use RASCAL.UserMessages;
with RASCAL.WimpTask;                   use RASCAL.WimpTask;
with RASCAL.FileExternal;               use RASCAL.FileExternal;
with RASCAL.Caret;
with RASCAL.ToolboxWindow;
with RASCAL.ToolboxWritableField;
with RASCAL.FileName;
with RASCAL.Pointer;
with RASCAL.Keyboard;

with Interfaces.C;                      use Interfaces.C;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Main;                              use Main;
with Model;                             use Model;
with Movies;                            use Movies;
with Ada.Exceptions;
with Reporter;

package body Controller_CloseWindow is

   --

   package Utility                renames RASCAL.Utility;
   package Error                  renames RASCAL.Error;                
   package MessageTrans           renames RASCAL.MessageTrans;         
   package UserMessages           renames RASCAL.UserMessages;         
   package WimpTask               renames RASCAL.WimpTask;             
   package FileExternal           renames RASCAL.FileExternal;         
   package Caret                  renames RASCAL.Caret;                
   package ToolboxWindow          renames RASCAL.ToolboxWindow;        
   package ToolboxWritableField   renames RASCAL.ToolboxWritableField; 
   package FileName               renames RASCAL.FileName;             
   package Pointer                renames RASCAL.Pointer;              
   package Keyboard               renames RASCAL.Keyboard;
   package Toolbox                renames RASCAL.Toolbox;
   
   --

   procedure Handle (The : in WEL_Reason_Event_CloseWindow) is

      Window    : Object_ID;
      Component : Component_ID;
      Movies      : Movies_Instance_Pointer;
   begin
      ToolboxWindow.Wimp_To_Toolbox(The.Event.all.Window,0,Window,Component);
     
      if Get_Template_Name(Window) = "Window" then
         Movies := Get_Instance(Main.Model,Window);
         if Pointer.Is_Adjust then
            case FileExternal.Get_Object_Type(S(Movies.all.Path)) is
            when File_Object => Utility.Call_OS_CLI("Filer_OpenDir " & FileName.Get_Path(S(Movies.all.Path)));
            when Dir|Image|Not_Found   => null;
            end case;
         end if;

         if not Keyboard.Is_Shift then
            if Movies.all.Modified then
               Movies.all.DCS_ID := Create_Object("DCSFile");
               Toolbox.Show_Object(Movies.all.DCS_ID,0,0,AtPointer);
            else
               Delete_Movies(Main.Model,Window);
            end if;
         end if;
      else
         Toolbox.Hide_Object(Window);
      end if;
   exception
      when ex: others => Report_Error("HANDLE_CLOSEWINDOW",Ada.Exceptions.Exception_Information (ex));
   end Handle;
   
   --

   procedure Handle (The : in TEL_Toolbox_Window_HasBeenHidden) is

      Object   : Object_ID    := Get_Self_Id(Main_Task);
      Ancestor : Object_ID    := Get_Ancestor_Id(Main_Task);
      Template : String       := Toolbox.Get_Template_Name(Object);
      Target   : Wimp_Handle_Type;
      Source   : Wimp_Handle_Type;
      Current  : Wimp_Handle_Type;
      Icon     : Icon_Handle_Type;
      X_Off,Y_Off,Index,Flags : Integer;
   begin
      if Template = "AddMovie" or Template = "EditMovie" then
         Target := ToolboxWindow.Get_Wimp_Handle(Ancestor);
         Source := ToolboxWindow.Get_Wimp_Handle(Object);            
         Caret.Get_Position (Current,Icon,X_Off,Y_Off,Flags,Index);
         if (Current = Source) or (Current = -1) then
            Caret.Set_Position (Target);
         end if;
      end if;
   exception
      when ex: others => Report_Error("HANDLE_HIDEWINDOW",Ada.Exceptions.Exception_Information (ex));      
   end Handle;

   --
        
end Controller_CloseWindow;
