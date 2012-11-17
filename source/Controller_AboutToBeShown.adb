with View_Main;                  use View_Main;
with Interfaces.C;               use Interfaces.C;
with System.Storage_Elements;    use System.Storage_Elements;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Kernel;                     use Kernel;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Main;                       use Main;
with Model;                      use Model;
with Movies;                     use Movies;
with Ada.Exceptions;
with Reporter;

with RASCAL.ToolboxScrollList;          use RASCAL.ToolboxScrollList;
with RASCAL.ToolboxWindow;              use RASCAL.ToolboxWindow;
with RASCAL.Time;                       use RASCAL.Time;
with RASCAL.Memory;                     use RASCAL.Memory;
with RASCAL.Utility;                    use RASCAL.Utility;
with RASCAL.FileExternal;               use RASCAL.FileExternal;
with RASCAL.FileName;                   use RASCAL.FileName;
with RASCAL.WimpTask;                   use RASCAL.WimpTask;
with RASCAL.Variable;
with RASCAL.FileExternal;
with RASCAL.FileName;

package body Controller_AboutToBeShown is

   --

   package ToolboxScrollList renames RASCAL.ToolboxScrollList;
   package ToolboxWindow     renames RASCAL.ToolboxWindow;    
   package Time              renames RASCAL.Time;             
   package Memory            renames RASCAL.Memory;           
   package Utility           renames RASCAL.Utility;          
   package FileExternal      renames RASCAL.FileExternal;     
   package FileName          renames RASCAL.FileName;         
   package WimpTask          renames RASCAL.WimpTask;         
   package Variable          renames RASCAL.Variable;         
   package ToolboxSaveAs     renames RASCAL.ToolboxSaveAs;
   package ToolboxFileInfo   renames RASCAL.ToolboxFileInfo;
   
   -- Main window
   List_Gadget      : constant Component_ID := 16#0#;

   -- Toolbar
   Edit_Gadget      : constant Component_ID := 16#6#;
   Remove_Gadget    : constant Component_ID := 16#2#;

   procedure Handle (The : in TEL_Toolbox_SaveAs_AboutToBeShown) is

      Ancestor      : Object_ID := Get_Ancestor_Id(Main_Task);
      Object        : Object_ID := Get_Self_Id(Main_Task);
      Template      : string    := Get_Template_Name(Object);
      FileName      : Ada.Strings.Unbounded.Unbounded_String;
      Path          : String := S(Get_Instance(Main.Model,Ancestor).all.Path);
   begin
      if Template = "SaveAs" then
         ToolboxSaveAs.Set_FileName(Object,Path);
         if ToolboxScrollList.Count_Selections (Ancestor,List_Gadget) = 0 then
            ToolboxSaveAs.Selection_Available (Object,false);
         else
            ToolboxSaveAs.Selection_Available (Object,true);
         end if;
      end if;
   exception
      when ex: others => Report_Error("HANDLE_SHOWSAVEAS",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

   procedure Handle (The : in TEL_Toolbox_Window_AboutToBeShown) is

      Object      : Object_ID    := Get_Self_Id(Main_Task);
      Parent      : Object_ID    := Get_Parent_Id(Main_Task);
      Ancestor    : Object_ID    := Get_Ancestor_Id(Main_Task);
      Component   : Component_ID := Get_Self_Component(Main_Task);
      Template    : String       := Get_Template_Name(Object);
      Selection   : Integer      := -1;
      Movies      : Movies_Instance_Pointer;
   begin
      if Template = "Window" then
         Movies := Get_Instance(Main.Model,Object);
         if Movies /= null then
            View_Main.Fill(Object,Movies);
   
            Selection := ToolboxScrollList.Get_Selected(Object,List_Gadget);
            if Selection = -1 then
               ToolboxWindow.Gadget_Fade(Object,Remove_Gadget);
               ToolboxWindow.Gadget_Fade(Object,Edit_Gadget);
            else
               ToolboxWindow.Gadget_UnFade(Object,Remove_Gadget);
               ToolboxWindow.Gadget_UnFade(Object,Edit_Gadget);
            end if;
            if Movies.all.Path = U("") or Movies.all.Path = Untitled_String then
               ToolboxWindow.Gadget_Fade(Object,16#247#);
            else
               ToolboxWindow.Gadget_UnFade(Object,16#247#);
            end if;
         end if;
      end if;
   exception
      when ex: others => Report_Error("HANDLE_SHOWWINDOW",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

   procedure Handle (The : in TEL_Toolbox_FileInfo_AboutToBeShown) is

      Object        : Object_ID    := Get_Self_Id(Main_Task);
      Ancestor      : Object_ID    := Get_Ancestor_Id(Main_Task);
      Movies        : Movies_Instance_Pointer := Get_Instance(Main.Model,Ancestor);
      UTC           : UTC_Pointer;
   begin
      ToolboxFileInfo.Set_FileName(Object,S(Movies.all.Path));
      ToolboxFileinfo.Set_Modified(Object,Movies.all.Modified);
      if Ada.Strings.Unbounded.Length(Movies.all.Path) > 0 and then FileExternal.Exists(S(Movies.all.Path)) then
         ToolboxFileinfo.Set_FileSize(Object,FileExternal.Get_Size(S(Movies.all.Path)));
         UTC := FileExternal.Get_Stamp(S(Movies.all.Path));
         if UTC /= null then
            ToolboxFileinfo.Set_Date(Object,UTC.all);
         end if;
      end if;
   exception
      when ex: others => Report_Error("HANDLE_SHOWFILEINFO",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --
    
end Controller_AboutToBeShown;
