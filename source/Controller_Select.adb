with RASCAL.Error;                      use RASCAL.Error;
with RASCAL.MessageTrans;               use RASCAL.MessageTrans;
with RASCAL.Utility;                    use RASCAL.Utility;
with RASCAL.WimpTask;                   use RASCAL.WimpTask;
with RASCAL.ToolboxOptionButton;        use RASCAL.ToolboxOptionButton;
with RASCAL.ToolboxWindow;              use RASCAL.ToolboxWindow;
with RASCAL.Toolbox;                    use RASCAL.Toolbox;
with RASCAL.OS;                         use RASCAL.OS;
with RASCAL.ToolboxScrollList;          use RASCAL.ToolboxScrollList;
with RASCAL.Caret;
with RASCAL.ToolboxWritableField;
with RASCAL.ToolboxStringSet;
with RASCAL.ToolboxNumberRange;
with RASCAL.Pointer;
with RASCAL.Time;

with Movies;                     use Movies;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with View_Main;                  use View_Main;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Main;                       use Main;
with Model;                      use Model;
with Ada.Exceptions;
with Reporter;

package body Controller_Select is

   --

   package Error                renames RASCAL.Error;
   package MessageTrans         renames RASCAL.MessageTrans;         
   package Utility              renames RASCAL.Utility;              
   package WimpTask             renames RASCAL.WimpTask;             
   package ToolboxOptionButton  renames RASCAL.ToolboxOptionButton;  
   package ToolboxWindow        renames RASCAL.ToolboxWindow;        
   package Toolbox              renames RASCAL.Toolbox;              
   package OS                   renames RASCAL.OS;                   
   package ToolboxScrollList    renames RASCAL.ToolboxScrollList;    
   package Caret                renames RASCAL.Caret;                
   package ToolboxWritableField renames RASCAL.ToolboxWritableField; 
   package ToolboxStringSet     renames RASCAL.ToolboxStringSet;     
   package ToolboxNumberRange   renames RASCAL.ToolboxNumberRange;   
   package Pointer              renames RASCAL.Pointer;              
   package Time                 renames RASCAL.Time;                 

   --

   List_Gadget   : constant Component_ID := 16#0#;
   Edit_Gadget   : constant Component_ID := 16#6#;
   Remove_Gadget : constant Component_ID := 16#2#;

   --
   
   procedure Handle(The : in TEL_Select_Type) is

      Object : Object_ID := Get_Self_Id(Main_Task);
      Selection : Integer;
   begin
      ToolboxScrollList.Select_All (Object,List_Gadget,0);
      Selection := ToolboxScrollList.Get_Selected(Object,List_Gadget);
      if Selection /= -1 then
         ToolboxWindow.Gadget_UnFade(Object,Remove_Gadget);
         ToolboxWindow.Gadget_UnFade(Object,Edit_Gadget);
      end if;
   end Handle;

   --

   procedure Handle(The : in TEL_DeSelect_Type) is

      Object : Object_ID := Get_Self_Id(Main_Task);
   begin
      ToolboxScrollList.DeSelect_All (Object,List_Gadget);
      ToolboxWindow.Gadget_Fade(Object,Remove_Gadget);
      ToolboxWindow.Gadget_Fade(Object,Edit_Gadget);
   end Handle;

   --

end Controller_Select;
