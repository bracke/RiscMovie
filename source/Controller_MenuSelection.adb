with RASCAL.Utility;                    use RASCAL.Utility;
with RASCAL.Error;                      use RASCAL.Error;
with RASCAL.MessageTrans;               use RASCAL.MessageTrans;
with RASCAL.UserMessages;               use RASCAL.UserMessages;
with RASCAL.WimpTask;                   use RASCAL.WimpTask;
with RASCAL.Toolbox;                    use RASCAL.Toolbox;
with RASCAL.ToolboxTextArea;
with RASCAL.ToolboxWindow;
with RASCAL.ToolboxWritableField;
with RASCAL.Keyboard;

with Interfaces.C;               use Interfaces.C;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Main;                       use Main;
with Model;                      use Model;
with View_Main;
with Ada.Exceptions;
with Reporter;

package body Controller_MenuSelection is

   --

   package Utility              renames RASCAL.Utility;
   package Error                renames RASCAL.Error;               
   package MessageTrans         renames RASCAL.MessageTrans;        
   package UserMessages         renames RASCAL.UserMessages;        
   package WimpTask             renames RASCAL.WimpTask;            
   package Toolbox              renames RASCAL.Toolbox;             
   package ToolboxWindow        renames RASCAL.ToolboxWindow;       
   package ToolboxTextArea      renames RASCAL.ToolboxTextArea;     
   package ToolboxMenu          renames RASCAL.ToolboxMenu;
   package ToolboxWritableField renames RASCAL.ToolboxWritableField;
   package Keyboard             renames RASCAL.Keyboard;

   --

   Predefined_Sets : constant array (0..5) of UString
                   := (U("A-Za-z"),U("A-Za-z0-9"),U("0-9"),U("0-9A-Fa-f"),
                       U("A-Z"),U("a-z"));

   --

   procedure Handle (The : in TEL_Toolbox_Menu_Selection) is

      Movies        : Movies_Instance_Pointer;
      Target        : Component_ID := -1;
      Matched       : Boolean      := true;
      Menu          : Object_ID    := Get_Self_Id(Main_Task);
      Parent        : Object_ID    := Get_Parent_Id(Main_Task);
      Component     : Component_ID := Get_Self_Component(Main_Task);
      Ancestor      : Object_ID    := Get_Ancestor_Id(Main_Task);
      Item          : String       := ToolboxMenu.Get_Entry_Text(Menu,Component);
      Menu_Template : String       := Toolbox.Get_Template_Name(Menu);
      Current       : Unbounded_String;
      EntryText     : Boolean      := true;
      Multiple      : Boolean      := false;
   begin
      if Menu_Template = "Category" then
         Target := 16#21#; Multiple := true;
      elsif Menu_Template = "Media" then
         Target := 16#23#;
      elsif Menu_Template = "Language" then
         Target := 16#19#; Multiple := true;
      elsif Menu_Template = "Subtitles" then
         Target := 16#1b#; Multiple := true;
      end if;
      if Target /= -1 then
         Current := U(ToolboxWritableField.Get_Value(Parent,Target));
         if Multiple and Length(Current) > 0 then
            if Element(Current,Length(Current))/= ',' then
               Append(Current,",");
            end if;
         end if;
         if Multiple then
            Append(Current,Item);
         else
            Current := U(Item);
         end if;
         ToolboxWritableField.Set_Value(Parent,Target,S(Current));
      end if;
   exception
      when ex: others => Report_Error("HANDLE_MENUSELECTION",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

end Controller_MenuSelection;
