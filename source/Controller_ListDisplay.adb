with RASCAL.Error;                      use RASCAL.Error;
with RASCAL.MessageTrans;               use RASCAL.MessageTrans;
with RASCAL.Utility;                    use RASCAL.Utility;
with RASCAL.WimpTask;                   use RASCAL.WimpTask;
with RASCAL.ToolboxOptionButton;        use RASCAL.ToolboxOptionButton;
with RASCAL.ToolboxWindow;              use RASCAL.ToolboxWindow;
with RASCAL.Toolbox;
with RASCAL.Caret;
with RASCAL.ToolboxWritableField;
with RASCAL.ToolboxStringSet;
with RASCAL.ToolboxNumberRange;
with RASCAL.Pointer;
with RASCAL.Time;

with Movies;                     use Movies;
with Main;                       use Main;
with Model;                      use Model;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with View_Main;                  use View_Main;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Exceptions;
with Reporter;

package body Controller_ListDisplay is

   --

   package Error                renames RASCAL.Error;
   package MessageTrans         renames RASCAL.MessageTrans;         
   package Utility              renames RASCAL.Utility;              
   package WimpTask             renames RASCAL.WimpTask;             
   package ToolboxOptionButton  renames RASCAL.ToolboxOptionButton;  
   package ToolboxWindow        renames RASCAL.ToolboxWindow;        
   package Toolbox              renames RASCAL.Toolbox;              
   package Caret                renames RASCAL.Caret;                
   package ToolboxWritableField renames RASCAL.ToolboxWritableField; 
   package ToolboxStringSet     renames RASCAL.ToolboxStringSet;     
   package ToolboxNumberRange   renames RASCAL.ToolboxNumberRange;   
   package Pointer              renames RASCAL.Pointer;              
   package Time                 renames RASCAL.Time;
   package ToolboxScrolllist    renames RASCAL.ToolboxScrolllist;
   
   --

   List_Gadget   : constant Component_ID := 16#0#;
   Edit_Gadget   : constant Component_ID := 16#6#;
   Remove_Gadget : constant Component_ID := 16#2#;

   --

   procedure Handle (The : in TEL_RemoveItem_Type) is

      E           : Error_Pointer          := Get_Error (Wimp_Task_Class(Main_Task));
      M           : Error_Message_Pointer  := new Error_Message_Type;
      Result      : Error_Return_Type      := XButton1;

      Object      : Object_ID    := Get_Self_Id(Main_Task);
      Component   : Component_ID := Get_Self_Component(Main_Task);
      Template    : String       := Toolbox.Get_Template_Name(Object);
      Items       : ItemNumber_List_Type := Get_SelectionNumbers(Object,List_Gadget);
      Movies      : Movies_Instance_Pointer  := null;
      Name,Dummy  : Unbounded_String;
   begin
      if Items'Length > 0 then

         -- Delete entries ?
         M.all.Token(1..8) := "DELENTRY";
         M.all.Category := Warning;
         M.all.Flags    := Error_Flag_Cancel;
         Dummy := U(MessageTrans.Lookup("DELBUTTON",E.all.Msg_Handle));

         M.all.Buttons(1..Length(Dummy))  := S(Dummy);
         Result := Error.Show_Message (E,M);

         Movies := Get_Instance(Main.Model,Object);
         if Result = XButton1 and Movies /= null then
            ToolboxWindow.Gadget_Fade(Object,Remove_Gadget);
            ToolboxWindow.Gadget_Fade(Object,Edit_Gadget);
            for i in Items'Range loop
                Remove_Element(Movies.all.Movie_List.all,Items(i));
            end loop;
            View_Main.Fill(Object,Movies);

            Movies.all.Modified := true;
            ToolboxWindow.Set_Title (Movies.all.Main_ID,S(Movies.all.Path) & " *");
         end if;
      end if;
   exception
      when ex: others => Report_Error("HANDLE_REMOVEITEM",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

   procedure Handle (The : in TEL_AddItem_Type) is

      Object      : Object_ID := Get_Self_Id(Main_Task);
      Parent      : Object_ID := Get_Parent_Id(Main_Task);
      Ancestor    : Object_ID := Get_Ancestor_Id(Main_Task);
      Template    : String    := Toolbox.Get_Template_Name(Ancestor);
      Movies      : Movies_Instance_Pointer := null;
   begin
      Movies := Get_Instance(Main.Model,Ancestor);
      if Template = "Window" then
         View_Main.Add(Object,Ancestor,Movies);
      end if;
      if Movies /= null then
         Movies.all.Modified := true;
         ToolboxWindow.Set_Title (Movies.all.Main_ID,S(Movies.all.Path) & " *");
      end if;
   exception
      when ex: others => Report_Error("HANDLE_ADDITEM",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

   procedure Handle (The : in TEL_EditItem_Type) is

      Object      : Object_ID    := Get_Self_Id(Main_Task);
      Component   : Component_ID := Get_Self_Component(Main_Task);
      Item        : Integer;
      Movies      : Movies_Instance_Pointer := null;
   begin
      Item := ToolboxScrollList.Get_Selected(Object,List_Gadget);
      if Item /= -1 then
         Movies := Get_Instance(Main.Model,Object);
         View_Main.Edit(editmovie_objectid,Object,Movies);
      end if;
      Toolbox.Show_Object(editmovie_objectid,Object,Component,Toolbox.AtPointer);
   exception
      when ex: others => Report_Error("HANDLE_EDITITEM",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

   procedure Handle (The : in TEL_ChangeItem_Type) is

      Select_Button : Boolean := Pointer.Is_Select;

      Ancestor    : Object_ID := Get_Ancestor_Id(Main_Task);      
      Object      : Object_ID := Get_Self_Id(Main_Task);
      Parent      : Object_ID := Get_Parent_Id(Main_Task);
      Template    : String    := Toolbox.Get_Template_Name(Ancestor);
      Movies      : Movies_Instance_Pointer := null;
   begin
      Movies := Get_Instance(Main.Model,Ancestor);
      View_Main.Change(Object,Ancestor,Movies);
      Movies.all.Modified := true;
      ToolboxWindow.Set_Title (Movies.all.Main_ID,S(Movies.all.Path) & " *");
   exception
      when ex: others => Report_Error("HANDLE_CHANGEITEM",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --
   
   procedure Handle (The : in TEL_OpenAddItem_Type) is

      Object      : Object_ID    := Get_Self_Id(Main_Task);
      Component   : Component_ID := Get_Self_Component(Main_Task);
   begin
      View_Main.Clear_Movie (addmovie_objectid);
      Toolbox.Show_Object(addmovie_objectid,Object,Component,Toolbox.AtPointer);
   exception
      when ex: others => Report_Error("HANDLE_OADDITEM",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

   procedure Handle (The : in TEL_Toolbox_ScrollList_Selection) is

      Select_Button : Boolean := Pointer.Is_Select;
      Object        : Object_ID    := Get_Self_Id(Main_Task);
      Component     : Component_ID := Get_Self_Component(Main_Task);
      Selection     : Integer      := ToolboxScrollList.Get_Selected(Object,Component);
      New_Time      : Integer      := Time.Read_MonotonicTime;
      Movies        : Movies_Instance_Pointer := null;      
   begin
      if Selection = -1 then
         ToolboxWindow.Gadget_Fade(Object,Remove_Gadget);
         ToolboxWindow.Gadget_Fade(Object,Edit_Gadget);
      else
         ToolboxWindow.Gadget_UnFade(Object,Remove_Gadget);
         ToolboxWindow.Gadget_UnFade(Object,Edit_Gadget);
      end if;
      Caret.Set_Position (ToolboxWindow.Get_Wimp_Handle(Object));

      if Select_Button then
         -- Open edit window if double clicked with select
         if ((New_Time - LastClick_Time) < DoubleClick_Delay) and (LastClick_Time /= 0) then
            Movies := Get_Instance(Main.Model,Object);

            View_Main.Edit(editmovie_objectid,Object,Movies,The.Event.all.Item);
            Toolbox.Show_Object(editmovie_objectid,Object,Component,Toolbox.AtPointer);
         end if;
         LastClick_Time := New_Time;
      end if;   
   exception
      when ex: others => Report_Error("HANDLE_SCROLLIST",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

end Controller_ListDisplay;
