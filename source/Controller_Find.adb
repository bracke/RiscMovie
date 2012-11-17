with RASCAL.Error;                      use RASCAL.Error;
with RASCAL.MessageTrans;               use RASCAL.MessageTrans;
with RASCAL.Utility;                    use RASCAL.Utility;
with RASCAL.WimpTask;                   use RASCAL.WimpTask;
with RASCAL.ToolboxOptionButton;        use RASCAL.ToolboxOptionButton;
with RASCAL.ToolboxWindow;              use RASCAL.ToolboxWindow;
with RASCAL.Toolbox;                    use RASCAL.Toolbox;
with RASCAL.OS;                         use RASCAL.OS;
with RASCAL.ToolboxScrollList;          use RASCAL.ToolboxScrollList;
with RASCAL.ToolboxWritableField;
with RASCAL.ToolboxStringSet;
with RASCAL.ToolboxNumberRange;
with RASCAL.Caret;
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

package body Controller_Find is

   --

   package Error                   renames RASCAL.Error;
   package MessageTrans            renames RASCAL.MessageTrans;        
   package Utility                 renames RASCAL.Utility;             
   package WimpTask                renames RASCAL.WimpTask;            
   package ToolboxOptionButton     renames RASCAL.ToolboxOptionButton; 
   package ToolboxWindow           renames RASCAL.ToolboxWindow;       
   package Toolbox                 renames RASCAL.Toolbox;             
   package OS                      renames RASCAL.OS;                  
   package ToolboxScrollList       renames RASCAL.ToolboxScrollList;   
   package ToolboxWritableField    renames RASCAL.ToolboxWritableField;
   package ToolboxStringSet        renames RASCAL.ToolboxStringSet;    
   package ToolboxNumberRange      renames RASCAL.ToolboxNumberRange;  
   package Caret                   renames RASCAL.Caret;               
   package Pointer                 renames RASCAL.Pointer;             
   package Time                    renames RASCAL.Time;

   --

   List_Gadget   : constant Component_ID := 16#0#;
   Edit_Gadget   : constant Component_ID := 16#6#;
   Remove_Gadget : constant Component_ID := 16#2#;

   --

   procedure Handle (The : in TEL_Find_Type) is

      Select_Button : Boolean                 := Pointer.Is_Select;
      Object        : Object_ID               := Get_Self_Id(Main_Task);
      Parent        : Object_ID               := Get_Parent_Id(Main_Task);
      Ancestor      : Object_ID               := Get_Ancestor_Id(Main_Task);
      Search        : String                  := To_Lower(ToolboxWritableField.Get_Value (Find_ObjectID,16#0#));
      E             : Error_Pointer           := Get_Error (Wimp_Task_Class(Main_Task));
      M             : Error_Message_Pointer   := new Error_Message_Type;
      Result        : Error_Return_Type       := XButton1;
      Movies_P      : Movies_Instance_Pointer := Get_Instance(Main.Model,Ancestor);
      Title_1       : Boolean                 := ToolboxOptionButton.Get_State(Find_ObjectID,3) = On;
      Title_2       : Boolean                 := ToolboxOptionButton.Get_State(Find_ObjectID,4) = On;
      Title_3       : Boolean                 := ToolboxOptionButton.Get_State(Find_ObjectID,5) = On;
      Categories    : Boolean                 := ToolboxOptionButton.Get_State(Find_ObjectID,6) = On;
      Media         : Boolean                 := ToolboxOptionButton.Get_State(Find_ObjectID,7) = On;
      Languages     : Boolean                 := ToolboxOptionButton.Get_State(Find_ObjectID,8) = On;
      Subtitles     : Boolean                 := ToolboxOptionButton.Get_State(Find_ObjectID,9) = On;
      Notes         : Boolean                 := ToolboxOptionButton.Get_State(Find_ObjectID,16#10#) = On;
      CaseSensitive : Boolean                 := ToolboxOptionButton.Get_State(Find_ObjectID,16#11#) = On;
      last          : Boolean                 := false;
      Match         : Boolean                 := false;
      First         : Boolean                 := true;
      i             : Movies.Position;
      Item          : Element_Pointer;
   begin
      ToolboxScrolllist.DeSelect_All(Ancestor,List_Gadget);

      if Search'Length > 0 then
         if not isEmpty(Movies_P.all.Movie_List.all) then
            i := Movies.First(Movies_P.all.Movie_List.all);
            loop
               Match := False;
               Item := Retrieve (Movies_P.all.Movie_List.all,i);

               if CaseSensitive then
                  if Title_1 then
                     Match := Match or (Index(Item.all.Title_1,Search) > 0);
                  end if;
                  if Title_2 then
                     Match := Match or (Index(Item.all.Title_2,Search) > 0);
                  end if;
                  if Title_3 then
                     Match := Match or (Index(Item.all.Title_3,Search) > 0);
                  end if;
                  if Categories then
                     Match := Match or (Index(Item.all.Categories,Search) > 0);
                  end if;
                  if Media then
                     Match := Match or (Index(Item.all.Media,Search) > 0);
                  end if;
                  if Languages then
                     Match := Match or (Index(Item.all.Languages,Search) > 0);
                  end if;
                  if Subtitles then
                     Match := Match or (Index(Item.all.Subtitles,Search) > 0);
                  end if;
                  if Notes then
                     Match := Match or (Index(Item.all.Notes,Search) > 0);
                  end if;
               else
                  if Title_1 then
                     Match := Match or (Index(U(To_Lower(S(Item.all.Title_1))),Search) > 0);
                  end if;
                  if Title_2 then
                     Match := Match or (Index(U(To_Lower(S(Item.all.Title_2))),Search) > 0);
                  end if;
                  if Title_3 then
                     Match := Match or (Index(U(To_Lower(S(Item.all.Title_3))),Search) > 0);
                  end if;
                  if Categories then
                     Match := Match or (Index(U(To_Lower(S(Item.all.Categories))),Search) > 0);
                  end if;
                  if Media then
                     Match := Match or (Index(U(To_Lower(S(Item.all.Media))),Search) > 0);
                  end if;
                  if Languages then
                     Match := Match or (Index(U(To_Lower(S(Item.all.Languages))),Search) > 0);
                  end if;
                  if Subtitles then
                     Match := Match or (Index(U(To_Lower(S(Item.all.Subtitles))),Search) > 0);
                  end if;
                  if Notes then
                     Match := Match or (Index(U(To_Lower(S(Item.all.Notes))),Search) > 0);
                  end if;
               end if;
               if Match then
                  ToolboxScrollList.Select_Item(Ancestor,0,Item.all.ID);
                  if First then
                     First := false;
                     ToolboxScrollList.Make_Visible(Ancestor,0,Item.all.ID-1);
               
                     ToolboxWindow.Gadget_UnFade(Ancestor,Remove_Gadget);
                     ToolboxWindow.Gadget_UnFade(Ancestor,Edit_Gadget);
                  end if;
               end if;
               exit when IsLast(Movies_P.all.Movie_List.all,i);
               GoAhead(Movies_P.all.Movie_List.all,i);
            end loop;
         end if;            
      end if;
      if First then
         -- Search did not find anything
         M.all.Token(1..8) := "NORESULT";
         M.all.Category := Info;
         Result := Error.Show_Message (E,M);
      else
         if Select_Button then
            Toolbox.Hide_Object (Find_ObjectID);
         end if;
      end if;
   exception
      when Exception_Data : others => Report_Error("HANDLE_FIND",Ada.Exceptions.Exception_Information (Exception_Data));
   end Handle;

   --

end Controller_Find;
