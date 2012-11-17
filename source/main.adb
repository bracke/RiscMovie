with Main.Choices;              use Main.Choices;
with Controller_Quit;           use Controller_Quit;
with Controller_Resize;         use Controller_Resize;
with Controller_Internet;       use Controller_Internet;
with Controller_Bugz;           use Controller_Bugz;
with Controller_Choices;        use Controller_Choices;
with Controller_MenuSelection;  use Controller_MenuSelection;
with Controller_ListDisplay;    use Controller_ListDisplay;
with Controller_Help;           use Controller_Help;
with Controller_DCS;            use Controller_DCS;
with Controller_DataLoad;       use Controller_DataLoad;
with Controller_SaveAs;         use Controller_SaveAs;
with Controller_Dummy;          use Controller_Dummy;
with Controller_Error;          use Controller_Error;
with Controller_NewList;        use Controller_NewList;
with Controller_Find;           use Controller_Find;
with Controller_Sort;           use Controller_Sort;
with Controller_CloseWindow;    use Controller_CloseWindow;
with Controller_AboutToBeShown; use Controller_AboutToBeShown;
with Controller_Select;         use Controller_Select;
with View_Main;                 use View_Main;
with CSV;                       use CSV;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Reporter;                  use Reporter;
with Ada.Command_Line;
with Ada.Exceptions;

with RASCAL.UserMessages;       use RASCAL.UserMessages;
with RASCAL.TaskManager;        use RASCAL.TaskManager;
with RASCAL.ToolboxWindow;      use RASCAL.ToolboxWindow;
with RASCAL.ToolboxMenu;        use RASCAL.ToolboxMenu;
with RASCAL.Toolbox;            use RASCAL.Toolbox;
with RASCAL.Mode;               use RASCAL.Mode;
with RASCAL.Error;              use RASCAL.Error;
with RASCAL.MessageTrans;       use RASCAL.MessageTrans;
with RASCAL.FileExternal;
with RASCAL.DragNDrop;          use RASCAL.DragNDrop;
with RASCAL.ToolboxProgInfo;
with RASCAL.Hourglass;
with RASCAL.WimpWindow;         use RASCAL.WimpWindow;

package body Main is

   --

   package ToolboxWindow   renames RASCAL.ToolboxWindow;
   package ToolboxMenu     renames RASCAL.ToolboxMenu;    
   package Toolbox         renames RASCAL.Toolbox;        
   package Mode            renames RASCAL.Mode;           
   package Error           renames RASCAL.Error;          
   package MessageTrans    renames RASCAL.MessageTrans;   
   package FileExternal    renames RASCAL.FileExternal;   
   package DragNDrop       renames RASCAL.DragNDrop;      
   package ToolboxProgInfo renames RASCAL.ToolboxProgInfo;
   package Hourglass       renames RASCAL.Hourglass;      
   package WimpWindow      renames RASCAL.WimpWindow;     
   package WimpTask        renames RASCAL.WimpTask;
   
   --

   Main_Window_Offset : integer := 0;

   --
   
   procedure Process_Drop is

      Window    : Wimp_Handle_Type;
      Icon      : Icon_Handle_Type;
      Object    : Object_ID;
      Component : Component_ID;
      Movies    : Movies_Instance_Pointer;
      Template  : Unbounded_String;
      Path      : Unbounded_String;
   begin
      Processing := true;
      DragNDrop.Pop_DragObject(Path,Window,Icon);

      if Integer(Window) = -2 or Integer(Window) = 0 then
         Movies := new Movies_Instance_Type;
         AddToRear(Model,Movies);
         Movies.all.Main_ID := Toolbox.Create_Object("Window");
         ToolboxWindow.Gadget_UnFade(Movies.all.Main_ID,16#247#);
         CSV.Read (S(Path),Movies);
         ToolboxWindow.Set_Title (Movies.all.Main_ID,S(Path));
         Movies.all.Path := Path;
         Show_Centered(Movies.all.Main_ID);
      else
         -- Drag to existing window/list
         Window := WimpWindow.Get_AncestorWindow(Window);
         begin
            ToolboxWindow.Wimp_To_Toolbox (Window,Icon,Object,Component);
            Template := U(Toolbox.Get_Template_Name (Object));
         exception
            when others => Template := U("");
         end;
         if Template = U("Window") then
            Movies := Get_Instance(Model,Object);
            if Movies /= null then
               CSV.Read (S(Path),Movies);
               Movies.all.Modified := true;
               ToolboxWindow.Set_Title (Movies.all.Main_ID,S(Movies.all.Path) & " *");
               View_Main.Fill(Movies.all.Main_ID,Movies);
            end if;   
         end if;
      end if;
      if not Empty then
         Process_Drop;
      end if;
      Processing := false;
   end Process_Drop;

   --

   procedure Show_Centered (Window : in Object_ID) is

      xres,yres   : Integer;
      xos,yos     : Integer;
      BBox        : Toolbox_BBox_Type := ToolboxWindow.Get_Extent (Window);
      xsize,ysize : Integer;
      xpos,ypos   : integer;
      xeig,yeig   : Integer;
   begin
      xeig := Get_X_Eig_Factor;
      yeig := Get_Y_Eig_Factor;
      xres := Get_X_Resolution;
      yres := Get_Y_Resolution;
      xos  := xres * 2**xeig;
      yos  := yres * 2**yeig;
      xsize := BBox.xmax - BBox.xmin;
      ysize := BBox.ymax - BBox.ymin;
      xpos := (xos - xsize) / 2 + Main_Window_Offset * 2**xeig;
      ypos := (yos + ysize) / 2 - Main_Window_Offset * 2**yeig;
      Main_Window_Offset := Main_Window_Offset + 20;
      if Main_Window_Offset >= 100 then
         Main_Window_Offset := 0;
      end if;
      Toolbox.Show_Object_At(Window,xpos,ypos,0,0);
   end Show_Centered;

   --

   procedure Report_Error (Token : in String;
                           Info  : in String) is

      E        : Error_Pointer          := Get_Error (Wimp_Task_Class(Main_Task));
      M        : Error_Message_Pointer  := new Error_Message_Type;
      Result   : Error_Return_Type      := XButton1;
   begin
      M.all.Token(1..Token'Length) := Token;
      M.all.Param1(1..Info'Length) := Info;
      M.all.Category := Warning;
      M.all.Flags    := Error_Flag_OK;
      Result         := Error.Show_Message (E,M);
   end Report_Error;

   --

   procedure Main is

      ProgInfo_Window : Object_ID;
      Source          : Unbounded_String;
      Target          : Integer := -1;
   begin
      Read;

      -- Wimp events
      Add_Listener(Main_Task,new WEL_Reason_OpenWindow);
      Add_Listener(Main_Task,new WEL_Reason_Event_CloseWindow);
   
      -- Messages
      Add_Listener (Main_Task,new MEL_Message_DataLoad);
      Add_Listener (Main_Task,new MEL_Message_Bugz_Query);
      Add_Listener (Main_Task,new MEL_Message_PreQuit);      
      Add_Listener (Main_Task,new MEL_Message_Quit);
      Add_Listener (Main_Task,new MEL_Message_ConfiX);
   
      -- Toolbox Events
      Add_Listener (Main_Task,new TEL_Quit_Quit);
      Add_Listener (Main_Task,new TEL_Toolbox_Window_HasBeenHidden);
      Add_Listener (Main_Task,new TEL_ViewManual_Type);
      Add_Listener (Main_Task,new TEL_ViewSection_Type);
      Add_Listener (Main_Task,new TEL_ViewIHelp_Type);
      Add_Listener (Main_Task,new TEL_ViewHomePage_Type);
      Add_Listener (Main_Task,new TEL_ViewChoices_Type);
      Add_Listener (Main_Task,new TEL_SendEmail_Type);
      Add_Listener (Main_Task,new TEL_CreateReport_Type);
      Add_Listener (Main_Task,new TEL_Toolbox_Error);
      Add_Listener (Main_Task,new TEL_NewList_Selected);
      Add_Listener (Main_Task,new TEL_Toolbox_Window_AboutToBeShown);
      Add_Listener (Main_Task,new TEL_Toolbox_FileInfo_AboutToBeShown);
      Add_Listener (Main_Task,new TEL_Toolbox_SaveAs_AboutToBeShown);
      Add_Listener (Main_Task,new TEL_Toolbox_Menu_Selection);
      Add_Listener (Main_Task,new TEL_RemoveItem_Type);
      Add_Listener (Main_Task,new TEL_AddItem_Type);
      Add_Listener (Main_Task,new TEL_EditItem_Type);
      Add_Listener (Main_Task,new TEL_Toolbox_DCS_Discard);
      Add_Listener (Main_Task,new TEL_Toolbox_DCS_Save);
      Add_Listener (Main_Task,new TEL_Toolbox_DCS_Cancel);
      Add_Listener (Main_Task,new TEL_ChangeItem_Type);
      Add_Listener (Main_Task,new TEL_OpenAddItem_Type);
      Add_Listener (Main_Task,new TEL_Toolbox_SaveAs_SaveToFile);
      Add_Listener (Main_Task,new TEL_Toolbox_SaveAs_SaveCompleted);
      Add_Listener (Main_Task,new TEL_Toolbox_ScrollList_Selection);
      Add_Listener (Main_Task,new TEL_Find_Type);
      Add_Listener (Main_Task,new TEL_Sort_Type);      
      Add_Listener (Main_Task,new TEL_Select_Type);
      Add_Listener (Main_Task,new TEL_DeSelect_Type);
      Add_Listener (Main_Task,new TEL_Dummy);

      -- Is RiscMovie already running ?
      if Is_Task ("RiscMovie") then
         Target := Get_TaskHandle("RiscMovie");
      end if;

      -- Start task
      WimpTask.Set_Resources_Path(Main_Task,"<RiscMovieRes$Dir>");
      WimpTask.Initialise(Main_Task);

      if Target = -1 then
   
         Untitled_String := U(MessageTrans.Lookup("UNTITLED",
                                                  Get_Message_Block(Wimp_Task_Class(Main_Task))));
         if Length(Untitled_String) = 0 then
            Untitled_String := U("Untitled");
         end if;
      
         Progress_Window := Toolbox.Create_Object("Progress");
         ProgInfo_Window := Toolbox.Create_Object("ProgInfo");
         ToolboxProgInfo.Set_Version(ProgInfo_Window,
                                     MessageTrans.Lookup("VERS",Get_Message_Block(Main_Task)));
      
         addmovie_objectid  := Toolbox.Create_Object ("AddMovie");
         editmovie_objectid := Toolbox.Create_Object ("EditMovie");
      
         category_objectid  := Toolbox.Create_Object ("Category");
         ToolboxMenu.Set_Available (category_objectid,S(Choice_Category),Category_Max_Entries);
      
         media_objectid     := Toolbox.Create_Object ("Media");
         ToolboxMenu.Set_Available (media_objectid,S(Choice_Media),Media_Max_Entries);
      
         language_objectid  := Toolbox.Create_Object ("Language");
         ToolboxMenu.Set_Available (language_objectid,S(Choice_Language),Language_Max_Entries);
      
         subtitles_objectid := Toolbox.Create_Object ("Subtitles");
         ToolboxMenu.Set_Available (subtitles_objectid,S(Choice_Subtitles),Subtitles_Max_Entries);
      
         Sort_ObjectID      := Toolbox.Create_Object ("Sort");
         Find_ObjectID      := Toolbox.Create_Object ("Find");
         SaveAs_ObjectID    := Toolbox.Create_Object ("SaveAs");
         ProgInfo_ObjectID  := Toolbox.Create_Object ("ProgInfo");
   
         if Ada.Command_Line.Argument_Count > 0 then
            Source := U(Ada.Command_Line.Argument(1));
            if FileExternal.Exists (S(Source)) then
               if FileExternal.Get_File_Type (S(Source))= 16#1bc# then
                  DragNDrop.Push_DragObject (Source,0,0);
                  if not Processing then
                     Process_Drop;
                  end if;
               else
                  Report_Error ("WRONGFILETYPE","");
               end if;
            else
               Report_Error ("FILENOTFOUND","");
            end if;
         end if;

         WimpTask.Poll(Main_Task);
      else
         if Ada.Command_Line.Argument_Count > 0 then
            declare
               DataLoad : Message_DataLoad;
            begin
               Source := U(Ada.Command_Line.Argument(1));
               DataLoad.Full_Path(1..Length(Source)) := S(Source);
               DataLoad.File_Type := 16#1BC#;
               DataLoad.Window := -2;
               SendMessage (DataLoad'Address,17,
                            Message_Event_Code_Type(3),
                            Wimp_Handle_Type(Target));
            end;
         else
            Report_Error("TASKEXIST","");
         end if;
      end if;
   exception
      when e: others => Report_Error("UNTRAPPED",Ada.Exceptions.Exception_Information (e));
   end Main;

   --

end Main;

