with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with Ada.Strings;                      use Ada.Strings;
with Ada.Characters.Handling;          use Ada.Characters.Handling;
with Main;                             use Main;
with Model;                            use Model;
with Reporter;
with Ada.Exceptions;

with RASCAL.Utility;                   use RASCAL.Utility;
with RASCAL.ToolboxScrollList;         use RASCAL.ToolboxScrollList;
with RASCAL.ToolboxWindow;             use RASCAL.ToolboxWindow;
with RASCAL.ToolboxNumberRange;
with RASCAL.ToolboxStringSet;
with RASCAL.ToolboxWritableField;
with RASCAL.ToolboxTextArea;
with RASCAL.Toolbox;
with RASCAL.Time;

package body View_Main is

   --

   package Utility              renames RASCAL.Utility;
   package ToolboxScrollList    renames RASCAL.ToolboxScrollList;   
   package ToolboxWindow        renames RASCAL.ToolboxWindow;       
   package ToolboxNumberRange   renames RASCAL.ToolboxNumberRange;  
   package ToolboxStringSet     renames RASCAL.ToolboxStringSet;    
   package ToolboxWritableField renames RASCAL.ToolboxWritableField;
   package ToolboxTextArea      renames RASCAL.ToolboxTextArea;     
   package Toolbox              renames RASCAL.Toolbox;             
   package Time                 renames RASCAL.Time;

   -- Main window
   List_Gadget      : constant Component_ID := 16#0#;
   Edit_Gadget      : constant Component_ID := 16#6#;
   Remove_Gadget    : constant Component_ID := 16#2#;
                    
   -- Movie window (Add,Edit)
   Title_1_Gadget   : constant Component_ID := 16#6#;
   Title_2_Gadget   : constant Component_ID := 16#4#;
   Title_3_Gadget   : constant Component_ID := 16#2#;

   Category_Gadget  : constant Component_ID := 16#21#;
   Media_Gadget     : constant Component_ID := 16#23#;

   Language_Gadget  : constant Component_ID := 16#19#;
   Subtitles_Gadget : constant Component_ID := 16#1B#;
   Notes_Gadget     : constant Component_ID := 16#1F#;
   Element_Gadget   : constant Component_ID := 16#4e#;

   --

   procedure Update_AllViews is

      i        : Model.Position;
      Movies   : Movies_Instance_Pointer;
   begin
      if not Model.isEmpty(Main.Model) then
         i := Model.First(Main.Model);
         loop
            Movies := Model.Retrieve (Main.Model,i);
            Fill(Movies.all.Main_ID,Movies);
            exit when Model.IsLast(Main.Model,i);
            Model.GoAhead(Main.Model,i);
         end loop;
      end if;
   end Update_AllViews;
   
   --

   procedure Fill(Object   : in Object_ID;
                  Movies_P : in Movies_Instance_Pointer) is

      i                  : Movies.Position;
      Item               : Element_Pointer;
      Item_Str           : Unbounded_String;
      Header             : Unbounded_String := U("");
      ID_Counter         : Natural := 0;
      First_Column       : boolean := true;
   begin
      ToolboxScrollList.Delete_All(Movies_P.all.Main_ID,List_Gadget);
      -- create header
      for x in Columns'Range loop
         if Columns(x).Display then
            if Length(Header) > 0 then
               Append(Header,ASCII.HT);
            end if;
            Append(Header,Columns(x).Header);
         end if;
      end loop;

      ToolboxScrollList.Set_Heading (Object,List_Gadget,ID_Counter,S(Header));

      if not isEmpty(Movies_P.all.Movie_List.all) then
         i := First(Movies_P.all.Movie_List.all);
         loop
            Item := Retrieve (Movies_P.all.Movie_List.all,i);
            Item_Str := U("");
            First_Column := true;
            for x in Columns'Range loop
                if Columns(x).Display then
                   if not First_Column then
                      Append(Item_Str,ASCII.HT);
                   else
                      First_Column := false;
                   end if;
                   case Columns(x).Content is
                   when Title_1    => Append(Item_Str,Item.all.Title_1);
                   when Title_2    => Append(Item_Str,Item.all.Title_2);
                   when Title_3    => Append(Item_Str,Item.all.Title_3);
                   when Categories => Append(Item_Str,Item.all.Categories);
                   when Media      => Append(Item_Str,Item.all.Media);
                   when Languages  => Append(Item_Str,Item.all.Languages);
                   when Subtitles  => Append(Item_Str,Item.all.Subtitles);
                   when Notes      => Append(Item_Str,Item.all.Notes);
                   end case;
                end if;
            end loop;
            ToolboxScrollList.Add_Item(Object,List_Gadget,S(Item_Str),ID_Counter);
            Item.all.ID := ID_Counter;
            ID_Counter  := ID_Counter + 1;

            exit when IsLast(Movies_P.all.Movie_List.all,i);
            GoAhead(Movies_P.all.Movie_List.all,i);
         end loop;
      end if;
 
   end Fill;

   --

   procedure Add(Add_Window  : in Object_ID;
                 List_Window : in Object_ID;
                 Movies      : in Movies_Instance_Pointer) is

      Title_1  : String := ToolboxWritableField.Get_Value(Add_Window,Title_1_Gadget);
      Title_2  : String := ToolboxWritableField.Get_Value(Add_Window,Title_2_Gadget);
      Title_3  : String := ToolboxWritableField.Get_Value(Add_Window,Title_3_Gadget);

      Category : String := ToolboxWritableField.Get_Value(Add_Window,Category_Gadget);
      Media    : String := ToolboxWritableField.Get_Value(Add_Window,Media_Gadget);
                      
      Language : String := ToolboxWritableField.Get_Value(Add_Window,Language_Gadget);
      Subtitles: String := ToolboxWritableField.Get_Value(Add_Window,Subtitles_Gadget);
      Notes    : String := ToolboxTextArea.Get_Text(Add_Window,Notes_Gadget);


      Movie    : Element_Pointer := new Movie_Type'(U(Title_1),U(Title_2),U(Title_3),
                                                    U(Category),U(Media),
                                                    U(Language),U(Subtitles),U(Notes),0);

      Toolbar  : Object_ID := ToolboxWindow.Get_Toolbars(List_Window,External_Top_Left);
   begin 
      AddToRear(Movies.all.Movie_List.all,Movie);
      Fill(List_Window,Movies);
      ToolboxWindow.Gadget_Fade(List_Window,Remove_Gadget);
      ToolboxWindow.Gadget_Fade(List_Window,Edit_Gadget);
   end Add;

   --

   procedure Edit(Edit_Window : in Object_ID;
                  List_Window : in Object_ID;
                  Movies      : in Movies_Instance_Pointer;
                  Index       : in Integer := -1) is


      Item_ID : Natural;
      Element : Element_Pointer; 
   begin
      if Index <=-1 then
         Item_ID := ToolboxScrollList.Get_Selected(List_Window,List_Gadget);
      else
         Item_ID := Index;
      end if;
      Element := Get_Element(Movies.all.Movie_List.all,Item_ID);

      if Element /= null then
         ToolboxWritableField.Set_Value(Edit_Window,Title_1_Gadget,S(Element.all.Title_1));
         ToolboxWritableField.Set_Value(Edit_Window,Title_2_Gadget,S(Element.all.Title_2));
         ToolboxWritableField.Set_Value(Edit_Window,Title_3_Gadget,S(Element.all.Title_3));
                                                                          
         ToolboxWritableField.Set_Value(Edit_Window,Category_Gadget,S(Element.all.Categories));
         ToolboxWritableField.Set_Value(Edit_Window,Media_Gadget,S(Element.all.Media));
                                                                          
         ToolboxWritableField.Set_Value(Edit_Window,Language_Gadget,S(Element.all.Languages));
         ToolboxWritableField.Set_Value(Edit_Window,Subtitles_Gadget,S(Element.all.Subtitles));
         ToolboxTextArea.Set_Text(Edit_Window,Notes_Gadget,S(Element.all.Notes));

         ToolboxNumberRange.Set_Value(Edit_Window,Element_Gadget,Item_ID);
      end if;
   end Edit;

   --

   procedure Change(Edit_Window : in Object_ID;
                    List_Window : in Object_ID;
                    Movies      : in Movies_Instance_Pointer) is

      Title_1  : String := ToolboxWritableField.Get_Value(Edit_Window,Title_1_Gadget);
      Title_2  : String := ToolboxWritableField.Get_Value(Edit_Window,Title_2_Gadget);
      Title_3  : String := ToolboxWritableField.Get_Value(Edit_Window,Title_3_Gadget);

      Category : String := ToolboxWritableField.Get_Value(Edit_Window,Category_Gadget);
      Media    : String := ToolboxWritableField.Get_Value(Edit_Window,Media_Gadget);
                      
      Language : String := ToolboxWritableField.Get_Value(Edit_Window,Language_Gadget);
      Subtitles: String := ToolboxWritableField.Get_Value(Edit_Window,Subtitles_Gadget);
      Notes    : String := ToolboxTextArea.Get_Text(Edit_Window,Notes_Gadget);

      Item     : Natural := ToolboxNumberRange.Get_Value (Edit_Window,Element_Gadget);
      Element  : Element_Pointer := Get_Element (Movies.all.Movie_List.all,Item);

      Items : ItemNumber_List_Type := ToolboxScrollList.Get_SelectionNumbers(List_Window,List_Gadget);
   begin
      if Element /= null then
         Element.all.Title_1   := U(Title_1);
         Element.all.Title_2   := U(Title_2);
         Element.all.Title_3   := U(Title_3);
                                           
         Element.all.Categories:= U(Category);
         Element.all.Media     := U(Media);
                                            
         Element.all.Languages := U(Language);
         Element.all.Subtitles := U(Subtitles);
         Element.all.Notes     := U(Notes);
                                             
         Fill(List_Window,Movies);
         ToolboxScrollList.Select_Items (List_Window,List_Gadget,Items);  
      end if;                       
   end Change;

   --

   procedure Clear_Movie (Movie_Window : in Object_ID) is

   begin
      ToolboxWritableField.Set_Value(Movie_Window,Title_1_Gadget,"");
      ToolboxWritableField.Set_Value(Movie_Window,Title_2_Gadget,"");
      ToolboxWritableField.Set_Value(Movie_Window,Title_3_Gadget,"");
                                                                       
      ToolboxWritableField.Set_Value(Movie_Window,Category_Gadget,"");
      ToolboxWritableField.Set_Value(Movie_Window,Media_Gadget,"");
                                                                       
      ToolboxWritableField.Set_Value(Movie_Window,Language_Gadget,"");
      ToolboxWritableField.Set_Value(Movie_Window,Subtitles_Gadget,"");
      ToolboxTextArea.Set_Text(Movie_Window,Notes_Gadget,"");
   end Clear_Movie;

   --

end View_Main;
