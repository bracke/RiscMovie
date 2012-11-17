with RASCAL.Error;                      use RASCAL.Error;
with RASCAL.MessageTrans;               use RASCAL.MessageTrans;
with RASCAL.Utility;                    use RASCAL.Utility;
with RASCAL.WimpTask;                   use RASCAL.WimpTask;
with RASCAL.ToolboxOptionButton;        use RASCAL.ToolboxOptionButton;
with RASCAL.ToolboxWindow;              use RASCAL.ToolboxWindow;
with RASCAL.Toolbox;                    use RASCAL.Toolbox;
with RASCAL.OS;                         use RASCAL.OS;
with RASCAL.ToolboxScrollList;          use RASCAL.ToolboxScrollList;
with RASCAL.ToolboxRadioButton;         use RASCAL.ToolboxRadioButton;
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
with Alphabetical;
with Ada.Exceptions;
with Reporter;

package body Controller_Sort is

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
   package ToolboxRadioButton   renames RASCAL.ToolboxRadioButton;  
   package Caret                renames RASCAL.Caret;               
   package ToolboxWritableField renames RASCAL.ToolboxWritableField;
   package ToolboxStringSet     renames RASCAL.ToolboxStringSet;    
   package ToolboxNumberRange   renames RASCAL.ToolboxNumberRange;  
   package Pointer              renames RASCAL.Pointer;             
   package Time                 renames RASCAL.Time;

   --

   procedure Handle (The : in TEL_Sort_Type) is

      Object        : Object_ID               := Get_Self_Id(Main_Task);
      Ancestor      : Object_ID               := Get_Ancestor_Id(Main_Task);
      Template      : String                  := Toolbox.Get_Template_Name(Object);
      Movies_P      : Movies_Instance_Pointer;
      i,i2          : Movies.Position;
      ItemI,ItemI2  : Element_Pointer;
      Changed       : Boolean := false;
      ItemI_Str     : Unbounded_String;
      ItemI2_Str    : Unbounded_String;
      Sort_Column   : Content_Type := Content_Type'Val(ToolboxStringSet.Get_Selected_Index(Sort_ObjectID,16#e#));
      Reverse_Direction : Boolean  := ToolboxRadioButton.Get_State(Sort_ObjectID,16#10#) = UnSelected;

      procedure SiftDown (x_in : in Movies.Position;
                          y_in : in Movies.Position) is

         x         : Movies.Position := x_in;
         y         : Movies.Position := y_in;
         ItemX     : Element_Pointer;
         ItemY     : Element_Pointer;
         ItemX_Str : Unbounded_String;
         ItemY_Str : Unbounded_String;
      begin
         loop
            exit when Movies.IsFirst (Movies_P.all.Movie_List.all,x);
            GoBack (Movies_P.all.Movie_List.all,x);
            ItemX := Retrieve (Movies_P.all.Movie_List.all,x);
            case Sort_Column is
            when Title_1    => ItemX_Str := ItemX.all.Title_1;
            when Title_2    => ItemX_Str := ItemX.all.Title_2;
            when Title_3    => ItemX_Str := ItemX.all.Title_3;
            when Categories => ItemX_Str := ItemX.all.Categories;
            when Media      => ItemX_Str := ItemX.all.Media;
            when Languages  => ItemX_Str := ItemX.all.Languages;
            when Subtitles  => ItemX_Str := ItemX.all.Subtitles;
            when Notes      => ItemX_Str := ItemX.all.Notes;
            end case;

            ItemY := Retrieve (Movies_P.all.Movie_List.all,y);
            case Sort_Column is
            when Title_1    => ItemY_Str := ItemY.all.Title_1;
            when Title_2    => ItemY_Str := ItemY.all.Title_2;
            when Title_3    => ItemY_Str := ItemY.all.Title_3;
            when Categories => ItemY_Str := ItemY.all.Categories;
            when Media      => ItemY_Str := ItemY.all.Media;
            when Languages  => ItemY_Str := ItemY.all.Languages;
            when Subtitles  => ItemY_Str := ItemY.all.Subtitles;
            when Notes      => ItemY_Str := ItemY.all.Notes;
            end case;

            exit when ItemX_Str = ItemY_Str;
            exit when (Alphabetical.Pos(S(ItemX_Str)) < Alphabetical.Pos(S(ItemY_Str))) and (not Reverse_Direction);
            exit when (Alphabetical.Pos(S(ItemX_Str)) > Alphabetical.Pos(S(ItemY_Str))) and Reverse_Direction;

            Movies.Swap (Movies_P.all.Movie_List.all,x,y);
            y := x;
         end loop;
      end SiftDown;

   begin
      if Template = "Window" then
         Ancestor := Object;
      end if;
      Movies_P  := Get_Instance(Main.Model,Ancestor);

      if not isEmpty(Movies_P.all.Movie_List.all) then
         i := Movies.First(Movies_P.all.Movie_List.all);
         loop
            i2 := i;
            loop
               exit when IsLast(Movies_P.all.Movie_List.all,i2);
               GoAhead(Movies_P.all.Movie_List.all,i2);

               ItemI := Retrieve (Movies_P.all.Movie_List.all,I);
               case Sort_Column is
               when Title_1    => ItemI_Str := ItemI.all.Title_1;
               when Title_2    => ItemI_Str := ItemI.all.Title_2;
               when Title_3    => ItemI_Str := ItemI.all.Title_3;
               when Categories => ItemI_Str := ItemI.all.Categories;
               when Media      => ItemI_Str := ItemI.all.Media;
               when Languages  => ItemI_Str := ItemI.all.Languages;
               when Subtitles  => ItemI_Str := ItemI.all.Subtitles;
               when Notes      => ItemI_Str := ItemI.all.Notes;
               end case;
   
               ItemI2 := Retrieve (Movies_P.all.Movie_List.all,I2);
               case Sort_Column is
               when Title_1    => ItemI2_Str := ItemI2.all.Title_1;
               when Title_2    => ItemI2_Str := ItemI2.all.Title_2;
               when Title_3    => ItemI2_Str := ItemI2.all.Title_3;
               when Categories => ItemI2_Str := ItemI2.all.Categories;
               when Media      => ItemI2_Str := ItemI2.all.Media;
               when Languages  => ItemI2_Str := ItemI2.all.Languages;
               when Subtitles  => ItemI2_Str := ItemI2.all.Subtitles;
               when Notes      => ItemI2_Str := ItemI2.all.Notes;
               end case;

               if ((Alphabetical.Pos(S(ItemI_Str)) > Alphabetical.Pos(S(ItemI2_Str))) and (not Reverse_Direction)) or
                  ((Alphabetical.Pos(S(ItemI_Str)) < Alphabetical.Pos(S(ItemI2_Str))) and Reverse_Direction) then

                  Changed := true;
                  Movies.Swap (Movies_P.all.Movie_List.all,i,i2);
                  SiftDown (i,i);
               end if;
               exit when IsLast(Movies_P.all.Movie_List.all,i2);
            end loop;
            exit when IsLast(Movies_P.all.Movie_List.all,i);
            GoAhead(Movies_P.all.Movie_List.all,i);
         end loop;
         if Changed then
            View_Main.Fill(Ancestor,Movies_P);
            Movies_P.all.Modified := true;
            ToolboxWindow.Set_Title (Movies_P.all.Main_ID,S(Movies_P.all.Path) & " *");
         end if;
      end if;            
   exception
      when Exception_Data : others => Report_Error("HAND_SORT",Ada.Exceptions.Exception_Information (Exception_Data));
   end Handle;

   --
    
end Controller_Sort;
