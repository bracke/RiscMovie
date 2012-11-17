with Movies;       use Movies;
with Model;        use Model;

with RASCAL.OS;    use RASCAL.OS;

package View_Main is

   --
   -- Update all views - calls Fill for all views.
   --
   procedure Update_AllViews;

   --
   -- The user has created a new item.
   --Read the values from the Add_Window, create a new item, add it to the model and refresh the list window.
   --
   procedure Add(Add_Window  : in Object_ID;
                 List_Window : in Object_ID;
                 Movies      : in Movies_Instance_Pointer);

   --
   -- The user wants to edit an item. Find the selected item in the model and fill the values into a new edit window.
   --
   procedure Edit(Edit_Window : in Object_ID;
                  List_Window : in Object_ID;
                  Movies      : in Movies_Instance_Pointer;
                  Index       : in Integer := -1);

   --
   -- The user has changed an item. Read the new values from the edit window, update the model and refresh the list window.
   --
   procedure Change(Edit_Window : in Object_ID;
                    List_Window : in Object_ID;
                    Movies      : in Movies_Instance_Pointer);

   --
   -- Fill the window with data from the model.
   --
   procedure Fill(Object   : in Object_ID;
                  Movies_P : in Movies_Instance_Pointer);


   --
   -- Clear the window.
   --
   procedure Clear_Movie (Movie_Window : in Object_ID);

end View_Main;
