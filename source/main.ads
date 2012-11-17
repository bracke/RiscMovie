with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Model;                      use Model;
with Movies;

with RASCAL.WimpTask;            use RASCAL.WimpTask;
with RASCAL.OS;                  use RASCAL.OS;
with RASCAL.Utility;             use RASCAL.Utility;
with RASCAL.Variable;            use RASCAL.Variable;
with RASCAL.Pointer;             use RASCAL.Pointer;      

package Main is

   type Content_Type is (Title_1,Title_2,Title_3,Categories,Media,Languages,Subtitles,Notes);

   type Column_Type is
   record
   Display : Boolean          := true;
   Header  : Unbounded_String := U("Title");
   Content : Content_Type     := Title_1;
   end record;

   type Table_Type is array (Positive range <>) of Column_Type;

   Processing : Boolean := false;
     
   -- Constants
   app_name       : constant String := "RiscMovie";
   Choices_Write  : constant String := Get_Value("Choices$Write") & "." & app_name;
   Choices_Read   : constant String := "Choices:" & app_name & ".Choices";

   --
   Main_Task          : ToolBox_Task_Class;
   main_objectid      : Object_ID             := -1;
   main_winid         : Wimp_Handle_Type      := -1;
   Sort_ObjectID      : Object_ID             := -1;
   Find_ObjectID      : Object_ID             := -1;
   SaveAs_ObjectID    : Object_ID             := -1;
   addmovie_objectid  : Object_ID             := -1;
   editmovie_objectid : Object_ID             := -1;
   category_objectid  : Object_ID             := -1;
   media_objectid     : Object_ID             := -1;
   language_objectid  : Object_ID             := -1;
   subtitles_objectid : Object_ID             := -1;
   ProgInfo_ObjectID  : Object_ID             := -1;
   Progress_Window    : Object_ID             := -1;
   Model              : Instances;
   Shutdown           : Boolean            := false;
   Untitled_String    : Unbounded_String;

   Replace_Characters : Unbounded_String := U("¤");

   Columns : Table_Type (1..8);
   
   Choice_Category  : Unbounded_String := U("Action,Børnefilm,Dokumentar,Drama,Eventyr,Farce," &
                                            "Historisk,Komedie,Kriminal,Krigsfilm,Kærlighed," &
                                            "Monumental,Science Fiction,Skræk,Splatter," &
                                            "Tegnefilm,Thriller,Western,Udefineret");
                                            
   Choice_Media     : Unbounded_String := U("DVD,DVD-RAM,Video-CD,Købevideo,E60,E120,E180,E210,E240,E300");
   Choice_Language  : Unbounded_String := U("Dansk,Tysk,Engelsk");
   Choice_Subtitles : Unbounded_String := U("Dansk,Tysk,Engelsk");

   Category_Max_Entries  : Natural := 0;
   Media_Max_Entries     : Natural := 0;
   Language_Max_Entries  : Natural := 0;
   Subtitles_Max_Entries : Natural := 0;

   DoubleClick_Delay     : Integer := Get_DoubleClick_Delay;
   LastClick_Time        : Integer := 0;

   --

   procedure Process_Drop;
   
   --
   
   procedure Show_Centered (Window : in Object_ID);

   --

   procedure Report_Error (Token : in String;
                           Info  : in String);

   --

   procedure Main;

   --

 end Main;


