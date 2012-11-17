with Ada.Strings.Maps;           use Ada.Strings.Maps;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Main;                       use Main;
with Strings_Cutter;             use Strings_Cutter;
with Ada.Exceptions;
with Reporter;

with RASCAL.Utility;             use RASCAL.Utility;
with RASCAL.Error;               use RASCAL.Error;
with RASCAL.MessageTrans;        use RASCAL.MessageTrans;
with RASCAL.FileExternal;        use RASCAL.FileExternal;
with RASCAL.FileInternal;        use RASCAL.FileInternal;
with RASCAL.FileName;            use RASCAL.FileName;
with RASCAL.WimpTask;            use RASCAL.WimpTask;
with RASCAL.Toolbox;             use RASCAL.Toolbox;
with RASCAL.Heap;                use RASCAL.Heap;
with RASCAL.ToolboxScrollList;   use RASCAL.ToolboxScrollList;
with RASCAL.ToolboxProgInfo;
with RASCAL.ToolboxSlider;
with RASCAL.ToolboxDisplayField;
with RASCAL.Memory;

package body CSV is

   --

   package Utility             renames RASCAL.Utility;
   package Error               renames RASCAL.Error;               
   package MessageTrans        renames RASCAL.MessageTrans;        
   package FileExternal        renames RASCAL.FileExternal;        
   package FileInternal        renames RASCAL.FileInternal;        
   package FileName            renames RASCAL.FileName;            
   package WimpTask            renames RASCAL.WimpTask;            
   package Toolbox             renames RASCAL.Toolbox;             
   package Heap                renames RASCAL.Heap;                
   package ToolboxScrollList   renames RASCAL.ToolboxScrollList;   
   package ToolboxProgInfo     renames RASCAL.ToolboxProgInfo;     
   package ToolboxSlider       renames RASCAL.ToolboxSlider;       
   package ToolboxDisplayField renames RASCAL.ToolboxDisplayField; 
   package Memory              renames RASCAL.Memory;

   --

   List_Gadget   : constant Component_ID := 16#0#;
   Edit_Gadget   : constant Component_ID := 16#6#;
   Remove_Gadget : constant Component_ID := 16#2#;

   Extent   : Natural       := 0;
   TabSpace : Character_Set := To_Set("  " & ASCII.HT);
   Offset   : Integer       := 0;

   --

   function Read_Buffer (Buffer : in Heap_Block_Type) return String is

      Line : String := Memory.Get_Line (Heap.Get_Address(Buffer),Offset);
   begin
      Offset := Offset + Line'Length+1;     
      return Line;
   end Read_Buffer;

   --

   function IsEOF return Boolean is
   begin
      return not (Offset < Extent);
   end IsEOF;

   --

   procedure Read (Path   : in String;
                   Movies : in Movies_Instance_Pointer) is

      E        : Error_Pointer          := Get_Error (Wimp_Task_Class(Main_Task));
      M        : Error_Message_Pointer  := new Error_Message_Type;
      Result   : Error_Return_Type      := XButton1;
      File     : FileHandle_Type(new UString'(U(Path)),Read);
      Line     : Unbounded_String;
      Cutted   : Cut_String;
      I        : Integer;
      Tag      : Unbounded_String;
      Value    : Unbounded_String;
      Bad_Data : Boolean := false;
   begin
      Offset := 0;
      Extent := FileInternal.Get_Extent(File);
      ToolboxSlider.Set_Value(Progress_Window,0,0);
      ToolboxDisplayField.Set_Value(Progress_Window,1,MessageTrans.Lookup("PROGRESS_HEADER",Get_Message_Block(Wimp_Task_Class(Main_Task))));
      Toolbox.Show_Object(Progress_Window,0,0,Toolbox.Centre);
      WimpTask.Single_Poll(Main_Task);
      Heap.Set_Budge(true);

      declare
         File_Buffer : Heap_Block_Type(Extent+1);
      begin
         FileInternal.Load_File (Path,Heap.Get_Address(File_Buffer));
         loop
            exit when IsEOF;

            Line := Trim(U(Read_Buffer (File_Buffer)),TabSpace,TabSpace);
            if Element(Line,1) = '*' then
               I := Index (Line,":");
               if I > 1 then
                  Tag   := Trim(U(To_Lower(Ada.Strings.Unbounded.Slice (Line,1,I-1))),TabSpace,TabSpace);
                  Value := Trim(U(Ada.Strings.Unbounded.Slice (Line,I+1,Length(Line))),TabSpace,TabSpace);
                  if Tag = U("format") then
                     Movies.all.Format := Value;
                  elsif Tag = U("creator") then
                     Movies.all.Creator := Value;
                  elsif Tag = U("version") then
                     Movies.all.Version := Value;
                  end if;
               end if;
            elsif Length(Line) > 0    and then
               Element(Line,1) /= '#'     then

               if Count (Line,",") >= 7 then
                  if Count (Line,",") > 7 then
                     Bad_Data := true;
                  end if;
                  Create (Cutted, From => S(Line), Separators => ",");
                  AddToRear (Movies.all.Movie_List.all,
                             new Movie_Type'(U(Field(Cutted,1)),
                                             U(Field(Cutted,2)),
                                             U(Field(Cutted,3)),
                                             U(Field(Cutted,4)),
                                             U(Field(Cutted,5)),
                                             U(Field(Cutted,6)),
                                             U(Field(Cutted,7)),
                                             U(Field(Cutted,8)),0));
               else
                  Bad_Data := true;
               end if;
            end if;
            ToolboxSlider.Set_Value(Progress_Window,0,(Offset*100)/Extent);
            WimpTask.Single_Poll(Main_Task);
         end loop;
         Toolbox.Hide_Object(Progress_Window);
         if Bad_Data then
            M.all.Token(1..7) := "BADDATA";
            M.all.Category     := Warning;                                      
            M.all.Flags        := Error_Flag_Ok;
            Result := Error.Show_Message (E,M);
         end if;
      end;
   exception
      when ex : others => Report_Error("HANDLE_CSVREAD",Ada.Exceptions.Exception_Information (ex));
   end Read;

   --

   procedure Save (Path     : in Unbounded_String;
                   Movies_P : in out Movies_Instance_Pointer;
                   Selection: in Boolean := false) is

      E               : Error_Pointer          := Get_Error (Wimp_Task_Class(Main_Task));
      M               : Error_Message_Pointer  := new Error_Message_Type;
      Path_Full       : String                 := FileName.Convert_Path(S(Path));
      Result          : Error_Return_Type      := XButton1;
      i               : Movies.Position;
      Item            : Element_Pointer;
      Item_Str        : Unbounded_String;
      Dummy           : Unbounded_String;
      Illegal_Letters : constant String := ",";
      Legal_Letters   : constant String := S(Replace_Characters);
      Legalise        : Character_Mapping := To_Mapping(Illegal_Letters,Legal_Letters);
   begin
      if FileExternal.Exists(S(Path)) and Path /= Movies_P.all.Path then
         -- Overwrite file ?                                             
         M.all.Token(1..13)    := "OVERWRITEMODE";                       
         M.all.Category := Warning;                                      
         M.all.Flags    := Error_Flag_Cancel;                            
         Dummy := U(MessageTrans.Lookup("OVERWRITEBUTTON",E.all.Msg_Handle));
         M.all.Buttons(1..Length(Dummy))  := S(Dummy);

         Result := Error.Show_Message (E,M);
      end if;
      if Result = XButton1 then
         declare
            File : FileHandle_Type(new UString'(Path),Write);
         begin
            Put_String (File,"*Format:1.0");
            Put_String (File,"*App:RiscMovie");
            Put_String (File,"*Version:" & ToolboxProgInfo.Get_Version(ProgInfo_ObjectID));
            Put_String (File,"#");

            if not isEmpty(Movies_P.all.Movie_List.all) then 
               i     := First(Movies_P.all.Movie_List.all);

               loop

                  Item := Retrieve (Movies_P.all.Movie_List.all,i);
                  if (Selection and Is_Selected(Movies_P.all.Main_ID,List_Gadget,Item.all.ID)) or
                     not Selection then
                     
                     Item_Str := Translate (Item.all.Title_1,Legalise);
                     Append(Item_Str,',');
                     Append(Item_Str,Translate (Item.all.Title_2,Legalise));
                     Append(Item_Str,',');
                     Append(Item_Str,Translate (Item.all.Title_3,Legalise));
                     Append(Item_Str,',');
                     Append(Item_Str,Translate (Item.all.Categories,Legalise));
                     Append(Item_Str,',');
                     Append(Item_Str,Translate (Item.all.Media,Legalise));
                     Append(Item_Str,',');
                     Append(Item_Str,Translate (Item.all.Languages,Legalise));
                     Append(Item_Str,',');
                     Append(Item_Str,Translate (Item.all.Subtitles,Legalise));
                     Append(Item_Str,',');
                     Append(Item_Str,Translate (Item.all.Notes,Legalise));
                     Put_String (File,S(Item_Str));
                  end if;
                  exit when IsLast(Movies_P.all.Movie_List.all,i);
                  GoAhead(Movies_P.all.Movie_List.all,i);
               end loop;
            end if;
         end;
         FileExternal.Set_File_Type(S(Path),16#1bc#);
      end if;
   exception
      when ex: others => Report_Error("HANDLE_SAVECSV",Ada.Exceptions.Exception_Information (ex));
   end Save;

   --
    
end CSV;