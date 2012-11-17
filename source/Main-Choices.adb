with RASCAL.OS;              use RASCAL.OS;
with RASCAL.MessageTrans;    use RASCAL.MessageTrans;
with RASCAL.ToolboxWindow;
with RASCAL.FileExternal;

with Ada.Exceptions;
with Reporter;

package body Main.Choices is

   --

   package Utility       renames RASCAL.Utility;
   package OS            renames RASCAL.OS;           
   package MessageTrans  renames RASCAL.MessageTrans; 
   package ToolboxWindow renames RASCAL.ToolboxWindow;
   package FileExternal  renames RASCAL.FileExternal;

   --

   procedure Read is

      Value: Unbounded_String;
      File : Messages_Handle_Type;
      Dummy: Integer;
   begin
      if FileExternal.Exists(Get_Read_Path(Choice) & "Choices") then
         File := MessageTrans.Open_File(Get_Read_Path(Choice) & "Choices");

         begin
            Read_String ("CATEGORY",Choice_Category,File);
            Read_String ("MEDIA",Choice_Media,File);
            Read_String ("LANGUAGE",Choice_Language,File);
            Read_String ("SUBTITLES",Choice_Subtitles,File);
         exception
            when others => null;            
         end;

         -- Columns
         for i in Columns'Range loop
            begin
               Read_Boolean ("COLUMN" & intstr(i),Columns(i).Display,File);
               Read_String ("HEADING" & intstr(i),Columns(i).Header,File);
               Read_Integer ("CONTENT" & intstr(i),Dummy,File);
               Columns(i).Content := Content_Type'Val(Dummy);
            exception
               when others => null;
            end; 
         end loop;
      end if;
   exception
      when ex : others => Report_Error("NOCHOICES",Ada.Exceptions.Exception_Information (ex));
   end Read;

end Main.Choices;
