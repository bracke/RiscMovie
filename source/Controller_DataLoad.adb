with RASCAL.Utility;                    use RASCAL.Utility;
with RASCAL.Error;                      use RASCAL.Error;
with RASCAL.MessageTrans;               use RASCAL.MessageTrans;
with RASCAL.WimpTask;                   use RASCAL.WimpTask;
with RASCAL.FileInternal;               use RASCAL.FileInternal;
with RASCAL.FileExternal;               use RASCAL.FileExternal;
with RASCAL.FileName;                   use RASCAL.FileName;
with RASCAL.Toolbox;                    use RASCAL.Toolbox;
with RASCAL.Memory;                     use RASCAL.Memory;
with RASCAL.Heap;                       use RASCAL.Heap;
with RASCAL.DragNDrop;                  use RASCAL.DragNDrop;
with RASCAL.UserMessages;
with RASCAL.ToolboxSlider;
with RASCAL.ToolboxDisplayField;
with RASCAL.ToolboxWindow;
with RASCAL.WimpWindow;

with Interfaces.C;               use Interfaces.C;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Maps;           use Ada.Strings.Maps;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Main;                       use Main;
with Movies;                     use Movies;
with Strings_Cutter;             use Strings_Cutter;
with CSV;                        use CSV;
with Ada.Exceptions;
with View_Main;
with Reporter;

package body Controller_DataLoad is

   --

   package Utility              renames RASCAL.Utility;
   package Error                renames RASCAL.Error;              
   package MessageTrans         renames RASCAL.MessageTrans;       
   package WimpTask             renames RASCAL.WimpTask;           
   package FileInternal         renames RASCAL.FileInternal;       
   package FileExternal         renames RASCAL.FileExternal;       
   package FileName             renames RASCAL.FileName;           
   package Toolbox              renames RASCAL.Toolbox;            
   package Memory               renames RASCAL.Memory;             
   package Heap                 renames RASCAL.Heap;               
   package DragNDrop            renames RASCAL.DragNDrop;          
   package UserMessages         renames RASCAL.UserMessages;       
   package ToolboxSlider        renames RASCAL.ToolboxSlider;      
   package ToolboxDisplayField  renames RASCAL.ToolboxDisplayField;
   package ToolboxWindow        renames RASCAL.ToolboxWindow;      
   package WimpWindow           renames RASCAL.WimpWindow;         

   --

   procedure Handle (The : in MEL_Message_DataLoad) is

      Path      : Unbounded_String := U(The.Event.all.Full_Path);
      File_Type : Integer          := The.Event.all.File_Type;
      Window    : Wimp_Handle_Type := The.Event.all.Window; 
      Icon      : Icon_Handle_Type := The.Event.all.Icon;
   begin
      if File_Type = 16#1bc# then
         The.Event.all.Header.MyRef := The.Event.all.Header.YourRef;
         UserMessages.SendMessage (The.Event.all'Address,19,Message_Event_DataLoadAck,
                                   Window,Icon,integer(The.Event.all.Header.Size));
         DragNDrop.Push_DragObject (Path,Window,Icon);

         if not Processing then
            Process_Drop;
         end if;
      else
         Report_Error ("WRONGFILETYPE","");
      end if;
   exception
      when ex : others => Report_Error("HANDLE_READERROR",
                                       Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

end Controller_DataLoad;
