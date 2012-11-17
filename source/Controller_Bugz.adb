with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Interfaces.C;               use Interfaces.C;
with Main;                       use Main;
with Ada.Exceptions;
with Reporter;

with RASCAL.Utility;             use RASCAL.Utility;
with RASCAL.ToolboxMenu;         use RASCAL.ToolboxMenu;
with RASCAL.Bugz;                use RASCAL.Bugz;
with RASCAL.WimpTask;            use RASCAL.WimpTask;
with RASCAL.FileExternal;
with RASCAL.Variable;
with RASCAL.UserMessages;

package body Controller_Bugz is

   --

   package Utility      renames RASCAL.Utility;
   package ToolboxMenu  renames RASCAL.ToolboxMenu; 
   package Bugz         renames RASCAL.Bugz;        
   package WimpTask     renames RASCAL.WimpTask;    
   package FileExternal renames RASCAL.FileExternal;
   package Variable     renames RASCAL.Variable;    
   package UserMessages renames RASCAL.UserMessages;
                                           
   --

   Bugz_Path : constant String := "<RiscMovieRes$Dir>.Bugz";

   --

   procedure Handle (The : in MEL_Message_Bugz_Query) is

      Query_Type : Bugz_Query_Type := The.Event.all.Query_Type;
      Msg_String : String := Bugz_Path & ASCII.NUL;
      Message    : Message_Bugz_BugzFile;
   begin
      if Query_Type = File_Request then
         RASCAL.UserMessages.Acknowledge(The.Event.all'Address);
         Message.Filename(1..Msg_String'Length) := To_C(Msg_String,false);
         RASCAL.UserMessages.SendMessage(Message    => Message'Address,
                                  Reason     => Reason_Event_UserMessage,
                                  Event_Code => Message_Event_Bugz_BugzFile,
                                  Window     => 0,
                                  Icon       => 0,
                                  Length     => (Message.Header'Size+Msg_String'Size)/8 ); -- Size in bit -> Length in byte
      end if;
   exception
      when Exception_Data : others => Report_Error("BUGZQUERY",Ada.Exceptions.Exception_Information (Exception_Data));
   end Handle;

   --

   procedure Handle (The : in TEL_CreateReport_Type) is

      Message    : Message_Bugz_BugzFile;
      Msg_String : String           := Bugz_Path & ASCII.NUL;
      Path       : Unbounded_String := U("");
      Ready      : Boolean          := false;
   begin
      Ready := Is_Task("Bugz");

      if not Ready then
         Path := U("<Bugz$Dir>.!Run");
         Ready := RASCAL.Variable.Get_Value("Bugz$Dir")'Length > 0;
      end if;
      if not Ready then
         Path := U("<Boot$Dir>.^.Apps.!Bugz");
         Ready := RASCAL.FileExternal.Exists("<Boot$Dir>.^.Apps.!Bugz.!Run");
      end if;
      if not Ready then
         Path := U("<Boot$Dir>.Resources.!Bugz");
         Ready := RASCAL.FileExternal.Exists("<Boot$Dir>.Resources.!Bugz.!Run");
      end if;
      if not Ready then
         Path := U("<Boot$Dir>.^.Utilities.!Bugz");
         Ready := RASCAL.FileExternal.Exists("<Boot$Dir>.^.Utilities.!Bugz.!Run");
      end if;
      if not Ready then
         Path := U("<Boot$Dir>.^.DevTools.!Bugz");
         Ready := RASCAL.FileExternal.Exists("<Boot$Dir>.^.DevTools.!Bugz.!Run");
      end if;
      if not Ready then
         Path := U("<Boot$Dir>.^.Internet.!Bugz");
         Ready := RASCAL.FileExternal.Exists("<Boot$Dir>.^.Internet.!Bugz.!Run");
      end if;

      if Ready and Ada.Strings.Unbounded.Length(Path) > 0 then
         Call_OS_CLI ("WimpTask " & S(Path) & " " & Bugz_Path);
      end if;

      if Ready and Ada.Strings.Unbounded.Length(Path) = 0 then
         Message.Filename(1..Msg_String'Length) := To_C(Msg_String,false);
         RASCAL.UserMessages.SendMessage(Message    => Message'Address,
                                  Reason     => Reason_Event_UserMessage,
                                  Event_Code => Message_Event_Bugz_BugzFile,
                                  Window     => 0,
                                  Icon       => 0,
                                  Length     => (Message.Header'Size+Msg_String'Size)/8 ); -- Size in bit -> Length in byte
      end if;

      if not Ready then
         Report_Error("NOBUGZ","");
      end if;
   exception
      when e: others => Report_Error("BUGZCREATE",Ada.Exceptions.Exception_Information (e));
   end Handle;

   --
   
end Controller_Bugz;
