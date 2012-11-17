with RASCAL.ToolboxSaveAs;     use RASCAL.ToolboxSaveAs;
with RASCAL.Utility;           use RASCAL.Utility;
with RASCAL.UserMessages;      use RASCAL.UserMessages;
with RASCAL.WimpTask;          use RASCAL.WimpTask;

with Kernel;                   use Kernel;
with Interfaces.C;             use Interfaces.C;
with Main;                     use Main;
with Model;                    use Model;
with Ada.Exceptions;
with Reporter;

package body Controller_DCS is

   --

   package ToolboxSaveAs renames RASCAL.ToolboxSaveAs;
   package Utility       renames RASCAL.Utility;      
   package UserMessages  renames RASCAL.UserMessages; 
   package WimpTask      renames RASCAL.WimpTask;     
                                             
   --

   procedure Handle (The : in TEL_Toolbox_DCS_Discard) is

      Wimp_ProcessKey : constant  := 16#400dc#;
      Object          : Object_ID := Get_Self_Id(Main_Task);
      Template        : String    := Get_Template_Name(Object);
      Movies            : Movies_Instance_Pointer := null;
      Register        : aliased Kernel.swi_regs;
      Error           : oserror_access;
   begin
      if Template = "DCS" then
         Set_Status(Wimp_Task_Class(Main.Main_Task),false);
         if Shutdown then
            Register.R(0) := 508;
            Error := Kernel.Swi (Wimp_ProcessKey, Register'Access, Register'Access);
            if Error /= null then
               pragma Debug(Reporter.Report("Controller-DCS: Wimp_ProcessKey" & To_Ada(Error.ErrMess)));
               Raise_Error(Error);
            end if;
         end if;
      else
         Movies := Get_Closed(Main.Model,Object);
         Movies.all.Modified := false;
         Movies.all.DCS_ID := 0;
         Delete_Movies(Main.Model,Movies.all.Main_ID);
      end if;
   exception
      when ex: others => Report_Error("HANDLE_DCSDISCARD",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

   procedure Handle (The : in TEL_Toolbox_DCS_Save) is

      Wimp_ProcessKey : constant  := 16#400dc#;
      Object          : Object_ID := Get_Self_Id(Main_Task);
      Template        : String    := Get_Template_Name(Object);
      Movies            : Movies_Instance_Pointer := null;
      Save            : Object_ID;
      Register        : aliased Kernel.swi_regs;
      Error           : oserror_access;
   begin
      if Template = "DCSFile" then
         Movies := Get_Closed(Main.Model,Object);
         Save := Create_Object("SaveAs");
         ToolboxSaveAs.Set_FileName(Save,S(Movies.all.Path));
         Show_Object(Save,Movies.all.Main_ID,0,AtPointer);
      elsif Template = "DCS" then
         Movies := Get_Changed(Main.Model);
         if Movies /= null then
            Save := Create_Object("SaveAs");
            ToolboxSaveAs.Set_FileName(Save,S(Movies.all.Path));
            Show_Object(Save,Movies.all.Main_ID,0,AtPointer);
         else
            Set_Status(Wimp_Task_Class(Main.Main_Task),false);
            if Shutdown then
               Register.R(0) := 508;
               Error := Kernel.Swi (Wimp_ProcessKey, Register'Access, Register'Access);
               if Error /= null then
                  pragma Debug(Reporter.Report("Controller-DCS: Wimp_ProcessKey" & To_Ada(Error.ErrMess)));
                  Raise_Error(Error);
               end if;
            end if;   
         end if;
      end if;
   exception
      when ex: others => Report_Error("HANDLE_DCSSAVE",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

   procedure Handle (The : in TEL_Toolbox_DCS_Cancel) is

      Object        : Object_ID := Get_Self_Id(Main_Task);
      Template      : String    := Get_Template_Name(Object);
      Movies          : Movies_Instance_Pointer;
   begin
      if Template = "DCSFile" then        
         Movies := Get_Closed(Main.Model,Object);
         Movies.all.DCS_ID := 0;
         Shutdown := false;
      end if;
   exception
      when ex: others => Report_Error("HANDLE_DCSCANCEL",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

end Controller_DCS;