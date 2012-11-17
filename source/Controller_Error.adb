with RASCAL.Toolbox;         use RASCAL.Toolbox;
with RASCAL.OS;              use RASCAL.OS;
with RASCAL.Error;           use RASCAL.Error;
with RASCAL.MessageTrans;    use RASCAL.MessageTrans;
with RASCAL.WimpTask;        use RASCAL.WimpTask;

with Interfaces.C;           use Interfaces.C;
with Main;                   use Main;
with Ada.Exceptions;
with Reporter;

package body Controller_Error is

   --

   package Toolbox      renames RASCAL.Toolbox;
   package OS           renames RASCAL.OS;          
   package Error        renames RASCAL.Error;       
   package MessageTrans renames RASCAL.MessageTrans;
   package WimpTask     renames RASCAL.WimpTask;

   --

   procedure Handle (The : in TEL_Toolbox_Error) is

      E        : Error_Pointer          := WimpTask.Get_Error (Wimp_Task_Class(Main_Task));
      M        : Error_Message_Pointer  := new Error_Message_Type;
      Result   : Error_Return_Type      := XButton1;
      TB_Error : String := To_Ada (The.Event.all.Message);
   begin
      M.all.Message(1..TB_Error'Length) := TB_Error;
      M.all.Category := Warning;
      M.all.Flags    := Error_Flag_OK;
      Result := Error.Show_Message (E,M);
   exception
      when e2: others => Report_Error("TOOLBOXERROR",Ada.Exceptions.Exception_Information (e2));      
   end Handle;

   --
    
end Controller_Error;
