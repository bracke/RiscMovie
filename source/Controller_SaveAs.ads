with RASCAL.ToolboxSaveAs; use RASCAL.ToolboxSaveAs;

package Controller_SaveAs is

   type TEL_Toolbox_SaveAs_SaveToFile    is new ATEL_Toolbox_SaveAs_SaveToFile    with null record;
   type TEL_Toolbox_SaveAs_SaveCompleted is new ATEL_Toolbox_SaveAs_SaveCompleted with null record;

   --
   -- The user has completed the Save dialogue.
   --
   procedure Handle (The : in TEL_Toolbox_SaveAs_SaveToFile);

   --
   -- The Save is successfully completed.
   --
   procedure Handle (The : in TEL_Toolbox_SaveAs_SaveCompleted);
    
end Controller_SaveAs;
