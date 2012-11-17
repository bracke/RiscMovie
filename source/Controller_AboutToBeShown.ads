with RASCAL.Toolbox;              use RASCAL.Toolbox;
with RASCAL.ToolboxWindow;        use RASCAL.ToolboxWindow;
with RASCAL.ToolboxSaveAs;        use RASCAL.ToolboxSaveAs;
with RASCAL.ToolboxFileInfo;      use RASCAL.ToolboxFileInfo;
with RASCAL.OS;                   use RASCAL.OS;

package Controller_AboutToBeShown is

   type TEL_Toolbox_Window_AboutToBeShown      is new ATEL_Toolbox_Window_AboutToBeShown      with null record;
   type TEL_Toolbox_SaveAs_AboutToBeShown      is new ATEL_Toolbox_SaveAs_AboutToBeShown      with null record;
   type TEL_Toolbox_FileInfo_AboutToBeShown    is new ATEL_Toolbox_FileInfo_AboutToBeShown    with null record;

   --
   -- Updates the content of the window about to be shown. It calls the apropriate View Fill method to achieve that.
   --
   procedure Handle (The : in TEL_Toolbox_Window_AboutToBeShown);

   --
   -- This handler makes sure the right filename/filepath is in the SaveAs writable field when the SaveAs dialogue is shown.
   --
   procedure Handle (The : in TEL_Toolbox_SaveAs_AboutToBeShown);

   --
   -- Update the fileinfo dialogue with the data from the 'current' mode.
   --
   procedure Handle (The : in TEL_Toolbox_FileInfo_AboutToBeShown);

private
end Controller_AboutToBeShown;
