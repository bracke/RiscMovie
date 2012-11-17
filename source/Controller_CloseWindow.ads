with RASCAL.Wimp;                      use RASCAL.Wimp;
with RASCAL.Toolbox;                   use RASCAL.Toolbox;
with RASCAL.ToolboxWindow;             use RASCAL.ToolboxWindow;
with RASCAL.OS;                        use RASCAL.OS;

package Controller_CloseWindow is
   
   type WEL_Reason_Event_CloseWindow      is new AWEL_Reason_CloseWindow           with null record;
   type TEL_Toolbox_Window_HasBeenHidden  is new ATEL_Toolbox_Window_HasBeenHidden with null record;

   --
   -- This message is currently only send by AddFunction and EditFunction windows to enable us to handle the tab subwindows.
   --
   procedure Handle (The : in TEL_Toolbox_Window_HasBeenHidden);

   --
   -- The user has clicked on the 'close' icon. Does he really want to discard that mode ?
   --
   procedure Handle (The : in WEL_Reason_Event_CloseWindow);

end Controller_CloseWindow;
