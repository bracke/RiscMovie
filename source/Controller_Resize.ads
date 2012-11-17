with RASCAL.OS;                   use RASCAL.OS;
with RASCAL.Wimp;                 use RASCAL.Wimp;
with RASCAL.WimpWindow;           use RASCAL.WimpWindow;

package Controller_Resize is
   
   type WEL_Reason_OpenWindow  is new AWEL_Reason_OpenWindow   with null record;

   procedure Handle (The : in WEL_Reason_OpenWindow);

   procedure Resize (State : in out Wimp_WindowState_Type);
   
end Controller_Resize;
