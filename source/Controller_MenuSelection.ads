with RASCAL.ToolboxMenu;          use RASCAL.ToolboxMenu;
with RASCAL.OS;                   use RASCAL.OS;

package Controller_MenuSelection is

   type TEL_Toolbox_Menu_Selection        is new ATEL_Toolbox_Menu_Selection       with null record;

   --
   -- The user has selected an entry in a menu.
   --
   procedure Handle (The : in TEL_Toolbox_Menu_Selection);

end Controller_MenuSelection;
