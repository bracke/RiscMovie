with RASCAL.Toolbox;              use RASCAL.Toolbox;
with RASCAL.ToolboxWindow;        use RASCAL.ToolboxWindow;
with RASCAL.ToolboxMenu;          use RASCAL.ToolboxMenu;
with RASCAL.OS;                   use RASCAL.OS;

package Controller_NewList is

   type TEL_NewList_Selected    is new Toolbox_UserEventListener(16#14#,-1,-1) with null record;

   --
   -- The user want to create a new list - allocate data structures and open the main window.
   --
   procedure Handle (The : in TEL_NewList_Selected);

end Controller_NewList;