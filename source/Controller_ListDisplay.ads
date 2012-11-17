with RASCAL.ToolboxScrolllist;    use RASCAL.ToolboxScrolllist;
with RASCAL.OS;                   use RASCAL.OS;

package Controller_ListDisplay is

   -- User defined toolbox event
   type TEL_RemoveItem_Type                is new Toolbox_UserEventListener(16#50#,-1,-1) with null record;
   type TEL_AddItem_Type                   is new Toolbox_UserEventListener(16#47#,-1,-1) with null record;
   type TEL_ChangeItem_Type                is new Toolbox_UserEventListener(16#44#,-1,-1) with null record;
   type TEL_EditItem_Type                  is new Toolbox_UserEventListener(16#46#,-1,-1) with null record;
   type TEL_OpenAddItem_Type               is new Toolbox_UserEventListener(16#51#,-1,-1) with null record;
   type TEL_Toolbox_ScrollList_Selection   is new ATEL_Toolbox_ScrollList_Selection       with null record;

   --
   -- The user has clicked on a 'remove' button - remove the selected item from the scrolllist.
   --
   procedure Handle (The : in TEL_RemoveItem_Type);
   
   --
   -- The user has clicked on a 'add' button - open the apropriate add dialogue window.
   --
   procedure Handle (The : in TEL_AddItem_Type);

   --
   -- The user has clicked upon a 'edit' button - open the selected item in an edit dialogue window.
   --
   procedure Handle (The : in TEL_EditItem_Type);

   --
   -- The user has edited an item - update the model accordingly.
   --
   procedure Handle (The : in TEL_ChangeItem_Type);

   --
   -- Wipe the contents of add dialogue windows before showing them.
   --
   procedure Handle (The : in TEL_OpenAddItem_Type);

   --
   -- The user has selected/de-selected something in a scrolllist - update 'remove' and 'edit' buttons accordingly.
   --
   procedure Handle (The : in TEL_Toolbox_ScrollList_Selection);

private
end Controller_ListDisplay;