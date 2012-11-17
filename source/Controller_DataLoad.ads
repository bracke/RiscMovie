with RASCAL.DragNDrop;                  use RASCAL.DragNDrop;
with RASCAL.OS;                         use RASCAL.OS;

with Model;                             use Model;

package Controller_DataLoad is

   type MEL_Message_DataLoad is new AMEL_Message_DataLoad with null record;

   --
   -- The user dragged a file onto the iconbar.
   --
   procedure Handle (The : in MEL_Message_DataLoad);


end Controller_DataLoad;
