with RASCAL.Toolbox;              use RASCAL.Toolbox;
with RASCAL.ToolboxDCS;           use RASCAL.ToolboxDCS;
with RASCAL.OS;                   use RASCAL.OS;

package Controller_DCS is

   type TEL_Toolbox_DCS_Discard  is new ATEL_Toolbox_DCS_Discard                with null record;
   type TEL_Toolbox_DCS_Save     is new ATEL_Toolbox_DCS_Save                   with null record;
   type TEL_Toolbox_DCS_Cancel   is new ATEL_Toolbox_DCS_Cancel                 with null record;

   --
   -- The user wants to discard the modifications.
   --
   procedure Handle (The : in TEL_Toolbox_DCS_Discard);

   --
   -- The user wants to save the modified modefile.
   --
   procedure Handle (The : in TEL_Toolbox_DCS_Save);

   --
   -- The user wants to abandon the quit process.
   --
   procedure Handle (The : in TEL_Toolbox_DCS_Cancel);
   
private
end Controller_DCS;