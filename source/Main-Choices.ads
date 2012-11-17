with RASCAL.Utility;         use RASCAL.Utility;
with RASCAL.Wimp;              use RASCAL.Wimp;
with RASCAL.Choices;           use RASCAL.Choices;

package Main.Choices is

   --
   -- Reads all preferences into global variables.
   --
   procedure Read;
   
   Choice      : Choices_Type (new UString'(U("RiscMovie")));
   
end Main.Choices;