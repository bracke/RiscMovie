with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Movies;                     use Movies;
with Model;                      use Model;

with RASCAL.OS;                  use RASCAL.OS;

package CSV is

   --
   -- Reads a CSV file and inserts information into movie list.
   --
   procedure Read (Path   : in String;
                   Movies : in Movies_Instance_Pointer);

   --
   -- Save movie list as CSV file.
   --
   procedure Save (Path     : in Unbounded_String;
                   Movies_P : in out Movies_Instance_Pointer;
                   Selection: in Boolean := false);
                                           
private
end CSV;