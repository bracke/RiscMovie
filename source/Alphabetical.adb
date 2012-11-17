-- in modification, HM

with Text_Io;

with Ada.Strings, Ada.Strings.Maps, Ada.Strings.Fixed;
use  Ada.Strings, Ada.Strings.Maps, Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Alphabetical is
  
  Mapc: Character_Mapping;  
  function Pos (St: String) return String is
  begin
    return Translate(St,Mapc);
    -- this mapping is equivalent to To_Upper(To_Basic(St)),
    -- with exceptions for Nordic and German Characters
  end;
  
  Basic,Normal: String(1..255);
  Ch:Character; 
  
begin
  for I in 1..255 loop
    Ch:=Character'Val(I);
    Normal(I):=Ch;
    case Ch is
      when '�' => 
        Basic(I):='S';                     -- German_Sharp_S
      when '�' | '�' =>        --Scandinavian AE-ligatur
        Basic(I):=Character'Succ('Z');
      when '�' | '�' =>        --Scandinavian OE-ligatur
        Basic(I):=Character'Succ(Character'Succ('Z'));
      when '�' | '�' =>                    -- Scandinavian AA-ligatur
        Basic(I):=Character'Succ(Character'Succ(Character'Succ('Z')));
      when '�' | '�' =>
        Basic(I):='T';                     -- Icelandic_Thorn
      when '�' | '�' =>
        Basic(I):='D';                     -- Icelandic_Eth
      when others =>
        Basic(I):= To_Upper(To_Basic(Ch)); -- Default case
    end case;
  end loop;
  
  Mapc:=To_Mapping(Normal,Basic);
  
end Alphabetical;


