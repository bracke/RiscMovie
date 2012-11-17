--
-- Taken from : http://www.adapower.com/reuse/alphcon.html
-- Author     : Hans Marqvardsen
-- License    : Public Domain
--
--
-- Modified by Bent Bracke to NOT change öÖ and äÄ behaviour
-- as it is correct for German.
--


package Alphabetical is
  function Pos (St: String) return String;
    -- Except for one German and some Nordic characters 
    -- Alphabetical.Pos returns To_Upper(To_Basic(St)); 
  
    -- The Exceptions are:
    -- German_Sharp_S:            ß     => S                     
    -- Icelandic_Eth:             Ðð    => D
    -- Icelandic_Thorn:           þÞ    => T
    -- Scandinavian AE-ligatur:   Ææ    => [ (Next after Z)
    -- Scandinavian OE-ligatur:   Øø    => \ (2 after Z)
    -- Scandinavian AA-ligatur:   Åå    => ] (3 after Z)
end Alphabetical;

