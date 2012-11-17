--
-- Taken from : http://www.adapower.com/reuse/alphcon.html
-- Author     : Hans Marqvardsen
-- License    : Public Domain
--
--
-- Modified by Bent Bracke to NOT change �� and �� behaviour
-- as it is correct for German.
--


package Alphabetical is
  function Pos (St: String) return String;
    -- Except for one German and some Nordic characters 
    -- Alphabetical.Pos returns To_Upper(To_Basic(St)); 
  
    -- The Exceptions are:
    -- German_Sharp_S:            �     => S                     
    -- Icelandic_Eth:             ��    => D
    -- Icelandic_Thorn:           ��    => T
    -- Scandinavian AE-ligatur:   ��    => [ (Next after Z)
    -- Scandinavian OE-ligatur:   ��    => \ (2 after Z)
    -- Scandinavian AA-ligatur:   ��    => ] (3 after Z)
end Alphabetical;

