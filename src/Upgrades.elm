module Upgrades exposing (Upgrades, init)


type alias Upgrades =
    { blackHatBootcamp : Bool
    }


init : Upgrades
init =
    { blackHatBootcamp = False
    }
