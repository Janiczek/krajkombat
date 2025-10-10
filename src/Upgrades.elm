module Upgrades exposing (Upgrades, all, init)


type alias Upgrades =
    { blackHatBootcamp : Bool
    }


init : Upgrades
init =
    { blackHatBootcamp = False
    }


all : Upgrades -> List ( String, Bool )
all upgrades =
    [ ( "Black Hat Bootcamp", upgrades.blackHatBootcamp )
    ]
