module Region exposing
    ( Region
    , generator
    , listGenerator
    , youName
    )

import Random exposing (Generator)
import Random.Extra
import Random.List
import Stats exposing (Stats)
import Upgrades exposing (Upgrades)


type alias Region =
    { name : String
    , stats : Stats
    , upgrades : Upgrades
    }


youName : String
youName =
    "MSK"


otherNames : List String
otherNames =
    -- TODO better names, maybe juice?
    [ "Hlavní město Praha"
    , "Středočeský kraj"
    , "Jihočeský kraj"
    , "Plzeňský kraj"
    , "Karlovarský kraj"
    , "Ústecký kraj"
    , "Liberecký kraj"
    , "Královéhradecký kraj"
    , "Pardubický kraj"
    , "Kraj Vysočina"
    , "Jihomoravský kraj"
    , "Zlínský kraj"
    , "Olomoucký kraj"
    ]


generator : String -> Generator Region
generator name =
    Random.constant
        { name = name
        , stats = Stats.init
        , upgrades = Upgrades.init
        }


listGenerator : Int -> Generator (List Region)
listGenerator n =
    Random.List.shuffle otherNames
        |> Random.map (List.take n)
        |> Random.andThen (Random.Extra.traverse generator)
