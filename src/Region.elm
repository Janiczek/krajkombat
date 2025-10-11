module Region exposing
    ( Region
    , advanceMonth
    , generator
    , listGenerator
    , youName
    )

import AssocSet
import Decision exposing (Decision)
import Random exposing (Generator)
import Random.Extra
import Random.List
import Resource exposing (Resources)
import Upgrade exposing (Upgrade)


type alias Region =
    { name : String
    , resources : Resources
    , upgrades : AssocSet.Set Upgrade
    , upgradesAvailable : List { upgrade : Upgrade, monthsLeft : Int }
    , availableDecisions : List Decision
    }


youName : String
youName =
    "MSK"


otherNames : List String
otherNames =
    [ "Pražaci"
    , "Středočesky kraj"
    , "Jihočesky kraj"
    , "Plzeňsky kraj"
    , "Karlovarsky kraj"
    , "Ústecky kraj"
    , "Liberecky kraj"
    , "Královehradecky kraj"
    , "Pardubicky kraj"
    , "Vysočina"
    , "Jihomoravsky kraj"
    , "Zlínsky kraj"
    , "Olomoucky kraj"
    ]


generator : String -> Generator Region
generator name =
    Random.constant
        (\availableDecisions ->
            { name = name
            , resources = Resource.init
            , upgrades = AssocSet.empty
            , upgradesAvailable = []
            , availableDecisions = availableDecisions
            }
        )
        |> Random.Extra.andMap Decision.listGenerator


listGenerator : Int -> Generator (List Region)
listGenerator n =
    Random.List.shuffle otherNames
        |> Random.map (List.take n)
        |> Random.andThen (Random.Extra.traverse generator)


advanceMonth : Region -> Generator Region
advanceMonth ({ resources } as region) =
    Random.constant
        (\availableDecisions ->
            { region
                | resources =
                    { resources
                        | ap = resources.ap + resources.apPerMonth
                        , bbv = resources.bbv + resources.bbvPerMonth
                    }
                , availableDecisions = availableDecisions
            }
        )
        |> Random.Extra.andMap Decision.listGenerator
