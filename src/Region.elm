module Region exposing
    ( Region
    , advanceMonth
    , applyDecision
    , applyRandomEvent
    , applyRandomEvents
    , discardDecision
    , initGenerator
    , initListGenerator
    , youName
    )

import AssocSet
import Decision exposing (Decision)
import Random exposing (Generator)
import Random.Extra
import Random.List
import RandomEvent exposing (RandomEvent)
import Resource exposing (Resources)
import Upgrade exposing (Upgrade)


type alias Region =
    { name : String
    , resources : Resources
    , upgrades : AssocSet.Set Upgrade
    , upgradesAvailable : List { upgrade : Upgrade, monthsLeft : Int }
    , availableDecisions : List Decision
    , randomEvents : List RandomEvent
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


initGenerator : String -> Generator Region
initGenerator name =
    let
        resources : Resources
        resources =
            Resource.init
    in
    Random.constant
        (\availableDecisions ->
            { name = name
            , resources = resources
            , upgrades = AssocSet.empty
            , upgradesAvailable = []
            , availableDecisions = availableDecisions
            , randomEvents = []
            }
        )
        |> Random.Extra.andMap (Decision.listGenerator resources)


initListGenerator : Int -> Generator (List Region)
initListGenerator n =
    Random.List.shuffle otherNames
        |> Random.map (List.take n)
        |> Random.andThen (Random.Extra.traverse initGenerator)


advanceMonth : Region -> Generator Region
advanceMonth ({ resources } as region) =
    let
        newResources : Resources
        newResources =
            { resources
                | ap = resources.ap + resources.apPerMonth
                , bbv = resources.bbv + resources.bbvPerMonth
            }
    in
    Random.constant
        (\availableDecisions randomEvents ->
            { region
                | resources = newResources
                , availableDecisions = availableDecisions
                , randomEvents = randomEvents
            }
        )
        |> Random.Extra.andMap (Decision.listGenerator newResources)
        |> Random.Extra.andMap (RandomEvent.listGenerator newResources)


applyRandomEvent : RandomEvent -> Region -> Region
applyRandomEvent event region =
    let
        newResources : Resource.Resources
        newResources =
            List.foldl Resource.applyDelta region.resources event.deltas
    in
    { region | resources = newResources }


applyRandomEvents : List RandomEvent -> Region -> Region
applyRandomEvents events region =
    List.foldl applyRandomEvent region events


applyDecision : Decision -> Region -> Region
applyDecision decision region =
    let
        newResources : Resource.Resources
        newResources =
            region.resources
                |> Resource.applyDeltas decision.deltas

        newAvailableDecisions : List Decision
        newAvailableDecisions =
            region.availableDecisions
                |> List.filter (\d -> d.flavorText /= decision.flavorText)
    in
    { region
        | resources = newResources
        , availableDecisions = newAvailableDecisions
    }


discardDecision : Decision -> Region -> Region
discardDecision decision region =
    { region
        | availableDecisions =
            region.availableDecisions
                |> List.filter (\d -> d.flavorText /= decision.flavorText)
    }
