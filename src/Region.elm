module Region exposing
    ( Region
    , advanceMonth
    , applyDecision
    , applyRandomEvents
    , buyUpgrade
    , discardDecision
    , hasAvailableUpgrades
    , hasPurchasedUpgrades
    , initGenerator
    , initListGenerator
    , youName
    )

import Decision exposing (Decision)
import Random exposing (Generator)
import Random.Extra
import Random.List
import RandomEvent exposing (RandomEvent)
import Resource exposing (Resources)
import ResourceDelta exposing (ResourceDelta)
import Upgrade exposing (Upgrade(..))


type alias Region =
    { name : String
    , resources : Resources
    , availableDecisions : List Decision
    , randomEvents : List RandomEvent
    , upgradesAvailable : List Upgrade

    -- Upgrades
    , blackHatUpgrade : Maybe Upgrade.BlackHatData
    , dataAnalyticsUpgrade : Bool
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
            , availableDecisions = availableDecisions
            , randomEvents = []
            , upgradesAvailable = []
            , blackHatUpgrade = Nothing
            , dataAnalyticsUpgrade = False
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
        (\availableDecisions randomEvents addBlackHatUpgrade addDataAnalyticsUpgrade ->
            let
                addedUpgrades : List Upgrade
                addedUpgrades =
                    [ if
                        addBlackHatUpgrade
                            && (region.blackHatUpgrade == Nothing)
                            && not (List.member BlackHatBootcamp region.upgradesAvailable)
                      then
                        Just BlackHatBootcamp

                      else
                        Nothing
                    , if
                        addDataAnalyticsUpgrade
                            && not region.dataAnalyticsUpgrade
                            && not (List.member DataAnalytics region.upgradesAvailable)
                      then
                        Just DataAnalytics

                      else
                        Nothing
                    ]
                        |> List.filterMap identity
            in
            { region
                | resources = newResources
                , availableDecisions = availableDecisions
                , randomEvents = randomEvents
                , blackHatUpgrade =
                    region.blackHatUpgrade
                        |> Maybe.map Upgrade.advanceBlackHat
                , upgradesAvailable =
                    region.upgradesAvailable ++ addedUpgrades
            }
        )
        |> Random.Extra.andMap (Decision.listGenerator newResources)
        |> Random.Extra.andMap (RandomEvent.listGenerator newResources)
        |> Random.Extra.andMap (upgradeChanceGenerator BlackHatBootcamp)
        |> Random.Extra.andMap (upgradeChanceGenerator DataAnalytics)


upgradeChanceGenerator : Upgrade -> Generator Bool
upgradeChanceGenerator upgrade =
    Random.float 0 1
        |> Random.map (\chance -> chance < Upgrade.chance upgrade)


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


hasPurchasedUpgrades : Region -> Bool
hasPurchasedUpgrades region =
    List.any identity [ region.blackHatUpgrade /= Nothing ]


hasAvailableUpgrades : Region -> Bool
hasAvailableUpgrades region =
    not (List.isEmpty region.upgradesAvailable)


buyUpgrade : Upgrade -> Region -> Region
buyUpgrade upgrade region =
    let
        cost : List ResourceDelta
        cost =
            Upgrade.cost upgrade
    in
    if Resource.canApplyDeltas region.resources cost then
        { region
            | upgradesAvailable = region.upgradesAvailable |> List.filter (\u -> u /= upgrade)
            , resources = region.resources |> Resource.applyDeltas cost
        }
            |> initializeUpgrade upgrade

    else
        region


initializeUpgrade : Upgrade -> Region -> Region
initializeUpgrade upgrade region =
    case upgrade of
        BlackHatBootcamp ->
            { region | blackHatUpgrade = Just Upgrade.initBlackHat }

        DataAnalytics ->
            { region | dataAnalyticsUpgrade = True }
