module Game exposing
    ( Game
    , Msg(..)
    , Phase(..)
    , Results(..)
    , ResultsData
    , advanceMonth
    , end
    , gameInitGenerator
    , update
    )

import Decision exposing (Decision)
import List.Extra
import Random exposing (Generator)
import Random.Extra
import RandomEvent
import Ranking exposing (Ranking)
import Region exposing (Region)
import Resource exposing (Resources)


type Msg
    = MakeDecision Decision
    | DiscardDecision Decision
    | ApplyNextRandomEvent
    | ApplyBlackHatOperation { regionName : String }


type Phase
    = MainMenu
    | Intro
    | GameLoop Game
    | GameEnded Results


type Results
    = YouWon ResultsData
    | YouLost ResultsData
    | YouLostByDraw ResultsData
    | Bug


type alias ResultsData =
    { you : Region
    , others : List Region
    , ranking : Ranking
    }


type alias Game =
    { you : Region
    , others : List Region
    , monthsLeft : Int
    }


otherRegionsCount : Int
otherRegionsCount =
    3


gameInitGenerator : Generator Game
gameInitGenerator =
    Random.constant
        (\you others ->
            { you = you
            , others = others
            , monthsLeft = 24
            }
        )
        |> Random.Extra.andMap (Region.initGenerator Region.youName)
        |> Random.Extra.andMap (Region.initListGenerator otherRegionsCount)


makeAIDecisions : Region -> Generator Region
makeAIDecisions aiRegion =
    let
        boolListGenerator : Generator (List Bool)
        boolListGenerator =
            Random.list (List.length aiRegion.availableDecisions) Random.Extra.bool
    in
    boolListGenerator
        |> Random.map
            (\bools ->
                let
                    selectedDecisions : List Decision
                    selectedDecisions =
                        List.map2 Tuple.pair bools aiRegion.availableDecisions
                            |> List.filterMap
                                (\( takeIt, d ) ->
                                    if takeIt then
                                        Just d

                                    else
                                        Nothing
                                )
                in
                if List.isEmpty selectedDecisions then
                    aiRegion

                else
                    List.foldl Region.applyDecision aiRegion selectedDecisions
            )


advanceMonth : Game -> Generator Game
advanceMonth game =
    Random.constant
        (\you others ->
            { game
                | you = you
                , others = others
                , monthsLeft = game.monthsLeft - 1
            }
        )
        |> Random.Extra.andMap (Region.advanceMonth game.you)
        |> Random.Extra.andMap
            (game.others
                |> Random.Extra.traverse makeAIDecisions
                |> Random.andThen (Random.Extra.traverse Region.advanceMonth)
            )
        |> Random.andThen generateAndApplyRandomEventsToOthers


generateAndApplyRandomEventsToOthers : Game -> Generator Game
generateAndApplyRandomEventsToOthers game =
    game.others
        |> Random.Extra.traverse generateAndApplyRandomEventsToRegion
        |> Random.map (\others -> { game | others = others })


generateAndApplyRandomEventsToRegion : Region -> Generator Region
generateAndApplyRandomEventsToRegion region =
    RandomEvent.listGenerator region.resources
        |> Random.map (\randomEvents -> Region.applyRandomEvents randomEvents region)


end : Game -> Results
end game =
    let
        ranking : Ranking
        ranking =
            Ranking.rank game
    in
    case ranking of
        fst :: _ ->
            let
                resultsData : ResultsData
                resultsData =
                    { you = game.you
                    , others = game.others
                    , ranking = ranking
                    }
            in
            if fst.names == [ Region.youName ] then
                YouWon resultsData

            else if List.member Region.youName fst.names then
                YouLostByDraw resultsData

            else
                YouLost resultsData

        [] ->
            Bug


update : Msg -> Game -> Game
update msg game =
    case msg of
        MakeDecision decision ->
            makeDecision decision game

        DiscardDecision decision ->
            discardDecision decision game

        ApplyNextRandomEvent ->
            applyNextRandomEvent game

        ApplyBlackHatOperation regionName ->
            applyBlackHatOperation regionName game


applyBlackHatOperation : { regionName : String } -> Game -> Game
applyBlackHatOperation { regionName } ({ you, others } as game) =
    let
        yourResources : Resources
        yourResources =
            you.resources
    in
    case List.Extra.find (\r -> r.name == regionName) others of
        Just other ->
            let
                amount =
                    other.resources.bbv // 2

                theirNewBbv =
                    other.resources.bbv - amount

                yourNewBbv =
                    yourResources.bbv + amount

                theirResources =
                    other.resources
            in
            { game
                | you = { you | resources = { yourResources | bbv = yourNewBbv } }
                , others =
                    others
                        |> List.map
                            (\r ->
                                if r.name == regionName then
                                    { other | resources = { theirResources | bbv = theirNewBbv } }

                                else
                                    r
                            )
            }

        Nothing ->
            game


makeDecision : Decision -> Game -> Game
makeDecision decision ({ you } as game) =
    { game | you = you |> Region.applyDecision decision }


discardDecision : Decision -> Game -> Game
discardDecision decision ({ you } as game) =
    { game | you = you |> Region.discardDecision decision }


applyNextRandomEvent : Game -> Game
applyNextRandomEvent ({ you } as game) =
    case you.randomEvents of
        [] ->
            game

        currentEvent :: rest ->
            let
                newResources : Resource.Resources
                newResources =
                    you.resources
                        |> Resource.applyDeltas currentEvent.deltas

                newYou : Region
                newYou =
                    { you
                        | resources = newResources
                        , randomEvents = rest
                    }
            in
            { game | you = newYou }
