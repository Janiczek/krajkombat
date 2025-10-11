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
import Random exposing (Generator)
import Random.Extra
import Ranking exposing (Ranking)
import Region exposing (Region)
import Resource


type Msg
    = MakeDecision Decision


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
        |> Random.Extra.andMap (Region.generator Region.youName)
        |> Random.Extra.andMap (Region.listGenerator otherRegionsCount)


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
                    List.foldl applyDecision aiRegion selectedDecisions
            )


applyDecision : Decision -> Region -> Region
applyDecision decision region =
    let
        newResources : Resource.Resources
        newResources =
            Decision.applyDeltas decision region.resources

        newAvailableDecisions : List Decision
        newAvailableDecisions =
            region.availableDecisions
                |> List.filter (\d -> d.flavorText /= decision.flavorText)
    in
    { region
        | resources = newResources
        , availableDecisions = newAvailableDecisions
    }


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


makeDecision : Decision -> Game -> Game
makeDecision decision ({ you } as game) =
    let
        updatedYou : Region
        updatedYou =
            applyDecision decision you
    in
    { game | you = updatedYou }
