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
        |> Random.Extra.andMap (Random.Extra.traverse Region.advanceMonth game.others)


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
makeDecision decision game =
    let
        updatedYou : Region
        updatedYou =
            { name = game.you.name
            , resources = game.you.resources
            , upgrades = game.you.upgrades
            , upgradesAvailable = game.you.upgradesAvailable
            , availableDecisions =
                game.you.availableDecisions
                    -- this works because we made the names unique in the generator
                    |> List.filter (\d -> d.flavorText /= decision.flavorText)
            }
    in
    { game | you = updatedYou }
