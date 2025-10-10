module Game exposing (Game, Phase(..), Results(..), ResultsData, advanceMonth, end, gameInitGenerator)

import Logic
import Random exposing (Generator)
import Random.Extra
import Ranking exposing (Ranking)
import Region exposing (Region)


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
        |> Random.Extra.andMap (Region.listGenerator 3)


advanceMonth : Game -> Game
advanceMonth game =
    let
        newYou : Region
        newYou =
            Logic.advanceMonthForRegion game.you

        newOthers : List Region
        newOthers =
            game.others
                |> List.map Logic.advanceMonthForRegion
    in
    { game
        | you = newYou
        , others = newOthers
        , monthsLeft = game.monthsLeft - 1
    }


end : Game -> Results
end game =
    let
        ranking : Ranking
        ranking =
            Ranking.rank game

        resultsData : ResultsData
        resultsData =
            { you = game.you
            , others = game.others
            , ranking = ranking
            }
    in
    case ranking of
        fst :: _ ->
            if fst.names == [ Region.youName ] then
                YouWon resultsData

            else if List.member Region.youName fst.names then
                YouLostByDraw resultsData

            else
                YouLost resultsData

        [] ->
            Bug
