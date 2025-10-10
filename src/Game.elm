module Game exposing (Game, Phase(..), Results, advanceMonth, end, gameInitGenerator)

import Logic
import Random exposing (Generator)
import Random.Extra
import Region exposing (Region)


type Phase
    = MainMenu
    | Intro
    | GameLoop Game
    | GameOver Results
    | GameWon Results


type alias Results =
    { you : Region
    , others : List Region
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
        |> Random.Extra.andMap Region.generator
        |> Random.Extra.andMap (Random.list 3 Region.generator)


advanceMonth : Game -> Game
advanceMonth game =
    let
        newYou =
            Logic.advanceMonthForRegion game.you

        newOthers =
            game.others
                |> List.map Logic.advanceMonthForRegion

        newGame =
            { game
                | you = newYou
                , others = newOthers
                , monthsLeft = game.monthsLeft - 1
            }
    in
    newGame


end : Game -> Result Results Results
end game =
    Debug.todo "Game.end"
