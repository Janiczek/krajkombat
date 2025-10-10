module Game exposing (Game, Phase(..), Region, gameInitGenerator)

import Random exposing (Generator)
import Random.Extra
import Stats exposing (Stats)
import Upgrades exposing (Upgrades)


type Phase
    = MainMenu
    | GameLoop Game
    | GameOver { you : Region, others : List Region }
    | GameWon { you : Region, others : List Region }


type alias Region =
    { stats : Stats
    , upgrades : Upgrades
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
        |> Random.Extra.andMap regionGenerator
        |> Random.Extra.andMap (Random.list 10 regionGenerator)


regionGenerator : Generator Region
regionGenerator =
    Random.constant
        { stats = Stats.init
        , upgrades = Upgrades.init
        }
