module Region exposing (Region, generator)

import Random exposing (Generator)
import Stats exposing (Stats)
import Upgrades exposing (Upgrades)


type alias Region =
    { stats : Stats
    , upgrades : Upgrades
    }


generator : Generator Region
generator =
    Random.constant
        { stats = Stats.init
        , upgrades = Upgrades.init
        }
