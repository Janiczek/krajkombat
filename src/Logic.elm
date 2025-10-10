module Logic exposing
    ( DecisionOutcome(..)
    , advanceMonthForRegion
    , presidentBribeChance
    , presidentUpgradeChance
    )

import Region exposing (Region)


type DecisionOutcome
    = AddToAp Int
    | AddToApPerMonth Int
    | AddToGref Float
    | AddToBref Float
    | AddToBbv Int


presidentBribeChance :
    { stats : { stats | bbv : Int }
    , bbvRanking : Int
    , regions : Int
    }
    -> Float
presidentBribeChance { stats, bbvRanking, regions } =
    Debug.todo "president bribe chance"


presidentUpgradeChance :
    { stats : { stats | bbv : Int }
    , bbvRanking : Int
    , regions : Int
    }
    -> Float
presidentUpgradeChance { stats, bbvRanking, regions } =
    Debug.todo "president upgrade chance"


advanceMonthForRegion : Region -> Region
advanceMonthForRegion ({ stats } as region) =
    { region
        | stats = { stats | ap = stats.ap + stats.apPerMonth }
    }
