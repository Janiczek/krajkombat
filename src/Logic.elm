module Logic exposing
    ( advanceMonthForRegion
    , presidentBribeChance
    , presidentUpgradeChance
    )

import Region exposing (Region)


presidentBribeChance :
    { resources : { resources | bbv : Int }
    , bbvRanking : Int
    , regions : Int
    }
    -> Float
presidentBribeChance { resources, bbvRanking, regions } =
    Debug.todo "president bribe chance"


presidentUpgradeChance :
    { resources : { resources | bbv : Int }
    , bbvRanking : Int
    , regions : Int
    }
    -> Float
presidentUpgradeChance { resources, bbvRanking, regions } =
    Debug.todo "president upgrade chance"


advanceMonthForRegion : Region -> Region
advanceMonthForRegion ({ resources } as region) =
    { region
        | resources =
            { resources
                | ap = resources.ap + resources.apPerMonth
                , bbv = resources.bbv + resources.bbvPerMonth
            }
    }
