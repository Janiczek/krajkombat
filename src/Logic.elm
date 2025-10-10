module Logic exposing (Outcome(..), presidentBribeChance, presidentUpgradeChance)


type Outcome
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
