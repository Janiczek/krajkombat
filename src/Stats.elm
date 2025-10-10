module Stats exposing (Stats, init, regionRanking)


type alias Stats =
    { ap : Int
    , apPerMonth : Int
    , gref : Float
    , bref : Float
    , bbv : Int
    }


regionRanking : { yourBbv : Int, otherBbvs : List Int } -> Int
regionRanking { yourBbv, otherBbvs } =
    Debug.todo "region ranking"


init : Stats
init =
    { ap = 100
    , apPerMonth = 100
    , gref = 1.0
    , bref = 0.8
    , bbv = 0
    }
