module Stats exposing (Stats, init)


type alias Stats =
    { ap : Int
    , apPerMonth : Int
    , gref : Float
    , bref : Float
    , bbv : Int
    }


init : Stats
init =
    { ap = 100
    , apPerMonth = 100
    , gref = 1.0
    , bref = 1.0
    , bbv = 0
    }
