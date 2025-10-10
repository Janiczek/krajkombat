module Resource exposing (Resources, init)


type alias Resources =
    { ap : Int
    , apPerMonth : Int
    , gref : Float
    , bref : Float
    , bbv : Int
    , bbvPerMonth : Int
    }


init : Resources
init =
    { ap = 100
    , apPerMonth = 100
    , gref = 1.0
    , bref = 1.0
    , bbv = 0
    , bbvPerMonth = 4 -- just under 100 after 24 months
    }
