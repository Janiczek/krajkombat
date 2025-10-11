module ResourceDelta exposing (ResourceDelta(..))


type ResourceDelta
    = AP Int
    | APPerMonth Int
    | GREF Float
    | BREF Float
    | BBV Int
    | BBVPerMonth Int
