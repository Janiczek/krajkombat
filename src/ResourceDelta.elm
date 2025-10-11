module ResourceDelta exposing (ResourceDelta(..), floatValue)


type ResourceDelta
    = AP Int
    | APPerMonth Int
    | GREF Float
    | BREF Float
    | BBV Int
    | BBVPerMonth Int


floatValue : ResourceDelta -> Float
floatValue delta =
    case delta of
        AP n ->
            toFloat n

        APPerMonth n ->
            toFloat n

        GREF n ->
            n

        BREF n ->
            n

        BBV n ->
            toFloat n

        BBVPerMonth n ->
            toFloat n
