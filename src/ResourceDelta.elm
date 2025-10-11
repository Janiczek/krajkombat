module ResourceDelta exposing
    ( ResourceDelta(..)
    , add
    , bundleGenerator
    , sub
    )

import Random exposing (Generator)
import Random.Extra


type ResourceDelta
    = AP Int
    | APPerMonth Int
    | GREF Int
    | BREF Int
    | BBV Int
    | BBVPerMonth Int


value : ResourceDelta -> Int
value delta =
    case delta of
        AP n ->
            n

        APPerMonth n ->
            n

        GREF n ->
            n

        BREF n ->
            n

        BBV n ->
            n

        BBVPerMonth n ->
            n


add : Int -> Int -> (Int -> ResourceDelta) -> Generator ResourceDelta
add min max constructor =
    Random.int min max
        |> Random.map constructor


sub : Int -> Int -> (Int -> ResourceDelta) -> Generator ResourceDelta
sub min max constructor =
    Random.int min max
        |> Random.map (negate >> constructor)


bundleGenerator : Generator ( String, List (Generator ResourceDelta) ) -> Generator ( String, List ResourceDelta )
bundleGenerator generator =
    generator
        |> Random.andThen
            (\( flavorText, deltas ) ->
                Random.Extra.sequence deltas
                    |> Random.map
                        (\deltas_ ->
                            ( flavorText
                            , deltas_
                                |> List.filter (\delta -> value delta /= 0)
                            )
                        )
            )
