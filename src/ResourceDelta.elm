module ResourceDelta exposing
    ( ResourceDelta(..)
    , add
    , addF
    , bundleGenerator
    , floatValue
    , sub
    , subF
    )

import Random exposing (Generator)
import Random.Extra


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


add : Int -> Int -> (Int -> ResourceDelta) -> Generator ResourceDelta
add min max constructor =
    Random.int min max
        |> Random.map constructor


addF : Float -> Float -> (Float -> ResourceDelta) -> Generator ResourceDelta
addF min max constructor =
    Random.float min max
        |> Random.map constructor


sub : Int -> Int -> (Int -> ResourceDelta) -> Generator ResourceDelta
sub min max constructor =
    Random.int min max
        |> Random.map (negate >> constructor)


subF : Float -> Float -> (Float -> ResourceDelta) -> Generator ResourceDelta
subF min max constructor =
    Random.float min max
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
                                |> List.filter (\delta -> floatValue delta /= 0)
                            )
                        )
            )
