module RandomEvent exposing (RandomEvent, listGenerator)

import Random exposing (Generator)
import ResourceDelta exposing (ResourceDelta(..), add, addF, sub, subF)


type alias RandomEvent =
    { flavorText : String
    , deltas : List ResourceDelta
    , isGood : Bool
    }


{-| TODO use ranges instead of constants
-}
goodGenerator : Generator RandomEvent
goodGenerator =
    Random.uniform ( "Dotace vyšly hej", [ add 150 300 AP ] )
        [ ( "Naš tym ty Pražaky zas uplně rozdupal", [ add 40 80 AP, add 15 30 BBV ] )
        , ( "Hackeři z SPŠE se někam naburali", [ add 140 200 AP, addF 0.01 0.02 BREF ] )
        , ( "Hackeři z SPŠE zjistili že databaze KrajKombatu ma defaultni heslo", [ add 10 20 BBV ] )
        , ( "Naše škola byla nejlepši v republice kamo", [ add 40 80 AP, add 20 40 APPerMonth, addF 0.03 0.05 GREF ] )
        ]
        |> ResourceDelta.bundleGenerator
        |> Random.map
            (\( flavorText, deltas ) ->
                { flavorText = flavorText
                , deltas = deltas
                , isGood = True
                }
            )


{-| TODO use ranges instead of constants
-}
badGenerator : Generator RandomEvent
badGenerator =
    Random.uniform ( "Přišli nam na podvod", [ sub 40 70 AP, addF 0.02 0.05 BREF ] )
        [ ( "Na tom hřišti sme to moc nedali", [ addF 0.05 0.1 BREF, subF 0.01 0.02 GREF ] )
        , ( "Zas nějaky doping", [ sub 20 40 AP, sub 2 4 BBVPerMonth ] )
        , ( "Hackeři z SPŠE po sobě nezametli stopy kuva", [ sub 30 60 AP ] )
        ]
        |> ResourceDelta.bundleGenerator
        |> Random.map
            (\( flavorText, deltas ) ->
                { flavorText = flavorText
                , deltas = deltas
                , isGood = False
                }
            )


listGenerator : { resources | gref : Float, bref : Float } -> Generator (List RandomEvent)
listGenerator { gref, bref } =
    Random.map2 (++)
        (listGenerator_ gref goodGenerator)
        (listGenerator_ bref badGenerator)


listGenerator_ : Float -> Generator RandomEvent -> Generator (List RandomEvent)
listGenerator_ ref gen =
    let
        intPart : Int
        intPart =
            floor ref

        fracPart : Float
        fracPart =
            ref - toFloat intPart
    in
    Random.float 0 1
        |> Random.andThen
            (\p ->
                let
                    bonus : Int
                    bonus =
                        if p < fracPart then
                            1

                        else
                            0

                    count : Int
                    count =
                        intPart + bonus
                in
                Random.list count gen
            )
