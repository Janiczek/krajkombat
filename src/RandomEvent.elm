module RandomEvent exposing (RandomEvent, listGenerator)

import Random exposing (Generator)
import ResourceDelta exposing (ResourceDelta(..), add, sub)


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
        , ( "Hackeři z SPŠE se někam naburali", [ add 140 200 AP, add 1 2 BREF ] )
        , ( "Hackeři z SPŠE zjistili že databaze KrajKombatu ma defaultni heslo", [ add 10 20 BBV ] )
        , ( "Naše škola byla nejlepši v republice kamo", [ add 40 80 AP, add 20 40 APPerMonth, add 3 5 GREF ] )
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
    Random.uniform ( "Přišli nam na podvod", [ sub 40 70 AP, add 2 5 BREF ] )
        [ ( "Na tom hřišti sme to moc nedali", [ add 5 10 BREF, sub 1 2 GREF ] )
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


listGenerator : { resources | gref : Int, bref : Int } -> Generator (List RandomEvent)
listGenerator { gref, bref } =
    Random.map2 (++)
        (listGenerator_ (toFloat gref / 100) goodGenerator)
        (listGenerator_ (toFloat bref / 100) badGenerator)


listGenerator_ : Int -> Generator RandomEvent -> Generator (List RandomEvent)
listGenerator_ ref gen =
    let
        sureEvents : Int
        sureEvents =
            ref // 100

        bonusPercent : Int
        bonusPercent =
            ref - sureEvents * 100
    in
    Random.int 0 100
        |> Random.andThen
            (\percentage ->
                let
                    bonus : Int
                    bonus =
                        if percentage < bonusPercent then
                            1

                        else
                            0

                    count : Int
                    count =
                        sureEvents + bonus
                in
                Random.list count gen
            )
