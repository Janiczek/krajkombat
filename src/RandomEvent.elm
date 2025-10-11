module RandomEvent exposing (RandomEvent, listGenerator)

import Random exposing (Generator)
import ResourceDelta exposing (ResourceDelta(..))


type alias RandomEvent =
    { flavorText : String
    , deltas : List ResourceDelta
    , isGood : Bool
    }


{-| TODO use ranges instead of constants
-}
goodGenerator : Generator RandomEvent
goodGenerator =
    Random.uniform ( "Dotace vyšly hej", [ AP 200 ] )
        [ ( "Naš tym je uplně rozdupal", [ AP 50, BBV 20 ] )
        , ( "Hackeři z SPŠE se někam naburali", [ AP 150, BREF 0.01 ] )
        , ( "Hackeři z SPŠE zjistili že databaze KrajKombatu ma defaultni heslo", [ BBV 50 ] )
        , ( "Naše škola byla nejlepši v republice kamo", [ AP 50, APPerMonth 20, GREF 0.03 ] )
        ]
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
    Random.uniform ( "Přišli nam na podvod", [ AP -50, BREF 0.03 ] )
        [ ( "Na tom hřišti sme to moc nedali", [ BREF 0.05 ] )
        , ( "Zas nějaky doping", [ AP -30, BBVPerMonth -2 ] )
        , ( "Hackeři z SPŠE po sobě nezametli stopy kuva", [ AP -50 ] )
        ]
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
