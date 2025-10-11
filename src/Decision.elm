module Decision exposing
    ( Decision
    , DecisionType(..)
    , listGenerator
    )

import Dict
import Random exposing (Generator)
import Random.Extra
import ResourceDelta exposing (ResourceDelta(..))


type DecisionType
    = Investment
    | InvestmentLongTerm
    | Prevention


type alias Decision =
    { type_ : DecisionType
    , flavorText : String
    , deltas : List ResourceDelta
    }


maxDecisionsPerMonth : Int
maxDecisionsPerMonth =
    10


listGenerator : Generator (List Decision)
listGenerator =
    Random.list maxDecisionsPerMonth decisionGenerator
        |> Random.map keepUniqueNames


decisionGenerator : Generator Decision
decisionGenerator =
    decisionTypeGenerator
        |> Random.andThen
            (\type_ ->
                Random.constant
                    (\( flavorText, deltas ) ->
                        { type_ = type_
                        , flavorText = flavorText
                        , deltas = deltas
                        }
                    )
                    |> Random.Extra.andMap (decisionContentGenerator type_)
            )


decisionTypeGenerator : Generator DecisionType
decisionTypeGenerator =
    Random.uniform
        Investment
        [ InvestmentLongTerm
        , Prevention
        ]


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


decisionContentGenerator : DecisionType -> Generator ( String, List ResourceDelta )
decisionContentGenerator type_ =
    (case type_ of
        Investment ->
            Random.uniform
                ( "Rozkopej Rudnou ale musi to byt"
                , [ sub 40 50 AP
                  , addF 0.05 0.15 GREF
                  , addF 0.02 0.05 BREF
                  ]
                )
                [ ( "Oprav chodniky"
                  , [ sub 20 30 AP
                    , addF 0.01 0.03 GREF
                    , subF 0.01 0.03 BREF
                    ]
                  )
                , ( "Mistni sportovni kroužky"
                  , [ sub 30 40 AP
                    , addF 0.02 0.04 GREF
                    , add 1 2 BBVPerMonth
                    ]
                  )
                , ( "Modernizuj dopravu"
                  , [ sub 40 70 AP
                    , add 25 35 APPerMonth
                    , addF 0.08 0.12 GREF
                    ]
                  )
                , ( "Investuj do zdravotnictvi"
                  , [ sub 35 45 AP
                    , add 18 28 APPerMonth
                    , addF 0.07 0.12 GREF
                    , subF 0.04 0.09 BREF
                    ]
                  )
                , ( "Nova lepši sekretařka"
                  , [ sub 3 8 AP
                    , add 5 15 APPerMonth
                    , addF 0.01 0.03 GREF
                    , addF 0.01 0.03 BREF
                    ]
                  )
                ]

        InvestmentLongTerm ->
            Random.uniform
                ( "Dlouhodoba strategie rozvoje"
                , [ sub 30 50 AP
                  , sub 4 8 APPerMonth
                  , addF 0.05 0.15 GREF
                  , subF 0.02 0.08 BREF
                  ]
                )
                [ ( "Vyzkum do biohackingu"
                  , [ sub 80 120 AP
                    , sub 5 10 APPerMonth
                    , addF 0.15 0.25 GREF
                    , add 4 6 BBVPerMonth
                    ]
                  )
                , ( "Digitalizace veřejne spravy"
                  , [ sub 120 180 AP
                    , sub 15 20 APPerMonth
                    , addF 0.25 0.35 GREF
                    ]
                  )
                , ( "Koučovani tělocvikařu"
                  , [ sub 30 40 AP
                    , sub 5 10 APPerMonth
                    , add 8 10 BBV
                    , add 3 5 BBVPerMonth
                    ]
                  )
                , ( "Zelena transformace bo GrinDyl"
                  , [ sub 30 50 AP
                    , add 20 30 APPerMonth
                    , addF 0.07 0.12 GREF
                    ]
                  )
                , ( "Mezinarodni spoluprace"
                  , [ sub 40 60 AP
                    , add 10 20 APPerMonth
                    , addF 0.03 0.08 GREF
                    , addF 0.01 0.02 BREF
                    ]
                  )
                , ( "Dustojne platy učitelu"
                  , [ sub 10 20 APPerMonth
                    , addF 0.15 0.25 GREF
                    ]
                  )
                ]

        Prevention ->
            Random.uniform
                ( "Prevence kriminality"
                , [ sub 20 30 AP
                  , sub 5 15 APPerMonth
                  , subF 0.05 0.15 BREF
                  ]
                )
                [ ( "Bezpečnostní opatření"
                  , [ sub 20 40 AP
                    , subF 0.05 0.15 BREF
                    ]
                  )
                , ( "Sociálni programy pro chude"
                  , [ sub 15 25 AP
                    , add -1 10 APPerMonth
                    , addF 0.01 0.04 GREF
                    , subF 0.05 0.15 BREF
                    ]
                  )
                , ( "Policejni stat kamo"
                  , [ sub 10 30 AP
                    , add 5 15 APPerMonth
                    , subF 0.1 0.2 GREF
                    , subF 0.1 0.4 BREF
                    ]
                  )
                , ( "Komunitní práce"
                  , [ sub 18 28 AP
                    , add -5 10 APPerMonth
                    , addF 0.03 0.08 GREF
                    , subF 0.09 0.18 BREF
                    ]
                  )
                ]
    )
        |> Random.andThen
            (\( flavorText, deltas ) ->
                Random.Extra.sequence deltas
                    |> Random.map
                        (\deltas_ ->
                            ( flavorText
                            , deltas_
                                |> List.filter (\delta -> ResourceDelta.floatValue delta /= 0)
                            )
                        )
            )


keepUniqueNames : List Decision -> List Decision
keepUniqueNames decisions =
    decisions
        |> List.map (\decision -> ( decision.flavorText, decision ))
        |> Dict.fromList
        |> Dict.values
