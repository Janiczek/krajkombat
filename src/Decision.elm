module Decision exposing
    ( Decision
    , Type(..)
    , listGenerator
    , typeLabel
    )

import Dict
import Random exposing (Generator)
import Random.Extra
import Resource exposing (Resources)
import ResourceDelta exposing (ResourceDelta(..), add, sub)


type Type
    = Investment
    | InvestmentLongTerm
    | Prevention


type alias Decision =
    { type_ : Type
    , flavorText : String
    , deltas : List ResourceDelta
    }


maxDecisionsPerMonth : Int
maxDecisionsPerMonth =
    6


listGenerator : Resources -> Generator (List Decision)
listGenerator resources =
    Random.list maxDecisionsPerMonth decisionGenerator
        |> Random.map keepUniqueNames
        |> Random.map (List.filter (\decision -> Resource.canApplyDeltas resources decision.deltas))


decisionGenerator : Generator Decision
decisionGenerator =
    typeGenerator
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


typeGenerator : Generator Type
typeGenerator =
    Random.uniform
        Investment
        [ InvestmentLongTerm
        , Prevention
        ]


decisionContentGenerator : Type -> Generator ( String, List ResourceDelta )
decisionContentGenerator type_ =
    ResourceDelta.bundleGenerator <|
        case type_ of
            Investment ->
                Random.uniform
                    ( "Rozkopej Rudnou ale musi to byt"
                    , [ sub 40 50 AP
                      , add 5 15 GREF
                      , add 2 5 BREF
                      ]
                    )
                    [ ( "Oprav chodniky"
                      , [ sub 20 30 AP
                        , sub 1 3 BREF
                        ]
                      )
                    , ( "Mistni sportovni kroužky"
                      , [ sub 30 40 AP
                        , add 2 4 GREF
                        , add 1 2 BBVPerMonth
                        ]
                      )
                    , ( "Postav nove Bazaly"
                      , [ sub 90 160 AP
                        , add 30 80 APPerMonth
                        , add 10 15 GREF
                        , add 5 10 BBVPerMonth
                        ]
                      )
                    , ( "Modernizuj dopravu"
                      , [ sub 40 70 AP
                        , add 25 35 APPerMonth
                        , add 8 12 GREF
                        ]
                      )
                    , ( "Investuj do zdravotnictvi"
                      , [ sub 35 45 AP
                        , add 18 28 APPerMonth
                        , sub 4 9 BREF
                        ]
                      )
                    , ( "Nova lepši sekretařka"
                      , [ sub 3 8 AP
                        , add 5 15 APPerMonth
                        , add 1 3 GREF
                        , add 1 3 BREF
                        ]
                      )
                    , ( "Sabatikal na Bali"
                      , [ sub 20 40 AP
                        , add 2 4 GREF
                        , add 1 3 BREF
                        ]
                      )
                    ]

            InvestmentLongTerm ->
                Random.uniform
                    ( "Dlouhodoba strategie rozvoje"
                    , [ sub 30 50 AP
                      , sub 4 8 APPerMonth
                      , add 5 15 GREF
                      ]
                    )
                    [ ( "Vyzkum do biohackingu"
                      , [ sub 80 120 AP
                        , sub 5 10 APPerMonth
                        , add 15 25 GREF
                        , add 4 6 BBVPerMonth
                        ]
                      )
                    , ( "Digitalizace veřejne spravy"
                      , [ sub 120 180 AP
                        , sub 15 20 APPerMonth
                        , add 25 35 GREF
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
                        , add 7 12 GREF
                        ]
                      )
                    , ( "Mezinarodni spoluprace"
                      , [ sub 40 60 AP
                        , add 10 20 APPerMonth
                        , add 3 8 GREF
                        ]
                      )
                    , ( "Dustojne platy učitelu"
                      , [ sub 10 20 APPerMonth
                        , add 15 25 GREF
                        ]
                      )
                    ]

            Prevention ->
                Random.uniform
                    ( "Prevence kriminality"
                    , [ sub 20 30 AP
                      , sub 5 15 APPerMonth
                      , sub 5 15 BREF
                      ]
                    )
                    [ ( "Najmi sekuriťaky na Bazaly"
                      , [ sub 20 40 AP
                        , sub 5 15 BREF
                        ]
                      )
                    , ( "Sociálni programy pro chude"
                      , [ sub 15 25 AP
                        , add -1 10 APPerMonth
                        , sub 5 15 BREF
                        ]
                      )
                    , ( "Policejni stat uvnitř statu kamo"
                      , [ sub 50 80 AP
                        , add 10 30 APPerMonth
                        , sub 10 20 GREF
                        , sub 10 40 BREF
                        ]
                      )
                    , ( "Komunitní práce"
                      , [ sub 18 28 AP
                        , add -5 10 APPerMonth
                        , add 3 8 GREF
                        ]
                      )
                    ]


keepUniqueNames : List Decision -> List Decision
keepUniqueNames decisions =
    decisions
        |> List.map (\decision -> ( decision.flavorText, decision ))
        |> Dict.fromList
        |> Dict.values


typeLabel : Type -> String
typeLabel type_ =
    case type_ of
        Investment ->
            "Investice"

        InvestmentLongTerm ->
            "Dlouhodobe investice"

        Prevention ->
            "Prevence"
