module TestRanking exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Ranking exposing (Ranking)
import Test exposing (Test)


type alias MinRegion =
    { name : String, stats : { bbv : Int } }


suite : Test
suite =
    Test.describe "Ranking"
        [ Test.test "rank example - you won" <|
            \() ->
                let
                    you : MinRegion
                    you =
                        { name = "MSK", stats = { bbv = 100 } }

                    others : List MinRegion
                    others =
                        [ { name = "Praha", stats = { bbv = 50 } }
                        , { name = "Brno", stats = { bbv = 30 } }
                        , { name = "Ostrava", stats = { bbv = 20 } }
                        , { name = "Plzeň", stats = { bbv = 10 } }
                        , { name = "Ústí nad Labem", stats = { bbv = 5 } }
                        , { name = "Liberec", stats = { bbv = 3 } }
                        , { name = "Hradec Králové", stats = { bbv = 2 } }
                        ]

                    ranking : Ranking
                    ranking =
                        Ranking.rank { you = you, others = others }
                in
                Expect.equal ranking
                    [ { rank = 0, bbv = 100, names = [ "MSK" ] }
                    , { rank = 1, bbv = 50, names = [ "Praha" ] }
                    , { rank = 2, bbv = 30, names = [ "Brno" ] }
                    , { rank = 3, bbv = 20, names = [ "Ostrava" ] }
                    , { rank = 4, bbv = 10, names = [ "Plzeň" ] }
                    , { rank = 5, bbv = 5, names = [ "Ústí nad Labem" ] }
                    , { rank = 6, bbv = 3, names = [ "Liberec" ] }
                    , { rank = 7, bbv = 2, names = [ "Hradec Králové" ] }
                    ]
        , Test.test "rank example - ties are grouped" <|
            \() ->
                let
                    you : MinRegion
                    you =
                        { name = "MSK", stats = { bbv = 100 } }

                    others : List MinRegion
                    others =
                        [ { name = "Praha", stats = { bbv = 100 } }
                        , { name = "Brno", stats = { bbv = 30 } }
                        , { name = "Ostrava", stats = { bbv = 20 } }
                        , { name = "Plzeň", stats = { bbv = 20 } }
                        , { name = "Ústí nad Labem", stats = { bbv = 5 } }
                        , { name = "Liberec", stats = { bbv = 3 } }
                        , { name = "Hradec Králové", stats = { bbv = 2 } }
                        ]

                    ranking : Ranking
                    ranking =
                        Ranking.rank { you = you, others = others }
                in
                Expect.equal ranking
                    [ { rank = 0, bbv = 100, names = [ "MSK", "Praha" ] }
                    , { rank = 2, bbv = 30, names = [ "Brno" ] }
                    , { rank = 3, bbv = 20, names = [ "Ostrava", "Plzeň" ] }
                    , { rank = 5, bbv = 5, names = [ "Ústí nad Labem" ] }
                    , { rank = 6, bbv = 3, names = [ "Liberec" ] }
                    , { rank = 7, bbv = 2, names = [ "Hradec Králové" ] }
                    ]
        ]
