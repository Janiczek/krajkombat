module TestGame exposing (suite)

import Expect
import Fuzz
import Game exposing (Game, Results(..), end)
import Region exposing (Region, youName)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Game"
        [ Test.describe "end"
            [ Test.test "end example - you won" <|
                \() ->
                    let
                        game : Game
                        game =
                            { you = createRegion youName 100
                            , others =
                                [ createRegion "Praha" 80
                                , createRegion "Brno" 90
                                ]
                            , monthsLeft = 0
                            }

                        result : Results
                        result =
                            end game
                    in
                    case result of
                        YouWon _ ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected YouWon result"
            , Test.test "end example - you lost by draw" <|
                \() ->
                    let
                        game : Game
                        game =
                            { you = createRegion youName 100
                            , others =
                                [ createRegion "Praha" 100
                                , createRegion "Brno" 90
                                ]
                            , monthsLeft = 0
                            }

                        result : Results
                        result =
                            end game
                    in
                    case result of
                        YouLostByDraw _ ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected YouLostByDraw result"
            , Test.test "end example - you lost" <|
                \() ->
                    let
                        game : Game
                        game =
                            { you = createRegion youName 80
                            , others =
                                [ createRegion "Praha" 100
                                , createRegion "Brno" 90
                                ]
                            , monthsLeft = 0
                            }

                        result : Results
                        result =
                            end game
                    in
                    case result of
                        YouLost _ ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected YouLost result"
            , Test.fuzz
                (Fuzz.map3
                    (\youBbv otherBbvs monthsLeft ->
                        { you = createRegion youName youBbv
                        , others = List.indexedMap (\i bbv -> createRegion ("Region" ++ String.fromInt i) bbv) otherBbvs
                        , monthsLeft = monthsLeft
                        }
                    )
                    (Fuzz.intRange 0 1000)
                    (Fuzz.list (Fuzz.intRange 0 1000) |> Fuzz.map (List.take 3))
                    (Fuzz.intRange 0 24)
                )
                "end - fuzz - never Bug"
                (\game ->
                    case end game of
                        Bug ->
                            Expect.fail "Game.end should never return Bug with valid game state"

                        _ ->
                            Expect.pass
                )
            ]
        ]


createRegion : String -> Int -> Region
createRegion name bbv =
    { name = name
    , stats =
        { ap = 100
        , apPerMonth = 100
        , gref = 1.0
        , bref = 0.8
        , bbv = bbv
        }
    , upgrades = { blackHatBootcamp = False }
    }
