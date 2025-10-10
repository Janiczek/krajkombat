module Main exposing (Flags, Model, Msg, main)

import Browser
import Game exposing (Game)
import Html exposing (Html)
import Html.Events
import Random
import Stats exposing (Stats)
import Upgrades exposing (Upgrades)


type alias Flags =
    ()


type alias Model =
    { gamePhase : Game.Phase
    }


type Msg
    = StartGame
    | GameLoopInitialized Game


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init () =
    ( { gamePhase = Game.MainMenu }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( model
            , Random.generate GameLoopInitialized Game.gameInitGenerator
            )

        GameLoopInitialized game ->
            ( { model | gamePhase = Game.GameLoop game }
            , Cmd.none
            )


title : String
title =
    "KrajKombat: MSK Edition"


view : Model -> Browser.Document Msg
view model =
    { title = title
    , body = viewGamePhase model.gamePhase
    }


viewGamePhase : Game.Phase -> List (Html Msg)
viewGamePhase phase =
    case phase of
        Game.MainMenu ->
            viewMainMenu

        Game.GameLoop game ->
            viewGameLoop game

        Game.GameOver results ->
            viewGameOver results

        Game.GameWon results ->
            viewGameWon results


viewMainMenu : List (Html Msg)
viewMainMenu =
    [ Html.h1 [] [ Html.text title ]
    , Html.button
        [ Html.Events.onClick StartGame ]
        [ Html.text "Start Game" ]
    ]


viewGameLoop : Game -> List (Html Msg)
viewGameLoop game =
    [ Html.h2 [] [ Html.text "GameLoop Phase" ]
    , Html.div []
        [ Html.h3 [] [ Html.text "Your Region:" ]
        , viewRegion game.you
        , Html.h3 [] [ Html.text "Other Regions:" ]
        , Html.ul []
            (game.others
                |> List.indexedMap
                    (\i r ->
                        Html.li []
                            [ Html.text ("Region " ++ String.fromInt (i + 1) ++ ":")
                            , viewRegion r
                            ]
                    )
            )
        , Html.div []
            [ Html.text ("Months left: " ++ String.fromInt game.monthsLeft) ]
        ]
    ]


viewGameOver : Game.Results -> List (Html Msg)
viewGameOver results =
    [ Html.h2 [] [ Html.text "GameOver Phase" ]
    , Html.h3 [] [ Html.text "Your Results:" ]
    , viewRegion results.you
    , Html.h3 [] [ Html.text "Other Regions:" ]
    , Html.ul []
        (results.others
            |> List.indexedMap
                (\i r ->
                    Html.li []
                        [ Html.text ("Region " ++ String.fromInt (i + 1) ++ ":")
                        , viewRegion r
                        ]
                )
        )
    ]


viewGameWon : Game.Results -> List (Html Msg)
viewGameWon results =
    [ Html.h2 [] [ Html.text "GameWon Phase" ]
    , Html.h3 [] [ Html.text "Your Results:" ]
    , viewRegion results.you
    , Html.h3 [] [ Html.text "Other Regions:" ]
    , Html.ul []
        (List.indexedMap
            (\i r -> Html.li [] [ Html.text ("Region " ++ String.fromInt (i + 1) ++ ":"), viewRegion r ])
            results.others
        )
    ]


viewRegion : Game.Region -> Html msg
viewRegion region =
    Html.div []
        [ Html.h4 [] [ Html.text "Stats:" ]
        , viewStats region.stats
        , Html.h4 [] [ Html.text "Upgrades:" ]
        , viewUpgrades region.upgrades
        ]


viewStats : Stats -> Html msg
viewStats stats =
    Html.ul []
        [ Html.li [] [ Html.text ("AP: " ++ String.fromInt stats.ap) ]
        , Html.li [] [ Html.text ("AP per Month: " ++ String.fromInt stats.apPerMonth) ]
        , Html.li [] [ Html.text ("GREF: " ++ String.fromFloat stats.gref) ]
        , Html.li [] [ Html.text ("BREF: " ++ String.fromFloat stats.bref) ]
        , Html.li [] [ Html.text ("BBV: " ++ String.fromInt stats.bbv) ]
        ]


viewUpgrades : Upgrades -> Html msg
viewUpgrades upgrades =
    Html.ul []
        [ viewUpgrade "Black Hat Bootcamp" upgrades.blackHatBootcamp
        ]


viewUpgrade : String -> Bool -> Html msg
viewUpgrade name hasUpgrade =
    Html.li []
        [ "{NAME}: {YES_NO}"
            |> String.replace "{NAME}" name
            |> String.replace "{YES_NO}"
                (if hasUpgrade then
                    "Yes"

                 else
                    "No"
                )
            |> Html.text
        ]
