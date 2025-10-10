module Main exposing (Flags, Model, Msg, main)

import Browser
import Game exposing (Game)
import Html exposing (Html)
import Html.Events
import Random
import Region exposing (Region)
import Stats exposing (Stats)
import UI
import Upgrades exposing (Upgrades)


type alias Flags =
    ()


type alias Model =
    { gamePhase : Game.Phase
    }


type Msg
    = -- MainMenu
      StartGame
      -- Intro
    | FinishIntro
    | GameLoopInitialized Game
      -- GameLoop
    | AdvanceMonth


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
            ( { model | gamePhase = Game.Intro }
            , Cmd.none
            )

        FinishIntro ->
            ( model
            , Random.generate GameLoopInitialized Game.gameInitGenerator
            )

        GameLoopInitialized game ->
            ( { model | gamePhase = Game.GameLoop game }
            , Cmd.none
            )

        AdvanceMonth ->
            updateGameLoop model <|
                \game ->
                    if game.monthsLeft <= 0 then
                        let
                            results =
                                game
                                    |> Game.advanceMonth
                                    |> Game.end
                        in
                        case results of
                            Err results_ ->
                                ( { model | gamePhase = Game.GameOver results_ }
                                , Cmd.none
                                )

                            Ok results_ ->
                                ( { model | gamePhase = Game.GameWon results_ }
                                , Cmd.none
                                )

                    else
                        let
                            newGame =
                                game
                                    |> Game.advanceMonth
                        in
                        ( { model | gamePhase = Game.GameLoop newGame }
                        , Cmd.none
                        )


updateGameLoop : Model -> (Game -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
updateGameLoop model f =
    case model.gamePhase of
        Game.GameLoop game ->
            f game

        Game.MainMenu ->
            ( model, Cmd.none )

        Game.Intro ->
            ( model, Cmd.none )

        Game.GameOver _ ->
            ( model, Cmd.none )

        Game.GameWon _ ->
            ( model, Cmd.none )


title : String
title =
    "KrajKombat: MSK Edition"


view : Model -> Browser.Document Msg
view model =
    { title = title
    , body =
        [ Html.div
            [ UI.cls "font-mono p-2" ]
            (viewGamePhase model.gamePhase)
        ]
    }


viewGamePhase : Game.Phase -> List (Html Msg)
viewGamePhase phase =
    case phase of
        Game.MainMenu ->
            viewMainMenu

        Game.Intro ->
            viewIntro

        Game.GameLoop game ->
            viewGameLoop game

        Game.GameOver results ->
            viewGameOver results

        Game.GameWon results ->
            viewGameWon results


viewMainMenu : List (Html Msg)
viewMainMenu =
    [ UI.col []
        [ Html.h1 [] [ Html.text title ]
        , UI.btn
            [ Html.Events.onClick StartGame ]
            "Start Game"
        ]
    ]


viewIntro : List (Html Msg)
viewIntro =
    [ UI.col []
        [ Html.text "TODO intro - story, exposition, blablabla"
        , UI.btn
            [ Html.Events.onClick FinishIntro ]
            "Finish Intro"
        ]
    ]


viewGameLoop : Game -> List (Html Msg)
viewGameLoop game =
    [ UI.col []
        [ Html.h2 [] [ Html.text "GameLoop Phase" ]
        , UI.btn [ Html.Events.onClick AdvanceMonth ] "Advance Month"
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
    ]


viewGameOver : Game.Results -> List (Html Msg)
viewGameOver results =
    [ UI.col []
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
    ]


viewGameWon : Game.Results -> List (Html Msg)
viewGameWon results =
    [ UI.col []
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
    ]


viewRegion : Region -> Html Msg
viewRegion region =
    UI.col []
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
        (Upgrades.all upgrades
            |> List.map viewUpgrade
        )


viewUpgrade : ( String, Bool ) -> Html msg
viewUpgrade ( name, hasUpgrade ) =
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
