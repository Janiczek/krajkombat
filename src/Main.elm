module Main exposing (Flags, Model, Msg, main)

import AssocSet
import Browser
import Game exposing (Game)
import Html exposing (Html)
import Html.Events
import Juice exposing (Juice)
import Random
import Ranking exposing (Ranking)
import Region exposing (Region)
import Resource exposing (Resources)
import UI
import Upgrade exposing (Upgrade)


type alias Flags =
    { randomSeed : Int }


type alias Model =
    { gamePhase : Game.Phase
    , juice : Juice
    , randomSeed : Random.Seed
    }


type Msg
    = -- MainMenu
      StartGame
      -- Intro
    | FinishIntro
      -- GameLoop
    | AdvanceMonth
      -- GameEnded
    | BackToMainMenu


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        seed : Random.Seed
        seed =
            Random.initialSeed flags.randomSeed

        ( juice, newSeed ) =
            Random.step Juice.generator seed
    in
    ( { gamePhase = Game.MainMenu
      , juice = juice
      , randomSeed = newSeed
      }
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
            let
                ( game, newSeed ) =
                    Random.step Game.gameInitGenerator model.randomSeed
            in
            ( { model
                | gamePhase = Game.GameLoop game
                , randomSeed = newSeed
              }
            , Cmd.none
            )

        AdvanceMonth ->
            updateGameLoop model <|
                \game ->
                    if game.monthsLeft <= 0 then
                        let
                            ( newGame, newSeed ) =
                                game
                                    |> Game.advanceMonth model.randomSeed

                            results =
                                newGame
                                    |> Game.end
                        in
                        ( { model
                            | gamePhase = Game.GameEnded results
                            , randomSeed = newSeed
                          }
                        , Cmd.none
                        )

                    else
                        let
                            ( newGame, newSeed ) =
                                game
                                    |> Game.advanceMonth model.randomSeed
                        in
                        ( { model
                            | gamePhase = Game.GameLoop newGame
                            , randomSeed = newSeed
                          }
                            |> advanceJuice
                        , Cmd.none
                        )

        BackToMainMenu ->
            ( { model | gamePhase = Game.MainMenu }
            , Cmd.none
            )


advanceJuice : Model -> Model
advanceJuice model =
    let
        ( juice, newSeed ) =
            Random.step Juice.generator model.randomSeed
    in
    { model
        | juice = juice
        , randomSeed = newSeed
    }


updateGameLoop : Model -> (Game -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
updateGameLoop model f =
    case model.gamePhase of
        Game.GameLoop game ->
            f game

        Game.MainMenu ->
            ( model, Cmd.none )

        Game.Intro ->
            ( model, Cmd.none )

        Game.GameEnded _ ->
            ( model, Cmd.none )


title : String
title =
    "KrajKombat"


view : Model -> Browser.Document Msg
view model =
    { title = title
    , body =
        [ Html.div
            [ UI.cls "font-mono p-2" ]
            (viewGamePhase model.juice model.gamePhase)
        ]
    }


viewGamePhase : Juice -> Game.Phase -> List (Html Msg)
viewGamePhase juice phase =
    case phase of
        Game.MainMenu ->
            viewMainMenu

        Game.Intro ->
            viewIntro juice

        Game.GameLoop game ->
            viewGameLoop juice game

        Game.GameEnded results ->
            viewGameEnded results


viewMainMenu : List (Html Msg)
viewMainMenu =
    [ UI.col [ UI.cls "items-center justify-center h-[60dvh] min-h-fit" ]
        [ Html.h1 [ UI.cls "font-bold text-2xl" ] [ Html.text title ]
        , Html.h2 [ UI.cls "text-sm" ] [ Html.text "Zadavatel: Game Devs Ostrava / Zpracovatel: Martin Janiczek" ]
        , UI.btn [ Html.Events.onClick StartGame ] "Cože"
        ]
    ]


viewIntro : Juice -> List (Html Msg)
viewIntro juice =
    [ UI.col []
        [ Html.text "TODO intro - story, exposition, blablabla"
        , UI.btn
            [ Html.Events.onClick FinishIntro ]
            -- "Seš moc dlouhej"
            juice.finishIntroButtonText
        ]
    ]


viewGameLoop : Juice -> Game -> List (Html Msg)
viewGameLoop juice game =
    let
        ranking : Ranking
        ranking =
            Ranking.rank { you = game.you, others = game.others }
    in
    [ UI.col []
        [ viewMonthStats
            juice.advanceMonthButtonText
            game.monthsLeft
        , UI.row []
            [ UI.col []
                [ Html.h3 [] [ Html.text Region.youName ]
                , viewYourStats game.you
                ]
            , UI.col []
                [ Html.h3 [] [ Html.text "Ranking" ]
                , viewRanking ranking
                ]
            ]
        ]
    ]


viewMonthStats : String -> Int -> Html Msg
viewMonthStats advanceMonthButtonText monthsLeft =
    UI.row []
        -- TODO: as there are fewer and fewer months left, add text effects, change colors, add sweating, shaking (on hover?)
        -- TODO: random funny button text
        [ Html.text ("Zbývá měsíců: " ++ String.fromInt monthsLeft)
        , UI.btn [ Html.Events.onClick AdvanceMonth ] advanceMonthButtonText
        ]


viewGameEnded : Game.Results -> List (Html Msg)
viewGameEnded results =
    case results of
        Game.YouWon data ->
            viewGameEnded_ "Dobřes je zrušil!" data

        Game.YouLost data ->
            viewGameEnded_ "Prohrals kamo!" data

        Game.YouLostByDraw data ->
            viewGameEnded_ "Prohrals kamo bo plichta neplati!" data

        Game.Bug ->
            [ Html.h2 [] [ Html.text "Ups, se to rozbilo" ] ]


viewGameEnded_ : String -> Game.ResultsData -> List (Html Msg)
viewGameEnded_ message resultsData =
    [ UI.col []
        [ Html.h2 [] [ Html.text message ]
        , viewRanking resultsData.ranking
        , UI.btn [ Html.Events.onClick StartGame ] "Hrat znovu"
        , UI.btn [ Html.Events.onClick BackToMainMenu ] "Do menu"
        ]
    ]


medalForRank : Int -> String
medalForRank rank =
    case rank of
        0 ->
            "🥇"

        1 ->
            "🥈"

        2 ->
            "🥉"

        _ ->
            ""


ordinal : Int -> String
ordinal n =
    String.fromInt (n + 1) ++ "."


viewRanking : Ranking -> Html Msg
viewRanking ranking =
    Html.table [ UI.cls "min-w-[20ch] table-auto border-spacing-y-2" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [ UI.cls "text-left" ] [ Html.text "#" ]
                , Html.th [ UI.cls "text-left" ] [ Html.text " " ]
                , Html.th [ UI.cls "text-left" ] [ Html.text "Kraj" ]
                , Html.th [ UI.cls "text-right" ] [ Html.text "BrankyBodyVteřiny" ]
                ]
            ]
        , Html.tbody []
            (ranking
                |> List.concatMap
                    (\{ rank, bbv, names } ->
                        let
                            isYouGroup =
                                List.member Region.youName names

                            rowCls =
                                if isYouGroup then
                                    UI.cls "font-bold bg-yellow-50"

                                else
                                    UI.cls ""
                        in
                        [ Html.tr [ rowCls ]
                            [ Html.td [] [ Html.text (ordinal rank) ]
                            , Html.td [ UI.cls "text-xl" ] [ Html.text (medalForRank rank) ]
                            , Html.td []
                                [ names
                                    |> String.join ", "
                                    |> Html.text
                                ]
                            , Html.td [ UI.cls "text-right" ] [ Html.text (String.fromInt bbv) ]
                            ]
                        ]
                    )
            )
        ]


viewYourStats : Region -> Html Msg
viewYourStats region =
    UI.col []
        [ Html.h4 [] [ Html.text "Stats:" ]
        , viewResources region.resources
        , viewUpgrades region.upgrades
        ]


viewResources : Resources -> Html msg
viewResources stats =
    Html.ul []
        [ Html.li [] [ Html.text ("AP: " ++ String.fromInt stats.ap) ]
        , Html.li [] [ Html.text ("AP/měsíc: " ++ String.fromInt stats.apPerMonth) ]
        , Html.li [] [ Html.text ("GREF: " ++ String.fromFloat stats.gref) ]
        , Html.li [] [ Html.text ("BREF: " ++ String.fromFloat stats.bref) ]
        , Html.li [] [ Html.text ("BBV: " ++ String.fromInt stats.bbv) ]
        , Html.li [] [ Html.text ("BBV/měsíc: " ++ String.fromInt stats.bbvPerMonth) ]
        ]


viewUpgrades : AssocSet.Set Upgrade -> Html msg
viewUpgrades upgrades =
    let
        purchasedUpgrades : List String
        purchasedUpgrades =
            upgrades
                |> AssocSet.toList
                |> List.map (\upgrade -> Upgrade.name upgrade)
    in
    if List.isEmpty purchasedUpgrades then
        UI.none

    else
        UI.col []
            [ Html.h4 [] [ Html.text "Upgrades:" ]
            , Html.ul []
                (purchasedUpgrades
                    |> List.map (\name -> Html.li [] [ Html.text name ])
                )
            ]
