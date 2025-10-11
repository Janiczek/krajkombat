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
import Round
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
    | MakeDecision Game.Decision
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
            withGameLoop model <|
                \game ->
                    let
                        ( newGame, newSeed ) =
                            game
                                |> Game.advanceMonth model.randomSeed

                        newModel : Model
                        newModel =
                            { model | randomSeed = newSeed }
                                |> advanceJuice

                        finalModel : Model
                        finalModel =
                            if game.monthsLeft <= 0 then
                                let
                                    results : Game.Results
                                    results =
                                        Game.end newGame
                                in
                                { newModel | gamePhase = Game.GameEnded results }

                            else
                                { newModel | gamePhase = Game.GameLoop newGame }
                    in
                    ( finalModel, Cmd.none )

        BackToMainMenu ->
            ( { model | gamePhase = Game.MainMenu }
            , Cmd.none
            )

        MakeDecision decision ->
            model
                |> updateGameLoop (Game.MakeDecision decision)


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


updateGameLoop : Game.Msg -> Model -> ( Model, Cmd Msg )
updateGameLoop msg model =
    withGameLoop model <|
        \game ->
            ( { model | gamePhase = Game.GameLoop (Game.update msg game) }
            , Cmd.none
            )


withGameLoop : Model -> (Game -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
withGameLoop model f =
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
            [ UI.cls "font-mono p-2 flex justify-center" ]
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
            viewGameEnded juice results


viewMainMenu : List (Html Msg)
viewMainMenu =
    [ UI.col [ UI.cls "items-center justify-center h-[60dvh] min-h-fit" ]
        [ Html.h1 [ UI.cls "font-bold text-2xl" ] [ Html.text title ]
        , Html.h2 [ UI.cls "text-sm" ]
            [ Html.text "Zadavatel: Game Devs Ostrava / Zpracovatel: "
            , UI.link "https://bsky.app/profile/janiczek.cz" "Martin Janiczek"
            ]
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
    [ UI.col [ UI.cls "w-fit" ]
        [ UI.row [ UI.cls "justify-center" ]
            [ viewMonthStats
                juice.advanceMonthButtonText
                game.monthsLeft
            ]
        , UI.row []
            [ UI.section
                [ UI.heading "Duležita rozhodnuti"
                , viewDecisions game.availableDecisions
                ]
            , UI.col []
                [ UI.section
                    [ UI.heading Region.youName
                    , viewYourStats game.you
                    ]
                , UI.section
                    [ UI.heading "KrajKombat - Průběžna tabulka"
                    , viewRanking ranking
                    ]
                ]
            ]
        ]
    ]


viewDecisions : List Game.Decision -> Html Msg
viewDecisions decisions =
    Html.table [ UI.cls "min-w-fit" ]
        (decisions |> List.map viewDecisionRow)


viewDecisionRow : Game.Decision -> Html Msg
viewDecisionRow decision =
    Html.tr []
        [ Html.td [] [ Html.text decision.flavorText ]
        , Html.td [ UI.cls "pl-[2ch]" ]
            [ decision.deltas
                |> List.map describeDelta
                |> String.join ", "
                |> Html.text
            ]
        , Html.td [ UI.cls "pl-[2ch]" ] [ UI.btn [ Html.Events.onClick (MakeDecision decision) ] "To chcu" ]
        ]


describeDelta : Game.ResourceDelta -> String
describeDelta delta =
    let
        plusMinus : number -> String
        plusMinus n =
            if n >= 0 then
                "+"

            else
                ""
    in
    case delta of
        Game.AP n ->
            plusMinus n ++ String.fromInt n ++ " AP"

        Game.APPerMonth n ->
            plusMinus n ++ String.fromInt n ++ " AP/m"

        Game.GREF f ->
            plusMinus f ++ Round.round 2 f ++ " GREF"

        Game.BREF f ->
            plusMinus f ++ Round.round 2 f ++ " BREF"

        Game.BBV n ->
            plusMinus n ++ String.fromInt n ++ " BBV"

        Game.BBVPerMonth n ->
            plusMinus n ++ String.fromInt n ++ " BBV/m"


viewMonthStats : String -> Int -> Html Msg
viewMonthStats advanceMonthButtonText monthsLeft =
    UI.row []
        -- TODO: as there are fewer and fewer months left, add text effects, change colors, add sweating, shaking (on hover?)
        -- TODO: random funny button text
        [ Html.text ("Zbývá měsíců: " ++ String.fromInt monthsLeft)
        , UI.btn [ Html.Events.onClick AdvanceMonth ] advanceMonthButtonText
        ]


viewGameEnded : Juice -> Game.Results -> List (Html Msg)
viewGameEnded juice results =
    case results of
        Game.YouWon data ->
            viewGameEnded_ juice.youWonMessage data

        Game.YouLost data ->
            viewGameEnded_ juice.youLostMessage data

        Game.YouLostByDraw data ->
            viewGameEnded_ juice.youLostByDrawMessage data

        Game.Bug ->
            [ Html.h2 [] [ Html.text "Ups, se to rozbilo" ] ]


viewGameEnded_ : String -> Game.ResultsData -> List (Html Msg)
viewGameEnded_ message resultsData =
    [ UI.col []
        [ Html.h2 [] [ Html.text message ]
        , viewRanking resultsData.ranking
        , UI.btn [ Html.Events.onClick StartGame ]
            -- Valim to zkusit znovu
            "Hrat znovu"
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
    Html.table [ UI.cls "max-w-fit table-auto" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [ UI.cls "text-left" ] [ Html.text "#" ]
                , Html.th [ UI.cls "text-left" ] [ Html.text " " ]
                , Html.th [ UI.cls "text-left" ] [ Html.text "Kraj" ]
                , Html.th [ UI.cls "text-right pl-[2ch]" ] [ Html.text "BrankyBodyVteřiny" ]
                ]
            ]
        , Html.tbody []
            (ranking
                |> List.concatMap
                    (\{ rank, bbv, names } ->
                        names
                            |> List.indexedMap
                                (\i name ->
                                    let
                                        isYou : Bool
                                        isYou =
                                            name == Region.youName

                                        rowCls : Html.Attribute Msg
                                        rowCls =
                                            if isYou then
                                                UI.cls "font-bold bg-yellow-50"

                                            else
                                                UI.cls ""

                                        ordinalText : String
                                        ordinalText =
                                            if i == 0 then
                                                ordinal rank

                                            else
                                                ""
                                    in
                                    Html.tr [ rowCls ]
                                        [ Html.td [] [ Html.text ordinalText ]
                                        , Html.td [ UI.cls "text-xl" ] [ Html.text (medalForRank rank) ]
                                        , Html.td [] [ Html.text name ]
                                        , Html.td [ UI.cls "text-right" ] [ Html.text (String.fromInt bbv) ]
                                        ]
                                )
                    )
            )
        ]


viewYourStats : Region -> Html Msg
viewYourStats region =
    UI.col []
        [ viewResources region.resources
        , viewUpgrades region.upgrades
        ]


viewResources : Resources -> Html msg
viewResources stats =
    UI.col []
        [ UI.heading "Kasa:"
        , Html.table [ UI.cls "min-w-[20ch] table-auto border-spacing-y-2" ]
            [ Html.tbody []
                [ Html.tr []
                    [ Html.td [ UI.cls "pr-2" ] [ Html.text "💰 Chechtaky:" ]
                    , Html.td [ UI.cls "text-right pl-[2ch]" ] [ Html.text (String.fromInt stats.ap) ]
                    ]
                , Html.tr []
                    [ Html.td [ UI.cls "pr-2" ] [ Html.text "💶 Chechtaky/měsíc:" ]
                    , Html.td [ UI.cls "text-right" ] [ Html.text (String.fromInt stats.apPerMonth) ]
                    ]
                , Html.tr []
                    [ Html.td [ UI.cls "pr-2" ] [ Html.text "📈 Šance na dobru nahodu:" ]
                    , Html.td [ UI.cls "text-right" ] [ Html.text (UI.float stats.gref) ]
                    ]
                , Html.tr []
                    [ Html.td [ UI.cls "pr-2" ] [ Html.text "📉 Šance na špatnu nahodu:" ]
                    , Html.td [ UI.cls "text-right" ] [ Html.text (UI.float stats.bref) ]
                    ]
                , Html.tr []
                    [ Html.td [ UI.cls "pr-2" ] [ Html.text "⚽ BrankyBodyVteřiny:" ]
                    , Html.td [ UI.cls "text-right" ] [ Html.text (String.fromInt stats.bbv) ]
                    ]
                , Html.tr []
                    [ Html.td [ UI.cls "pr-2" ] [ Html.text "🏒 BrankyBodyVteřiny/měsíc:" ]
                    , Html.td [ UI.cls "text-right" ] [ Html.text (String.fromInt stats.bbvPerMonth) ]
                    ]
                ]
            ]
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
            [ UI.heading "Upgrades:"
            , Html.ul []
                (purchasedUpgrades
                    |> List.map (\name -> Html.li [] [ Html.text name ])
                )
            ]
