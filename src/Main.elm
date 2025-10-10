module Main exposing (Flags, Model, Msg, main)

import Browser
import Game exposing (Game)
import Html exposing (Html)
import Html.Events
import Juice exposing (Juice)
import Random
import Ranking exposing (Ranking)
import Region exposing (Region)
import Stats exposing (Stats)
import UI
import Upgrades exposing (Upgrades)


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
                            results : Game.Results
                            results =
                                game
                                    |> Game.advanceMonth
                                    |> Game.end
                        in
                        ( { model | gamePhase = Game.GameEnded results }
                        , Cmd.none
                        )

                    else
                        let
                            newGame : Game
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

        Game.GameEnded _ ->
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
    [ UI.col []
        [ Html.h1 [] [ Html.text title ]
        , UI.btn
            [ Html.Events.onClick StartGame ]
            "Cože"
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
    [ UI.col []
        [ viewMonthStats
            juice.advanceMonthButtonText
            game.monthsLeft
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
            [ UI.col []
                [ Html.h2 [] [ Html.text "Dobřes je zrušil!" ]
                , viewRanking data.ranking
                ]
            ]

        Game.YouLost data ->
            [ UI.col []
                [ Html.h2 [] [ Html.text "Prohrals kamo!" ]
                , viewRanking data.ranking
                ]
            ]

        Game.YouLostByDraw data ->
            [ UI.col []
                [ Html.h2 [] [ Html.text "Prohrals kamo bo plichta neplati!" ]
                , viewRanking data.ranking
                ]
            ]

        Game.Bug ->
            [ Html.h2 [] [ Html.text "Ups, se to rozbilo" ] ]


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
                                    |> List.intersperse ", "
                                    |> String.concat
                                    |> Html.text
                                ]
                            , Html.td [ UI.cls "text-right" ] [ Html.text (String.fromInt bbv) ]
                            ]
                        ]
                    )
            )
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
