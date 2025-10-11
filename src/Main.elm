module Main exposing (Flags, Model, Msg, main)

import AssocSet
import Browser
import Decision exposing (Decision)
import Game exposing (Game)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Juice exposing (Juice)
import List.Extra
import Random
import Ranking exposing (Ranking)
import Region exposing (Region)
import Resource exposing (Resources)
import ResourceDelta exposing (ResourceDelta(..))
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
    | MakeDecision Decision
    | DiscardDecision Decision
      -- Random Events Modal
    | ApplyNextRandomEvent
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
                            Random.step (Game.advanceMonth game) model.randomSeed

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

        DiscardDecision decision ->
            model
                |> updateGameLoop (Game.DiscardDecision decision)

        ApplyNextRandomEvent ->
            model
                |> updateGameLoop Game.ApplyNextRandomEvent


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
        Html.div
            [ UI.cls "font-mono p-2 pt-8 flex justify-center" ]
            (viewGamePhase model.juice model.gamePhase)
            :: viewRandomEventsModal model
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
    [ UI.col [ UI.cls "items-center" ]
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
    [ UI.col []
        [ UI.row [ UI.cls "justify-center" ]
            [ viewMonthStats
                juice.advanceMonthButtonText
                game.monthsLeft
            ]
        , UI.row []
            [ UI.section [ UI.cls "w-[60ch]" ]
                [ UI.heading "Duležita rozhodnuti"
                , if List.isEmpty game.you.availableDecisions then
                    viewNoDecisions

                  else
                    viewDecisions game.you.resources game.you.availableDecisions
                ]
            , UI.col [ UI.cls "w-[40ch]" ]
                [ UI.section []
                    [ UI.heading ("Tvuj tym: " ++ Region.youName ++ "!")
                    , viewYourStats game.you
                    ]
                , UI.section []
                    [ UI.heading "KrajKombat - Průběžna tabulka"
                    , viewRanking ranking
                    ]
                ]
            ]
        ]
    ]


viewNoDecisions : Html Msg
viewNoDecisions =
    Html.text "Už neni o čem!"


viewDecisions : Resources -> List Decision -> Html Msg
viewDecisions yourResources decisions =
    let
        groups : List ( Decision.Type, List Decision )
        groups =
            decisions
                |> List.Extra.gatherEqualsBy .type_
                |> List.map (\( x, xs ) -> ( x.type_, x :: xs ))
    in
    UI.col []
        (groups
            |> List.map (viewDecisionsOfType yourResources)
        )


viewDecisionsOfType : Resources -> ( Decision.Type, List Decision ) -> Html Msg
viewDecisionsOfType yourResources ( decisionType, decisions ) =
    UI.col []
        [ UI.heading (Decision.typeLabel decisionType)
        , Html.table []
            (decisions |> List.map (viewDecisionRow yourResources))
        ]


viewDecisionRow : Resources -> Decision -> Html Msg
viewDecisionRow yourResources decision =
    let
        canApply : Bool
        canApply =
            Resource.canApplyDeltas yourResources decision.deltas

        flavorTextNode : Html msg
        flavorTextNode =
            if canApply then
                Html.text decision.flavorText

            else
                Html.span [ UI.cls "line-through text-gray-400" ] [ Html.text decision.flavorText ]

        deltaClass : String
        deltaClass =
            if canApply then
                ""

            else
                "text-gray-400"
    in
    Html.tr
        [ UI.mod "hover"
            (if canApply then
                "bg-blue-50"

             else
                "bg-red-50"
            )
        ]
        [ Html.td [ UI.cls "py-2" ]
            [ Html.div []
                [ Html.div [] [ flavorTextNode ]
                , Html.ul [ UI.cls ("text-sm " ++ deltaClass) ]
                    (decision.deltas
                        |> List.map (\delta -> Html.li [] [ viewDelta { canApply = canApply } delta ])
                    )
                ]
            ]
        , Html.td [ UI.cls "pl-[2ch] text-right text-nowrap" ]
            [ UI.btn
                [ Html.Events.onClick (MakeDecision decision)
                , Html.Attributes.disabled (not canApply)

                -- TODO tooltip saying what you're missing
                ]
                "To chcu"
            , UI.btn
                [ Html.Events.onClick (DiscardDecision decision)
                , Html.Attributes.disabled (not canApply)
                , UI.cls "bg-red-500"
                , UI.mod "hover" "bg-red-400"
                , UI.mod "active" "bg-red-500"

                -- TODO tooltip saying what you're missing
                ]
                "Zapomeň"
            ]
        ]


viewMonthStats : String -> Int -> Html Msg
viewMonthStats advanceMonthButtonText monthsLeft =
    UI.row []
        -- TODO: as there are fewer and fewer months left, add text effects, change colors, add sweating, shaking (on hover?)
        -- TODO: random funny button text
        [ Html.text ("Zbývá měsíců: " ++ String.fromInt monthsLeft)
        , UI.btn
            [ Html.Events.onClick AdvanceMonth ]
            (advanceMonthButtonText ++ " →")
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
    Html.table [ UI.cls "table-auto" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [ UI.cls "text-left" ] [ Html.text "#" ]
                , Html.th [ UI.cls "text-left" ] [ Html.text " " ]
                , Html.th [ UI.cls "text-left pl-[2ch]" ] [ Html.text "Kraj" ]
                , Html.th [ UI.cls "text-right pl-[2ch]" ] [ Html.text "BBV" ]
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
                                    Html.tr
                                        [ rowCls
                                        , UI.mod "hover" "bg-blue-50"
                                        ]
                                        [ Html.td [] [ Html.text ordinalText ]
                                        , Html.td [ UI.cls "pl-[2ch] text-xl" ] [ Html.text (medalForRank rank) ]
                                        , Html.td [ UI.cls "pl-[2ch]" ] [ Html.text name ]
                                        , Html.td [ UI.cls "pl-[2ch] text-right" ] [ Html.text (String.fromInt bbv) ]
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
    let
        statRow : String -> String -> Html msg
        statRow label value =
            Html.tr [ UI.mod "hover" "bg-blue-50" ]
                [ Html.td [ UI.cls "py-1 pr-2 text-base" ] [ Html.text label ]
                , Html.td [ UI.cls "text-right align-top text-base" ] [ Html.text value ]
                ]
    in
    UI.col []
        [ UI.heading "Kasa:"
        , Html.table [ UI.cls "min-w-[20ch] table-auto border-spacing-y-2" ]
            [ Html.tbody []
                [ statRow "💰 Chechtaky" (String.fromInt stats.ap)
                , statRow "💶 Chechtaky/měsíc" (String.fromInt stats.apPerMonth)
                , statRow "📈 Dobre nahody/měsíc" (UI.float stats.gref)
                , statRow "📉 Špatne nahody/měsíc" (UI.float stats.bref)
                , statRow "⚽ BrankyBodyVteřiny" (String.fromInt stats.bbv)
                , statRow "🏒 BrankyBodyVteřiny/měsíc" (String.fromInt stats.bbvPerMonth)
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


type Nature
    = Good
    | Bad


flipNature : Nature -> Nature
flipNature n =
    case n of
        Good ->
            Bad

        Bad ->
            Good


nature : ResourceDelta -> Nature
nature delta =
    let
        f : number -> Nature
        f n =
            if n >= 0 then
                Good

            else
                Bad
    in
    case delta of
        AP n ->
            f n

        APPerMonth n ->
            f n

        GREF n ->
            f n

        BREF n ->
            f n |> flipNature

        BBV n ->
            f n

        BBVPerMonth n ->
            f n


viewDelta : { canApply : Bool } -> ResourceDelta -> Html msg
viewDelta { canApply } delta =
    let
        plusMinus : number -> String
        plusMinus n =
            if n >= 0 then
                "+"

            else
                "-"

        content : String
        content =
            case delta of
                AP n ->
                    plusMinus n ++ String.fromInt (abs n) ++ " Chechtaky"

                APPerMonth n ->
                    plusMinus n ++ String.fromInt (abs n) ++ " Chechtaky/měsíc"

                GREF n ->
                    plusMinus n ++ UI.float (abs n) ++ " Dobre nahody/měsíc"

                BREF n ->
                    plusMinus n ++ UI.float (abs n) ++ " Špatne nahody/měsíc"

                BBV n ->
                    plusMinus n ++ String.fromInt (abs n) ++ " BrankyBodyVteřiny"

                BBVPerMonth n ->
                    plusMinus n ++ String.fromInt (abs n) ++ " BrankyBodyVteřiny/měsíc"

        colorClass : String
        colorClass =
            if canApply then
                case nature delta of
                    Good ->
                        "text-green-700"

                    Bad ->
                        "text-red-700"

            else
                "text-gray-400"
    in
    Html.span
        [ UI.cls colorClass ]
        [ Html.text content ]


viewRandomEventsModal : Model -> List (Html Msg)
viewRandomEventsModal model =
    case model.gamePhase of
        Game.MainMenu ->
            []

        Game.Intro ->
            []

        Game.GameEnded _ ->
            []

        Game.GameLoop game ->
            case game.you.randomEvents of
                [] ->
                    []

                currentEvent :: _ ->
                    let
                        headingText : String
                        headingText =
                            -- TODO juice
                            if currentEvent.isGood then
                                "Dobra nahoda"

                            else
                                "Špatna nahoda (a kurde)"

                        buttonText : String
                        buttonText =
                            if currentEvent.isGood then
                                "Tuž dobre no ni?"

                            else
                                "Oukej no"
                    in
                    [ UI.modal []
                        [ UI.col [ UI.cls "gap-4" ]
                            [ UI.heading headingText
                            , Html.div []
                                [ Html.text currentEvent.flavorText ]
                            , if not (List.isEmpty currentEvent.deltas) then
                                Html.ul [ UI.cls "text-sm list-disc ml-6" ]
                                    (currentEvent.deltas
                                        |> List.map (\delta -> Html.li [] [ viewDelta { canApply = True } delta ])
                                    )

                              else
                                UI.none
                            , UI.row [ UI.cls "justify-between items-center" ]
                                [ UI.btn
                                    [ Html.Events.onClick ApplyNextRandomEvent ]
                                    buttonText
                                ]
                            ]
                        ]
                    ]
