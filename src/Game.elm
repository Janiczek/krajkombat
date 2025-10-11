module Game exposing
    ( Decision
    , DecisionType(..)
    , Game
    , Msg(..)
    , Phase(..)
    , ResourceDelta(..)
    , Results(..)
    , ResultsData
    , advanceMonth
    , end
    , gameInitGenerator
    , update
    )

import Dict
import Logic
import Random exposing (Generator)
import Random.Extra
import Ranking exposing (Ranking)
import Region exposing (Region)


type Msg
    = MakeDecision Decision


type Phase
    = MainMenu
    | Intro
    | GameLoop Game
    | GameEnded Results


type Results
    = YouWon ResultsData
    | YouLost ResultsData
    | YouLostByDraw ResultsData
    | Bug


type alias ResultsData =
    { you : Region
    , others : List Region
    , ranking : Ranking
    }


type alias Game =
    { you : Region
    , others : List Region
    , monthsLeft : Int
    , availableDecisions : List Decision
    }


type DecisionType
    = Investment
    | InvestmentLongTerm
    | Prevention


type alias Decision =
    { type_ : DecisionType
    , flavorText : String
    , deltas : List ResourceDelta
    }


type ResourceDelta
    = AP Int
    | APPerMonth Int
    | GREF Float
    | BREF Float
    | BBV Int
    | BBVPerMonth Int


decisionsGenerator : Generator (List Decision)
decisionsGenerator =
    Random.list availableDecisionsPerMonth decisionGenerator
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
add min max resource =
    Random.int min max
        |> Random.map resource


addF : Float -> Float -> (Float -> ResourceDelta) -> Generator ResourceDelta
addF min max resource =
    Random.float min max
        |> Random.map resource


sub : Int -> Int -> (Int -> ResourceDelta) -> Generator ResourceDelta
sub min max resource =
    add min max resource
        |> Random.map negateResourceDelta


subF : Float -> Float -> (Float -> ResourceDelta) -> Generator ResourceDelta
subF min max resource =
    addF min max resource
        |> Random.map negateResourceDelta


negateResourceDelta : ResourceDelta -> ResourceDelta
negateResourceDelta resourceDelta =
    case resourceDelta of
        AP x ->
            AP -x

        APPerMonth x ->
            APPerMonth -x

        GREF x ->
            GREF -x

        BREF x ->
            BREF -x

        BBV x ->
            BBV -x

        BBVPerMonth x ->
            BBVPerMonth -x


decisionContentGenerator : DecisionType -> Generator ( String, List ResourceDelta )
decisionContentGenerator type_ =
    (case type_ of
        Investment ->
            Random.uniform
                ( "Nová lepší sekretářka", [ sub 5 20 AP, add 2 5 APPerMonth ] )
                [ ( "TODO", [ sub 15 30 AP, add 4 10 APPerMonth ] )
                ]

        InvestmentLongTerm ->
            Random.uniform
                ( "TODO", [ sub 5 10 APPerMonth, addF 0.01 0.05 GREF ] )
                []

        Prevention ->
            Random.uniform
                ( "TODO", [ sub 5 10 APPerMonth, subF 0.01 0.03 BREF ] )
                [ ( "TODO", [ sub 10 20 AP, subF 0.01 0.02 BREF ] )
                ]
    )
        |> Random.andThen
            (\( flavorText, deltaGenerators ) ->
                Random.Extra.sequence deltaGenerators
                    |> Random.map (\deltas -> ( flavorText, deltas ))
            )


otherRegionsCount : Int
otherRegionsCount =
    3


gameInitGenerator : Generator Game
gameInitGenerator =
    Random.constant
        (\you others availableDecisions ->
            { you = you
            , others = others
            , monthsLeft = 24
            , availableDecisions = availableDecisions
            }
        )
        |> Random.Extra.andMap (Region.generator Region.youName)
        |> Random.Extra.andMap (Region.listGenerator otherRegionsCount)
        |> Random.Extra.andMap decisionsGenerator


keepUniqueNames : List Decision -> List Decision
keepUniqueNames decisions =
    decisions
        |> List.map (\decision -> ( decision.flavorText, decision ))
        |> Dict.fromList
        |> Dict.values


availableDecisionsPerMonth : Int
availableDecisionsPerMonth =
    10


advanceMonth : Random.Seed -> Game -> ( Game, Random.Seed )
advanceMonth seed game =
    let
        newYou : Region
        newYou =
            Logic.advanceMonthForRegion game.you

        newOthers : List Region
        newOthers =
            game.others
                |> List.map Logic.advanceMonthForRegion

        ( newAvailableDecisions, seed1 ) =
            Random.step decisionsGenerator seed
    in
    ( { game
        | you = newYou
        , others = newOthers
        , monthsLeft = game.monthsLeft - 1
        , availableDecisions = newAvailableDecisions
      }
    , seed1
    )


end : Game -> Results
end game =
    let
        ranking : Ranking
        ranking =
            Ranking.rank game
    in
    case ranking of
        fst :: _ ->
            let
                resultsData : ResultsData
                resultsData =
                    { you = game.you
                    , others = game.others
                    , ranking = ranking
                    }
            in
            if fst.names == [ Region.youName ] then
                YouWon resultsData

            else if List.member Region.youName fst.names then
                YouLostByDraw resultsData

            else
                YouLost resultsData

        [] ->
            Bug


update : Msg -> Game -> Game
update msg game =
    case msg of
        MakeDecision decision ->
            Debug.todo "make decision"
