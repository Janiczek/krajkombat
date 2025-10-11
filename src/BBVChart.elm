module BBVChart exposing (view)

import Constants
import Dict exposing (Dict)
import Html
import Html.Attributes
import List.Extra
import Region
import UI
import VegaLite


view : Int -> Int -> Int -> Bool -> Dict String (List Int) -> Html.Html msg
view yourBbvPerMonth yourBbv currentMonthsLeft isFinalScreen data =
    Html.node "x-vega"
        [ Html.Attributes.property "spec"
            (spec
                yourBbvPerMonth
                yourBbv
                currentMonthsLeft
                isFinalScreen
                data
            )
        ]
        []


type alias Row =
    { month : Int
    , symbol : String
    , bbv : Int
    , predicted : Bool
    }


spec : Int -> Int -> Int -> Bool -> Dict String (List Int) -> VegaLite.Spec
spec yourBbvPerMonth yourBbv currentMonthsLeft isFinalScreen rawData =
    let
        currentMonth =
            Constants.initMonthsLeft - currentMonthsLeft

        ( names, seriesList ) =
            rawData
                |> Dict.map (\_ xs -> List.reverse xs)
                |> Dict.toList
                |> List.sortBy
                    (\( name, _ ) ->
                        if name == Region.youName then
                            0

                        else
                            1
                    )
                |> List.unzip

        getRow : Int -> List Row
        getRow i =
            let
                rowValues : List ( String, Int )
                rowValues =
                    List.map2 Tuple.pair names seriesList
                        |> List.filterMap
                            (\( name, xs ) ->
                                xs
                                    |> List.Extra.getAt i
                                    |> Maybe.map (\value -> ( name, value ))
                            )
            in
            rowValues
                |> List.map
                    (\( name, value ) ->
                        { month = i
                        , symbol = name
                        , bbv =
                            if
                                (name == Region.youName)
                                    && (i == currentMonth)
                                    && not isFinalScreen
                            then
                                yourBbv

                            else
                                value
                        , predicted = False
                        }
                    )

        rowsWithoutPredictions : List Row
        rowsWithoutPredictions =
            List.range 0 (Constants.initMonthsLeft + 1)
                |> List.concatMap getRow

        predictionRows : List Row
        predictionRows =
            List.range 0 currentMonthsLeft
                |> List.map
                    (\i ->
                        { month = currentMonth + i
                        , symbol = Region.youName
                        , bbv = yourBbv + yourBbvPerMonth * i
                        , predicted = True
                        }
                    )

        rows : List Row
        rows =
            rowsWithoutPredictions ++ predictionRows

        data =
            VegaLite.dataFromColumns []
                << VegaLite.dataColumn "month" (VegaLite.nums (List.map (.month >> toFloat) rows))
                << VegaLite.dataColumn "symbol" (VegaLite.strs (List.map .symbol rows))
                << VegaLite.dataColumn "bbv" (VegaLite.nums (List.map (.bbv >> toFloat) rows))
                << VegaLite.dataColumn "predicted" (VegaLite.boos (List.map .predicted rows))

        enc =
            VegaLite.encoding
                << VegaLite.position VegaLite.X
                    [ VegaLite.pName "month"
                    , VegaLite.pOrdinal
                    , VegaLite.pAxis
                        [ VegaLite.axTitle (UI.pluralize UI.Mesic 1)
                        ]
                    ]
                << VegaLite.position VegaLite.Y
                    [ VegaLite.pName "bbv"
                    , VegaLite.pQuant
                    ]
                << VegaLite.strokeDash
                    [ VegaLite.mName "predicted"
                    , VegaLite.mNominal
                    ]
                << VegaLite.color
                    [ VegaLite.mName "symbol"
                    , VegaLite.mScale
                        [ VegaLite.scDomain (VegaLite.doStrs names)
                        , VegaLite.scRange
                            (VegaLite.raStrs
                                (names
                                    |> List.map
                                        (\name ->
                                            if name == Region.youName then
                                                "#ff0000"

                                            else
                                                "#aaaaff55"
                                        )
                                )
                            )
                        ]
                    ]
    in
    VegaLite.toVegaLite
        [ data []
        , enc []
        , VegaLite.line []
        ]
