module BBVChart exposing (view)

import Constants
import Dict exposing (Dict)
import Html
import Html.Attributes
import List.Extra
import Region
import UI
import VegaLite


view : Dict String (List Int) -> Html.Html msg
view data =
    Html.node "x-vega"
        [ Html.Attributes.property "spec" (spec data) ]
        []


spec : Dict String (List Int) -> VegaLite.Spec
spec rawData =
    let
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

        getRow : Int -> List { month : Int, symbol : String, bbv : Int }
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
                        , bbv = value
                        }
                    )

        rows : List { month : Int, symbol : String, bbv : Int }
        rows =
            List.range 0 Constants.initMonthsLeft
                |> List.concatMap getRow

        data =
            VegaLite.dataFromColumns []
                << VegaLite.dataColumn "month" (VegaLite.nums (List.map (.month >> toFloat) rows))
                << VegaLite.dataColumn "symbol" (VegaLite.strs (List.map .symbol rows))
                << VegaLite.dataColumn "bbv" (VegaLite.nums (List.map (.bbv >> toFloat) rows))

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
