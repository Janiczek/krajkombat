module UI exposing
    ( btn
    , cls
    , col
    , float
    , heading
    , link
    , mod
    , modal
    , none
    , row
    , section
    )

import Html exposing (Html)
import Html.Attributes
import Round


cls : String -> Html.Attribute msg
cls =
    Html.Attributes.class


mod : String -> String -> Html.Attribute msg
mod state classes =
    classes
        |> String.split " "
        |> List.map (\c -> state ++ ":" ++ c)
        |> String.join " "
        |> Html.Attributes.class


heading : String -> Html msg
heading label =
    Html.h4 [ cls "font-bold" ] [ Html.text label ]


btn : List (Html.Attribute msg) -> String -> Html msg
btn attrs label =
    Html.button
        (attrs
            ++ [ cls "bg-blue-500 text-white px-[1ch] rounded-md w-fit shadow-sm transition-shadow border border-white"
               , mod "hover" "bg-blue-400 shadow-md cursor-pointer"
               , mod "active" "bg-blue-500 shadow-inner translate-y-[2px]"
               , mod "disabled" "bg-gray-300 text-gray-500 cursor-not-allowed shadow-none"
               ]
        )
        [ Html.text label ]


col : List (Html.Attribute msg) -> List (Html msg) -> Html msg
col attrs children =
    Html.div
        (attrs ++ [ cls "flex flex-col gap-2" ])
        children


row : List (Html.Attribute msg) -> List (Html msg) -> Html msg
row attrs children =
    Html.div
        (attrs ++ [ cls "flex flex-row gap-2" ])
        children


none : Html msg
none =
    Html.text ""


link : String -> String -> Html msg
link href label =
    Html.a
        [ Html.Attributes.href href
        , cls "text-blue-700 underline"
        , mod "hover" "text-blue-500"
        ]
        [ Html.text label ]


section : List (Html.Attribute msg) -> List (Html msg) -> Html msg
section attrs children =
    col
        (attrs ++ [ cls "border border-gray-200 bg-gray-50 rounded-md p-2 shadow-sm" ])
        children


float : Float -> String
float n =
    Round.round 2 n


modal : List (Html.Attribute msg) -> List (Html msg) -> Html msg
modal attrs children =
    Html.div
        [ cls "font-mono fixed inset-0 bg-slate-900/50 flex items-center justify-center z-50" ]
        [ Html.div
            (attrs ++ [ cls "bg-slate-50 rounded-lg shadow-xl max-w-md w-full mx-4 p-4" ])
            children
        ]
