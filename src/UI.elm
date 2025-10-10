module UI exposing
    ( btn
    , cls
    , col
    , row
    )

import Html exposing (Html)
import Html.Attributes


cls : String -> Html.Attribute msg
cls =
    Html.Attributes.class


btn : List (Html.Attribute msg) -> String -> Html msg
btn attrs label =
    Html.button
        (attrs ++ [ cls "bg-blue-500 text-white px-[1ch] rounded-md w-fit" ])
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
