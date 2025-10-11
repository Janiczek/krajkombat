module UI exposing
    ( Sprite(..)
    , btn
    , cls
    , col
    , cssVars
    , float
    , heading
    , link
    , mod
    , modal
    , none
    , row
    , section
    , sprite
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


type Sprite
    = CharNormal
    | CharWin
    | CharDraw
    | CharLose
    | CharExplain
    | CharConfused


spriteUrl : Sprite -> String
spriteUrl s =
    case s of
        CharNormal ->
            "char_normal.png"

        CharWin ->
            "char_win.png"

        CharDraw ->
            "char_draw.png"

        CharLose ->
            "char_lose.png"

        CharExplain ->
            "char_explain.png"

        CharConfused ->
            "char_confused.png"


spriteSize : Sprite -> ( Int, Int )
spriteSize s =
    case s of
        CharNormal ->
            ( 156, 203 )

        CharWin ->
            ( 166, 216 )

        CharDraw ->
            ( 135, 203 )

        CharLose ->
            ( 198, 231 )

        CharExplain ->
            ( 190, 205 )

        CharConfused ->
            ( 142, 197 )


sprite : Sprite -> Html msg
sprite s =
    let
        ( w, h ) =
            spriteSize s
    in
    Html.img
        [ Html.Attributes.width (w // 2)
        , Html.Attributes.height (h // 2)
        , Html.Attributes.src (spriteUrl s)
        ]
        []


cssVars : List ( String, String ) -> Html.Attribute msg
cssVars vars =
    vars
        |> List.map (\( name, value ) -> "--" ++ name ++ ": " ++ value)
        |> String.join ";"
        |> Html.Attributes.attribute "style"
