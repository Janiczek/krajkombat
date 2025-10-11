module UI exposing
    ( Sprite(..)
    , Word(..)
    , btn
    , cls
    , col
    , cssVars
    , float
    , handwriting
    , heading
    , link
    , mod
    , modal
    , none
    , pluralize
    , prose
    , row
    , section
    , sprite
    )

import Html exposing (Html)
import Html.Attributes
import Markdown
import PluralRules
import PluralRules.Cz
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
            ++ [ cls "bg-blue-500 text-white px-[1ch] rounded-md w-fit shadow-sm transition-shadow border border-white text-nowrap h-fit translate-y-[-1px]"
               , mod "hover" "bg-blue-400 shadow-md cursor-pointer"
               , mod "active" "bg-blue-500 shadow-inner translate-y-[2px]"
               , mod "disabled" "bg-gray-300 text-gray-500 cursor-not-allowed shadow-none"
               , mod "focus" "outline-2 outline-offset-1 outline-blue-500"
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
        (attrs
            ++ [ cls "border border-gray-200 bg-gray-200/30 rounded-md p-2 shadow-sm h-fit"
               , Html.Attributes.style "backdrop-filter" "blur(6px)"
               ]
        )
        children


float : Float -> String
float n =
    Round.round 2 n


modal : Bool -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
modal isVisible attrs children =
    Html.div
        [ cls
            ("font-mono fixed inset-0 bg-slate-900/50 flex items-center justify-center z-50 modal-backdrop"
                ++ (if isVisible then
                        " show"

                    else
                        ""
                   )
            )
        ]
        [ Html.div
            (attrs ++ [ cls "bg-slate-50 rounded-lg shadow-xl max-w-md mx-4 p-4 modal-content" ])
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
            ( 156, 203 )


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


prose : String -> Html msg
prose content =
    Html.p
        [ cls "text-sm max-w-[80ch] leading-relaxed" ]
        [ Markdown.toHtml [] content ]


handwriting : String -> Html msg
handwriting text =
    Html.span
        [ cls "inline-block text-2xl font-handwriting -rotate-8 text-red-600" ]
        [ Html.text text ]


type Word
    = Chechtak
    | Mesic


pluralWord : Word -> String
pluralWord word =
    case word of
        Chechtak ->
            "Chechták"

        Mesic ->
            "měsíc"


pluralize : Word -> Int -> String
pluralize word count =
    PluralRules.Cz.pluralize pluralRules count (pluralWord word)


pluralRules : PluralRules.Rules
pluralRules =
    PluralRules.fromList
        [ ( pluralWord Mesic
          , [ ( PluralRules.One, "měsíc" )
            , ( PluralRules.Few, "měsíce" ) -- 2..4
            , ( PluralRules.Many, "měsíců" )
            , ( PluralRules.Other, "měsíců" )
            ]
          )
        , ( pluralWord Chechtak
          , [ ( PluralRules.One, "Chechták" )
            , ( PluralRules.Few, "Chechtáky" ) -- 2..4
            , ( PluralRules.Many, "Chechtáků" )
            , ( PluralRules.Other, "Chechtáků" )
            ]
          )
        ]
