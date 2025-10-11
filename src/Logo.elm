module Logo exposing (height, logoMsk, width)

import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes


logoMsk : Svg msg
logoMsk =
    Svg.svg
        [ Svg.Attributes.width (String.fromInt width)
        , Svg.Attributes.height (String.fromInt height)
        , Svg.Attributes.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
        , Svg.Attributes.fill "none"
        , Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg"
        ]
        [ Svg.g
            [ Svg.Attributes.opacity "0.3"
            , Svg.Attributes.clipPath "url(#clip0_3_4)"
            ]
            [ Svg.path
                [ Svg.Attributes.d "M33.69 26.425C32.522 26.88 31.352 27.451 29.249 28.592C24.572 31.101 21.182 37.147 28.313 40.342C35.445 43.423 37.548 47.873 27.962 52.208C20.13 55.743 13.115 60.42 10.778 65.554C9.608 64.641 8.555 63.729 7.503 62.587C0.489998 55.173 -1.148 43.994 1.893 33.499C2.009 32.929 2.709 32.472 3.413 32.244C9.725 29.733 25.507 26.31 33.69 26.425Z"
                , Svg.Attributes.fill "#DC2F34"
                ]
                []
            , Svg.path
                [ Svg.Attributes.d "M66.307 8.628C78.234 21.405 74.843 44.904 58.59 59.392C56.02 61.789 52.046 63.955 49.59 65.553C45.849 67.948 41.173 64.983 45.264 60.532C59.059 45.59 53.565 42.281 36.38 36.806C31.12 34.637 34.043 28.591 37.315 25.853H37.433C37.549 25.738 37.668 25.738 37.668 25.626C37.9 23.685 39.07 22.088 40.475 21.861L41.291 10.566C41.291 10.453 41.408 10.337 41.524 10.223C41.643 10.223 41.759 10.111 41.875 10.111C41.993 10.111 42.225 10.223 42.342 10.223C42.342 10.338 42.461 10.452 42.461 10.566L43.278 21.861C44.914 21.973 46.317 24.026 46.317 26.423V26.536C48.303 26.764 53.448 27.334 57.541 28.703C61.749 29.957 64.088 31.213 65.138 32.01C66.307 28.133 67.127 16.496 57.19 10.11C41.991 2.01 25.39 9.652 13.933 18.437C12.065 19.919 9.843 18.208 11.479 16.611C12.531 15.585 13.349 14.558 15.104 13.19C16.858 11.593 18.61 10.111 20.481 8.856C35.678 -1.752 55.436 -2.666 66.307 8.628Z"
                , Svg.Attributes.fill "#004289"
                ]
                []
            ]
        ]


width : Int
width =
    74


height : Int
height =
    67
