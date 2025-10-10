module Juice exposing (Juice, generator)

import Random exposing (Generator)
import Random.Extra


type alias Juice =
    { finishIntroButtonText : String
    , advanceMonthButtonText : String
    }


generator : Generator Juice
generator =
    Random.constant
        (\finishIntroButtonText advanceMonthButtonText ->
            { finishIntroButtonText = finishIntroButtonText
            , advanceMonthButtonText = advanceMonthButtonText
            }
        )
        |> Random.Extra.andMap
            (Random.uniform
                firstFinishIntroButtonText
                otherFinishIntroButtonTexts
            )
        |> Random.Extra.andMap
            (Random.uniform
                firstAdvanceMonthButtonText
                otherAdvanceMonthButtonTexts
            )


firstFinishIntroButtonText : String
firstFinishIntroButtonText =
    "Drž už zobak"


otherFinishIntroButtonTexts : List String
otherFinishIntroButtonTexts =
    [ "Si moc dluhy"
    , "No a co jako"
    ]


firstAdvanceMonthButtonText : String
firstAdvanceMonthButtonText =
    "Nojo furt"


otherAdvanceMonthButtonTexts : List String
otherAdvanceMonthButtonTexts =
    [ "Popojedem"
    , "Nevim, další!"
    ]
