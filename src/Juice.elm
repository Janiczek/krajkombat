module Juice exposing (Juice, generator)

import Random exposing (Generator)
import Random.Extra


type alias Juice =
    { finishIntroButtonText : String
    , advanceMonthButtonText : String
    , youWonMessage : String
    , youLostMessage : String
    , youLostByDrawMessage : String
    , tryAgainMessage : String
    }


uniform : ( x, List x ) -> Generator x
uniform ( x, xs ) =
    Random.uniform x xs


generator : Generator Juice
generator =
    Random.constant
        (\finishIntroButtonText advanceMonthButtonText youWonMessage youLostMessage youLostByDrawMessage tryAgainMessage ->
            { finishIntroButtonText = finishIntroButtonText
            , advanceMonthButtonText = advanceMonthButtonText
            , youWonMessage = youWonMessage
            , youLostMessage = youLostMessage
            , youLostByDrawMessage = youLostByDrawMessage
            , tryAgainMessage = tryAgainMessage
            }
        )
        |> Random.Extra.andMap (uniform finishIntroButtonTexts)
        |> Random.Extra.andMap (uniform advanceMonthButtonTexts)
        |> Random.Extra.andMap (uniform youWonMessageTexts)
        |> Random.Extra.andMap (uniform youLostMessageTexts)
        |> Random.Extra.andMap (uniform youLostByDrawMessageTexts)
        |> Random.Extra.andMap (uniform tryAgainMessageTexts)


finishIntroButtonTexts : ( String, List String )
finishIntroButtonTexts =
    ( "Drž už zobak"
    , [ "Si moc dluhy"
      , "Už drž pec prosimtě"
      , "No dobre no"
      , "Tak dem na to"
      , "Tak už pome"
      ]
    )


advanceMonthButtonTexts : ( String, List String )
advanceMonthButtonTexts =
    ( "Nojo furt"
    , [ "Popojedem"
      , "Nevim, dalši!"
      , "Sypej tam dalši"
      ]
    )


youWonMessageTexts : ( String, List String )
youWonMessageTexts =
    ( "Fajně jak cyp!"
    , [ "To byla haluz jak cyp!"
      , "Jiřik to byl fajront jak cyp!"
      , "Dobřes je zrušil!"
      ]
    )


youLostMessageTexts : ( String, List String )
youLostMessageTexts =
    ( "Tak si chuj?"
    , [ "Ty si kus cypa!"
      , "Prohrals kamo!"
      , "Šecko zdupane!"
      ]
    )


youLostByDrawMessageTexts : ( String, List String )
youLostByDrawMessageTexts =
    ( "Jak se musiš dělit tak chuj s tym! Přiště lepi!"
    , [ "Prohrals kamo bo plichta je nanic!"
      , "Ale lepši by bylo aby oni byli druzi, nu ni?"
      ]
    )


tryAgainMessageTexts : ( String, List String )
tryAgainMessageTexts =
    ( "Valim to zkusit znovu"
    , [ "Hrat znovu"
      , "Poď do mě, tě sundam kolikrat budu chtit"
      , "Dem znovu, ti rozhodim sandál"
      ]
    )
