module Urgency exposing (Urgency)

import ResourceDelta exposing (ResourceDelta)


type alias Urgency =
    { flavorText : String
    , monthsLeft : Int
    , deltas : List ResourceDelta
    }
