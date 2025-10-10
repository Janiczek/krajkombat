module Upgrade exposing (Upgrade(..), name)


type Upgrade
    = BlackHatBootcamp


name : Upgrade -> String
name upgrade =
    case upgrade of
        BlackHatBootcamp ->
            -- TODO ostravstina
            "Black Hat Bootcamp"
