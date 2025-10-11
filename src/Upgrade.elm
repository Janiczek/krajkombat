module Upgrade exposing (Upgrade(..), cost, description, name, procButtonLabel)

import ResourceDelta exposing (ResourceDelta(..))


type Upgrade
    = -- Once in 3 months you can hack the national KrajKombat database and steal half of the BBV of one region.
      BlackHatBootcamp


name : Upgrade -> String
name upgrade =
    case upgrade of
        BlackHatBootcamp ->
            "Black Hat Bootcamp"


description : Upgrade -> String
description upgrade =
    case upgrade of
        BlackHatBootcamp ->
            """
            SPŠE chce zorganizovat intenzivni Black Hat Bootcamp pro tvou
            sekretařku. Z te se stane nadupany hacker a každe tři měsice se
            mužeš nabourat do narodni KrajKombat databaze a ukrast někomu pulku
            bodu. Myslim že si Maňa zasluži přidat.
            """


cost : Upgrade -> List ResourceDelta
cost upgrade =
    case upgrade of
        BlackHatBootcamp ->
            [ AP -150, APPerMonth -5 ]


procButtonLabel : Upgrade -> String
procButtonLabel upgrade =
    case upgrade of
        BlackHatBootcamp ->
            "Maňa zas se tam naburej"
