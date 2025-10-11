module Upgrade exposing
    ( BlackHatData
    , Upgrade(..)
    , advanceBlackHat
    , blackHatAmount
    , blackHatButtonLabel
    , chance
    , cost
    , description
    , initBlackHat
    , name
    , resetBlackHat
    )

import ResourceDelta exposing (ResourceDelta(..))


type Upgrade
    = -- Once in 3 months you can hack the national KrajKombat database and steal half of the BBV of one region.
      BlackHatBootcamp
    | DataAnalytics


name : Upgrade -> String
name upgrade =
    case upgrade of
        BlackHatBootcamp ->
            "Black Hat Bootcamp"

        DataAnalytics ->
            "Datovy analytik ze Slezske univerzity"


description : Upgrade -> String
description upgrade =
    case upgrade of
        BlackHatBootcamp ->
            """
SPŠE chce zorganizovat intenzivni Black Hat Bootcamp pro tvou sekretařku. Z te
se stane nadupany hacker a každych pět měsíců se mužeš nabourat do narodni
KrajKombat databaze a ukrast někomu pulku bodu. Myslim že si Maňa zasluži
přidat.
"""

        DataAnalytics ->
            """
Profesor Bartl ti chce dohodit do officu jednoho doktoranta Aplikovane
matematiky. Jeho ukol bude donest ti každy měsic hezky graf jak se vyvijela
tabulka KrajKombatu (jak na tom byly a jsou všechny kraje ohledně BBV). Možna to
k něčemu bude?
"""


cost : Upgrade -> List ResourceDelta
cost upgrade =
    case upgrade of
        BlackHatBootcamp ->
            [ AP -400, APPerMonth -20 ]

        DataAnalytics ->
            [ APPerMonth -10 ]


blackHatButtonLabel : String
blackHatButtonLabel =
    "Maňa do toho"


chance : Upgrade -> Float
chance upgrade =
    case upgrade of
        BlackHatBootcamp ->
            0.4

        DataAnalytics ->
            0.9


type alias BlackHatData =
    { monthsUntilAvailable : Int }


initBlackHat : BlackHatData
initBlackHat =
    { monthsUntilAvailable = 0 }


resetBlackHat : BlackHatData
resetBlackHat =
    { monthsUntilAvailable = 5 }


advanceBlackHat : BlackHatData -> BlackHatData
advanceBlackHat data =
    { data | monthsUntilAvailable = max 0 (data.monthsUntilAvailable - 1) }


blackHatAmount : Int -> Int
blackHatAmount bbv =
    bbv // 2
