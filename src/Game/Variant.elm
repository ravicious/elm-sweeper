module Game.Variant exposing (Identifier(..), Variant, get)

import Game.ExpProgression as ExpProgression


type Identifier
    = Normal
    | Huge


type alias Variant =
    { rows : Int
    , columns : Int
    , minPower : Int
    , maxPower : Int
    , cellConfiguration : List ( Int, Int )
    , expProgression : ExpProgression.ExpProgression
    , initialPlayerHp : Int
    }


get : Identifier -> Variant
get identifier =
    case identifier of
        Normal ->
            normal

        Huge ->
            huge


normal : Variant
normal =
    { rows = 16
    , columns = 30
    , minPower = 1
    , maxPower = 5
    , initialPlayerHp = 10
    , cellConfiguration =
        -- The configuration has been taken from the original game.
        [ ( 1, 33 ), ( 2, 27 ), ( 3, 20 ), ( 4, 13 ), ( 5, 6 ) ]
    , expProgression =
        ExpProgression.init [ ( 1, 10 ), ( 2, 50 ), ( 3, 167 ), ( 4, 271 ) ]
    }


huge : Variant
huge =
    { rows = 25
    , columns = 50
    , minPower = 1
    , maxPower = 9
    , initialPlayerHp = 30
    , cellConfiguration =
        [ ( 1, 52 ), ( 2, 46 ), ( 3, 40 ), ( 4, 36 ), ( 5, 30 ), ( 6, 24 ), ( 7, 18 ), ( 8, 13 ), ( 9, 1 ) ]
    , expProgression =
        ExpProgression.init [ ( 1, 10 ), ( 2, 90 ), ( 3, 202 ), ( 4, 400 ), ( 5, 1072 ), ( 6, 1840 ), ( 7, 2992 ), ( 8, 4656 ) ]
    }
