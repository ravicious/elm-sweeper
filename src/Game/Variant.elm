module Game.Variant exposing (Identifier(..), Variant, get)

import Game.ExpProgression as ExpProgression


type Identifier
    = Normal


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
