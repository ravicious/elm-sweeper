module Game.Variant exposing (Identifier(..), Variant, get)


type Identifier
    = SixteenByThirty


type alias Variant =
    { rows : Int
    , columns : Int
    , cellConfiguration : List ( Int, Int )
    }


get : Identifier -> Variant
get identifier =
    case identifier of
        SixteenByThirty ->
            { rows = 16
            , columns = 30
            , cellConfiguration =
                [ ( 1, 33 ), ( 2, 27 ), ( 3, 20 ), ( 4, 13 ), ( 5, 6 ) ]
                -- The configuration has been taken from the original game.
            }
