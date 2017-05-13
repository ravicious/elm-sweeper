module Game.Variant exposing (Identifier(..), Variant, get)


type Identifier
    = SixteenByThirty


type alias Variant =
    { rows : Int
    , columns : Int
    }


get : Identifier -> Variant
get identifier =
    case identifier of
        SixteenByThirty ->
            { rows = 16
            , columns = 30
            }
