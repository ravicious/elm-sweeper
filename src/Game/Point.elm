module Game.Point exposing (Point, transform)


type alias Point =
    { x : Int
    , y : Int
    }


transform : Int -> Int -> Point -> Point
transform transformX transformY point =
    Point (point.x + transformX) (point.y + transformY)
