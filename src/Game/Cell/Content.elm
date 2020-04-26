module Game.Cell.Content exposing (Content(..), toDescription)


type Content
    = Bet Int
    | SurroundingPower Int
    | Power Int
    | Nothing


toDescription : Content -> String
toDescription content =
    case content of
        Bet _ ->
            "bet"

        SurroundingPower _ ->
            "surroundingPower"

        Power _ ->
            "power"

        Nothing ->
            "none"
