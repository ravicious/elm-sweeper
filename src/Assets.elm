module Assets exposing (monsterSrc)

import Html exposing (..)
import Html.Attributes exposing (..)


monsterSrc : Int -> String
monsterSrc power =
    "assets/monsters/" ++ String.fromInt power ++ ".png"
