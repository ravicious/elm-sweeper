module Assets exposing (monsterSrc)

import Html exposing (..)
import Html.Attributes exposing (..)


monsterSrc : Int -> String
monsterSrc power =
    let
        monsterName =
            case power of
                1 ->
                    "SlimeA"

                2 ->
                    "SnakeA"

                3 ->
                    "MushroomA"

                4 ->
                    "TentacleD"

                5 ->
                    "SkeletonD"

                6 ->
                    "EyeBallA"

                7 ->
                    "GhastA"

                8 ->
                    "HeadA"

                9 ->
                    "ReaperA"

                _ ->
                    "unknown"
    in
    "assets/monsters/" ++ monsterName ++ "-normal.png"
