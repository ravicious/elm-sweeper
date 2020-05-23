module Game.GameResultTests exposing (..)

import Expect
import Fuzz
import Game.GameResult as GameResult exposing (GameResult, Placing(..))
import Game.Variant
import List.Extra
import Random
import Test exposing (..)
import Time


generateGameResultInMillis : Int -> GameResult
generateGameResultInMillis gameDurationInMillis =
    { variant = Game.Variant.Normal
    , name = String.fromInt gameDurationInMillis ++ "s game"
    , seed = 0
    , startedAt = Time.millisToPosix 0
    , endedAt = Time.millisToPosix gameDurationInMillis
    }


updateName : String -> GameResult -> GameResult
updateName name gameResult =
    { gameResult | name = name }


generateGameResultInSeconds : Int -> GameResult
generateGameResultInSeconds =
    (*) 1000 >> generateGameResultInMillis


compareTests : Test
compareTests =
    describe "compare"
        [ fuzz3 (Fuzz.intRange 0 1000000) (Fuzz.intRange 0 999) (Fuzz.intRange 0 999) "rounds results to seconds" <|
            \baseSeconds millis1 millis2 ->
                let
                    resultA =
                        generateGameResultInMillis <| baseSeconds * 1000 + millis1

                    resultB =
                        generateGameResultInMillis <| baseSeconds * 1000 + millis2
                in
                GameResult.compare resultA resultB
                    |> Expect.equal EQ
        ]


toLeaderboardTests : Test
toLeaderboardTests =
    describe "toLeaderboard"
        [ test "returns Place 1 for the first result" <|
            \() ->
                let
                    targetResult =
                        generateGameResultInSeconds 3

                    otherResult =
                        generateGameResultInSeconds 5
                in
                GameResult.toLeaderboard [ otherResult, targetResult ]
                    |> List.head
                    |> Expect.equal (Just <| ( Place 1, targetResult ))
        , test "returns Place 2 for the second result" <|
            \() ->
                let
                    targetResult =
                        generateGameResultInSeconds 5

                    otherResult =
                        generateGameResultInSeconds 3
                in
                GameResult.toLeaderboard [ otherResult, targetResult ]
                    |> List.Extra.last
                    |> Expect.equal (Just <| ( Place 2, targetResult ))
        , test "returns Tie 2 for second place tie" <|
            \() ->
                let
                    targetResult =
                        generateGameResultInSeconds 3

                    otherResults =
                        [ generateGameResultInSeconds 1
                        , generateGameResultInSeconds 3
                        , generateGameResultInSeconds 3
                        , generateGameResultInSeconds 5
                        ]
                in
                GameResult.toLeaderboard (targetResult :: otherResults)
                    |> List.Extra.find (Tuple.second >> (==) targetResult)
                    |> Maybe.map Tuple.first
                    |> Expect.equal (Just <| Tie 2)
        , test "next place after three second place ties has 5th place" <|
            \() ->
                let
                    targetResult =
                        generateGameResultInSeconds 5

                    otherResults =
                        [ generateGameResultInSeconds 1
                        , generateGameResultInSeconds 3
                        , generateGameResultInSeconds 3
                        , generateGameResultInSeconds 3
                        ]
                in
                GameResult.toLeaderboard (targetResult :: otherResults)
                    |> List.Extra.last
                    |> Expect.equal (Just <| ( Place 5, targetResult ))
        , test "Most recently achieved tie is always the last in the list of ties" <|
            \() ->
                let
                    targetResult =
                        generateGameResultInSeconds 3
                            |> updateName "targetResult"

                    otherResults =
                        [ generateGameResultInSeconds 1
                        , generateGameResultInSeconds 3
                        , generateGameResultInSeconds 3
                        , generateGameResultInSeconds 5
                        ]
                in
                GameResult.toLeaderboard (List.append otherResults [ targetResult ])
                    |> List.Extra.getAt 3
                    |> Expect.equal (Just <| ( Tie 2, targetResult ))
        ]
