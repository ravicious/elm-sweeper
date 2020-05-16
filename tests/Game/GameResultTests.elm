module Game.GameResultTests exposing (..)

import Expect
import Fuzz
import Game.GameResult as GameResult exposing (GameResult, Placing(..), Timing)
import Game.Variant
import Random
import Test exposing (..)
import Time


generateGameResult : Int -> GameResult
generateGameResult gameDurationInSeconds =
    let
        timing =
            generateTiming gameDurationInSeconds
    in
    { variant = Game.Variant.Normal
    , name = String.fromInt gameDurationInSeconds ++ "s game"
    , seed = 0
    , startedAt = timing.startedAt
    , endedAt = timing.endedAt
    }


generateTiming : Int -> Timing
generateTiming gameDurationInSeconds =
    { startedAt = Time.millisToPosix 0
    , endedAt = Time.millisToPosix <| gameDurationInSeconds * 1000
    }


calculatePlacingTests : Test
calculatePlacingTests =
    describe "calculatePlacingTests"
        [ test "returns NoPreviousResults if there are no other results" <|
            \() ->
                GameResult.calculatePlacing [] (generateTiming 1)
                    |> Expect.equal NoPreviousResults
        , test "returns Place 1 after placing above the first result" <|
            \() ->
                let
                    existingResults =
                        [ generateGameResult 2 ]

                    newTiming =
                        generateTiming 1
                in
                GameResult.calculatePlacing existingResults newTiming
                    |> Expect.equal (Place 1)
        , test "returns Place 2 after placing below the first result" <|
            \() ->
                let
                    existingResults =
                        [ generateGameResult 2 ]

                    newTiming =
                        generateTiming 3
                in
                GameResult.calculatePlacing existingResults newTiming
                    |> Expect.equal (Place 2)
        , test "counts results with equal time as a single place" <|
            \() ->
                let
                    existingResults =
                        [ generateGameResult 1
                        , generateGameResult 2
                        , generateGameResult 2
                        , generateGameResult 3
                        ]

                    newTiming =
                        generateTiming 2
                in
                GameResult.calculatePlacing existingResults newTiming
                    |> Expect.equal (Tie 2)
        , test "next place after dead heat placements doesn't follow natural numbering, but rather counts as if dead head placements were sequential placements" <|
            \() ->
                let
                    existingResults =
                        [ generateGameResult 2
                        , generateGameResult 1
                        , generateGameResult 2
                        , generateGameResult 2
                        ]

                    newTiming =
                        generateTiming 3
                in
                GameResult.calculatePlacing existingResults newTiming
                    |> Expect.equal (Place 5)
        , fuzz3 (Fuzz.intRange 0 1000000) (Fuzz.intRange 0 999) (Fuzz.intRange 0 999) "rounds results to seconds" <|
            \baseSeconds millis1 millis2 ->
                let
                    existingResults =
                        [ { variant = Game.Variant.Normal
                          , name = "Foo"
                          , seed = 0
                          , startedAt = Time.millisToPosix 0
                          , endedAt = Time.millisToPosix <| baseSeconds * 1000 + millis1
                          }
                        ]

                    newTiming =
                        { startedAt = Time.millisToPosix 0
                        , endedAt = Time.millisToPosix <| baseSeconds * 1000 + millis2
                        }
                in
                GameResult.calculatePlacing existingResults newTiming
                    |> Expect.equal (Tie 1)
        ]
