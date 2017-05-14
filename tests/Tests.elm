module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz


-- Our imports

import Game.Board as Board


all : Test
all =
    Test.concat [ cellOperationsTests ]


boardDimensionFuzzer : Fuzz.Fuzzer Int
boardDimensionFuzzer =
    Fuzz.intRange 1 30


boardDimensionsFuzzer : Fuzz.Fuzzer ( Int, Int )
boardDimensionsFuzzer =
    Fuzz.map2 (,) boardDimensionFuzzer boardDimensionFuzzer


{-| Creates a fuzzer which generates board dimensions and then generates a valid index
for those dimensions.
-}
boardDimensionsAndCellIndexFuzzer : Fuzz.Fuzzer ( ( Int, Int ), Int )
boardDimensionsAndCellIndexFuzzer =
    boardDimensionsFuzzer
        |> Fuzz.andThen
            (\( x, y ) ->
                Fuzz.map2 (,)
                    (Fuzz.constant ( x, y ))
                    (Fuzz.intRange 0 (x * y - 1))
            )


cellOperationsTests : Test
cellOperationsTests =
    describe "Cell operations tests"
        [ fuzz
            boardDimensionsAndCellIndexFuzzer
            "Transforming an index to a point and then back to an index returns the same index"
          <|
            \( ( rows, columns ), index ) ->
                let
                    maybeBoard =
                        Board.initWithZeroPower rows columns
                in
                    case maybeBoard of
                        Just board ->
                            Expect.equal
                                (Just index)
                                (Board.indexToPoint board index
                                    |> Maybe.andThen (Board.pointToIndex board)
                                )

                        Nothing ->
                            Expect.fail "Board.initWithZeroPower returned Nothing"
        ]
