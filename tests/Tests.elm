module Tests exposing (..)

import Expect
import Fuzz
import Game.Board as Board
import Random
import Shrink
import Test exposing (..)


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


{-| Creates a fuzzer which generates board dimensions and then generates a valid index
for those dimensions.
-}
boardDimensionsAndCellIndexFuzzer : Fuzz.Fuzzer ( ( Int, Int ), Int )
boardDimensionsAndCellIndexFuzzer =
    let
        boardDimensionGenerator =
            Random.int 1 30

        generator =
            Random.map2 Tuple.pair boardDimensionGenerator boardDimensionGenerator
                |> Random.andThen
                    (\( x, y ) ->
                        Random.map2 Tuple.pair
                            (Random.constant ( x, y ))
                            (Random.int 0 (x * y - 1))
                    )
    in
    Fuzz.custom generator Shrink.noShrink
