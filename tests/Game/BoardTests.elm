module Game.BoardTests exposing (..)

import Dict
import Expect
import Fuzz
import Game.Board as Board
import Game.Cell as Cell
import Game.ExpProgression
import Game.Variant
import Random
import Shrink
import Tagged
import Test exposing (..)


oneByOneVariant : Game.Variant.Variant
oneByOneVariant =
    { rows = 1
    , columns = 1
    , minPower = 1
    , maxPower = 1
    , initialPlayerHp = 10
    , cellConfiguration = [ ( 1, 1 ) ]
    , expProgression = Game.ExpProgression.init []
    }


initTests : Test
initTests =
    describe "init"
        [ -- This is in case a change in our or 3rd party code changes how boards are generated.
          -- We want the same seed to always generate the same board, even if we update Elm or
          -- libraries.
          test "doesn't change how board is randomized over time" <|
            \_ ->
                let
                    expectedIndexToPower =
                        [ ( 0, 0 ), ( 1, 0 ), ( 2, 2 ), ( 3, 0 ), ( 4, 0 ), ( 5, 0 ), ( 6, 3 ), ( 7, 0 ), ( 8, 0 ), ( 9, 0 ), ( 10, 2 ), ( 11, 0 ), ( 12, 0 ), ( 13, 0 ), ( 14, 1 ), ( 15, 1 ), ( 16, 0 ), ( 17, 4 ), ( 18, 5 ), ( 19, 0 ), ( 20, 0 ), ( 21, 0 ), ( 22, 0 ), ( 23, 0 ), ( 24, 1 ), ( 25, 2 ), ( 26, 0 ), ( 27, 0 ), ( 28, 0 ), ( 29, 4 ), ( 30, 0 ), ( 31, 5 ), ( 32, 0 ), ( 33, 0 ), ( 34, 1 ), ( 35, 0 ), ( 36, 0 ), ( 37, 0 ), ( 38, 0 ), ( 39, 2 ), ( 40, 0 ), ( 41, 2 ), ( 42, 0 ), ( 43, 4 ), ( 44, 3 ), ( 45, 0 ), ( 46, 0 ), ( 47, 0 ), ( 48, 0 ), ( 49, 0 ), ( 50, 0 ), ( 51, 0 ), ( 52, 0 ), ( 53, 0 ), ( 54, 0 ), ( 55, 0 ), ( 56, 0 ), ( 57, 0 ), ( 58, 0 ), ( 59, 0 ), ( 60, 0 ), ( 61, 0 ), ( 62, 0 ), ( 63, 0 ), ( 64, 0 ), ( 65, 0 ), ( 66, 0 ), ( 67, 0 ), ( 68, 0 ), ( 69, 0 ), ( 70, 2 ), ( 71, 0 ), ( 72, 0 ), ( 73, 0 ), ( 74, 0 ), ( 75, 3 ), ( 76, 0 ), ( 77, 3 ), ( 78, 0 ), ( 79, 4 ), ( 80, 0 ), ( 81, 0 ), ( 82, 0 ), ( 83, 2 ), ( 84, 0 ), ( 85, 0 ), ( 86, 0 ), ( 87, 1 ), ( 88, 0 ), ( 89, 0 ), ( 90, 0 ), ( 91, 0 ), ( 92, 0 ), ( 93, 1 ), ( 94, 0 ), ( 95, 0 ), ( 96, 0 ), ( 97, 0 ), ( 98, 0 ), ( 99, 0 ), ( 100, 4 ), ( 101, 0 ), ( 102, 0 ), ( 103, 0 ), ( 104, 0 ), ( 105, 0 ), ( 106, 0 ), ( 107, 0 ), ( 108, 0 ), ( 109, 1 ), ( 110, 0 ), ( 111, 0 ), ( 112, 0 ), ( 113, 0 ), ( 114, 0 ), ( 115, 0 ), ( 116, 0 ), ( 117, 0 ), ( 118, 0 ), ( 119, 0 ), ( 120, 0 ), ( 121, 0 ), ( 122, 0 ), ( 123, 0 ), ( 124, 0 ), ( 125, 0 ), ( 126, 0 ), ( 127, 1 ), ( 128, 0 ), ( 129, 0 ), ( 130, 5 ), ( 131, 0 ), ( 132, 0 ), ( 133, 0 ), ( 134, 2 ), ( 135, 0 ), ( 136, 0 ), ( 137, 0 ), ( 138, 0 ), ( 139, 0 ), ( 140, 0 ), ( 141, 1 ), ( 142, 2 ), ( 143, 0 ), ( 144, 0 ), ( 145, 1 ), ( 146, 1 ), ( 147, 0 ), ( 148, 0 ), ( 149, 0 ), ( 150, 0 ), ( 151, 0 ), ( 152, 0 ), ( 153, 0 ), ( 154, 0 ), ( 155, 0 ), ( 156, 1 ), ( 157, 0 ), ( 158, 2 ), ( 159, 0 ), ( 160, 0 ), ( 161, 2 ), ( 162, 0 ), ( 163, 1 ), ( 164, 0 ), ( 165, 0 ), ( 166, 0 ), ( 167, 3 ), ( 168, 0 ), ( 169, 0 ), ( 170, 0 ), ( 171, 0 ), ( 172, 0 ), ( 173, 0 ), ( 174, 0 ), ( 175, 0 ), ( 176, 0 ), ( 177, 0 ), ( 178, 0 ), ( 179, 0 ), ( 180, 0 ), ( 181, 3 ), ( 182, 0 ), ( 183, 0 ), ( 184, 2 ), ( 185, 0 ), ( 186, 0 ), ( 187, 0 ), ( 188, 0 ), ( 189, 0 ), ( 190, 0 ), ( 191, 0 ), ( 192, 1 ), ( 193, 1 ), ( 194, 0 ), ( 195, 1 ), ( 196, 0 ), ( 197, 0 ), ( 198, 0 ), ( 199, 0 ), ( 200, 0 ), ( 201, 0 ), ( 202, 0 ), ( 203, 0 ), ( 204, 0 ), ( 205, 0 ), ( 206, 2 ), ( 207, 0 ), ( 208, 0 ), ( 209, 0 ), ( 210, 0 ), ( 211, 0 ), ( 212, 0 ), ( 213, 0 ), ( 214, 0 ), ( 215, 0 ), ( 216, 0 ), ( 217, 0 ), ( 218, 0 ), ( 219, 0 ), ( 220, 0 ), ( 221, 0 ), ( 222, 0 ), ( 223, 0 ), ( 224, 0 ), ( 225, 3 ), ( 226, 0 ), ( 227, 0 ), ( 228, 0 ), ( 229, 0 ), ( 230, 0 ), ( 231, 0 ), ( 232, 0 ), ( 233, 2 ), ( 234, 0 ), ( 235, 0 ), ( 236, 0 ), ( 237, 0 ), ( 238, 0 ), ( 239, 0 ), ( 240, 0 ), ( 241, 0 ), ( 242, 0 ), ( 243, 0 ), ( 244, 0 ), ( 245, 0 ), ( 246, 0 ), ( 247, 1 ), ( 248, 0 ), ( 249, 3 ), ( 250, 0 ), ( 251, 0 ), ( 252, 1 ), ( 253, 1 ), ( 254, 0 ), ( 255, 0 ), ( 256, 0 ), ( 257, 0 ), ( 258, 0 ), ( 259, 0 ), ( 260, 0 ), ( 261, 0 ), ( 262, 0 ), ( 263, 1 ), ( 264, 0 ), ( 265, 0 ), ( 266, 0 ), ( 267, 0 ), ( 268, 1 ), ( 269, 0 ), ( 270, 0 ), ( 271, 0 ), ( 272, 0 ), ( 273, 0 ), ( 274, 0 ), ( 275, 5 ), ( 276, 4 ), ( 277, 0 ), ( 278, 3 ), ( 279, 0 ), ( 280, 0 ), ( 281, 1 ), ( 282, 0 ), ( 283, 0 ), ( 284, 0 ), ( 285, 0 ), ( 286, 2 ), ( 287, 0 ), ( 288, 0 ), ( 289, 1 ), ( 290, 0 ), ( 291, 0 ), ( 292, 0 ), ( 293, 0 ), ( 294, 0 ), ( 295, 0 ), ( 296, 4 ), ( 297, 1 ), ( 298, 2 ), ( 299, 1 ), ( 300, 0 ), ( 301, 0 ), ( 302, 0 ), ( 303, 3 ), ( 304, 2 ), ( 305, 0 ), ( 306, 0 ), ( 307, 2 ), ( 308, 0 ), ( 309, 0 ), ( 310, 0 ), ( 311, 0 ), ( 312, 0 ), ( 313, 0 ), ( 314, 0 ), ( 315, 2 ), ( 316, 0 ), ( 317, 4 ), ( 318, 0 ), ( 319, 0 ), ( 320, 2 ), ( 321, 0 ), ( 322, 0 ), ( 323, 0 ), ( 324, 1 ), ( 325, 0 ), ( 326, 0 ), ( 327, 0 ), ( 328, 0 ), ( 329, 0 ), ( 330, 0 ), ( 331, 0 ), ( 332, 0 ), ( 333, 0 ), ( 334, 0 ), ( 335, 0 ), ( 336, 0 ), ( 337, 3 ), ( 338, 0 ), ( 339, 0 ), ( 340, 0 ), ( 341, 3 ), ( 342, 0 ), ( 343, 0 ), ( 344, 2 ), ( 345, 2 ), ( 346, 0 ), ( 347, 2 ), ( 348, 0 ), ( 349, 0 ), ( 350, 0 ), ( 351, 3 ), ( 352, 0 ), ( 353, 0 ), ( 354, 0 ), ( 355, 0 ), ( 356, 3 ), ( 357, 0 ), ( 358, 0 ), ( 359, 0 ), ( 360, 0 ), ( 361, 0 ), ( 362, 1 ), ( 363, 0 ), ( 364, 0 ), ( 365, 0 ), ( 366, 0 ), ( 367, 0 ), ( 368, 0 ), ( 369, 0 ), ( 370, 3 ), ( 371, 0 ), ( 372, 0 ), ( 373, 0 ), ( 374, 0 ), ( 375, 0 ), ( 376, 0 ), ( 377, 0 ), ( 378, 0 ), ( 379, 0 ), ( 380, 0 ), ( 381, 0 ), ( 382, 0 ), ( 383, 0 ), ( 384, 0 ), ( 385, 0 ), ( 386, 0 ), ( 387, 0 ), ( 388, 3 ), ( 389, 0 ), ( 390, 0 ), ( 391, 2 ), ( 392, 0 ), ( 393, 0 ), ( 394, 0 ), ( 395, 0 ), ( 396, 0 ), ( 397, 0 ), ( 398, 0 ), ( 399, 0 ), ( 400, 0 ), ( 401, 0 ), ( 402, 0 ), ( 403, 0 ), ( 404, 0 ), ( 405, 0 ), ( 406, 0 ), ( 407, 0 ), ( 408, 0 ), ( 409, 0 ), ( 410, 0 ), ( 411, 3 ), ( 412, 0 ), ( 413, 0 ), ( 414, 1 ), ( 415, 3 ), ( 416, 1 ), ( 417, 0 ), ( 418, 0 ), ( 419, 0 ), ( 420, 0 ), ( 421, 2 ), ( 422, 0 ), ( 423, 0 ), ( 424, 0 ), ( 425, 5 ), ( 426, 0 ), ( 427, 1 ), ( 428, 0 ), ( 429, 0 ), ( 430, 4 ), ( 431, 0 ), ( 432, 0 ), ( 433, 4 ), ( 434, 0 ), ( 435, 0 ), ( 436, 0 ), ( 437, 0 ), ( 438, 0 ), ( 439, 0 ), ( 440, 0 ), ( 441, 0 ), ( 442, 0 ), ( 443, 0 ), ( 444, 0 ), ( 445, 5 ), ( 446, 0 ), ( 447, 4 ), ( 448, 0 ), ( 449, 4 ), ( 450, 0 ), ( 451, 0 ), ( 452, 0 ), ( 453, 0 ), ( 454, 3 ), ( 455, 0 ), ( 456, 2 ), ( 457, 0 ), ( 458, 0 ), ( 459, 0 ), ( 460, 0 ), ( 461, 0 ), ( 462, 0 ), ( 463, 0 ), ( 464, 0 ), ( 465, 0 ), ( 466, 0 ), ( 467, 3 ), ( 468, 0 ), ( 469, 0 ), ( 470, 0 ), ( 471, 0 ), ( 472, 4 ), ( 473, 1 ), ( 474, 0 ), ( 475, 0 ), ( 476, 1 ), ( 477, 2 ), ( 478, 1 ), ( 479, 0 ) ]

                    actualIndexToPower =
                        Board.init (Game.Variant.get Game.Variant.Normal) (Random.initialSeed 1)
                            |> .cells
                            |> Dict.map (\_ cell -> cell |> Cell.getPower |> Tagged.untag)
                            |> Dict.toList
                in
                Expect.equal expectedIndexToPower actualIndexToPower
        ]


toMonsterSummaryTests : Test
toMonsterSummaryTests =
    describe "toMonsterSummary"
        [ test "called on a freshly initialized board is equal to cell configuration" <|
            \() ->
                let
                    variant =
                        Game.Variant.get Game.Variant.Normal

                    expected =
                        Dict.fromList variant.cellConfiguration

                    actual =
                        Board.init variant (Random.initialSeed 0)
                            |> Board.toMonsterSummary
                in
                Expect.equal expected actual
        , test "includes a power level even if all monsters of that level have been revealed" <|
            \() ->
                let
                    expected =
                        Dict.fromList [ ( 1, 0 ) ]

                    actual =
                        Board.init oneByOneVariant (Random.initialSeed 0)
                            |> Board.touchCell 0
                            |> Board.toMonsterSummary
                in
                Expect.equal expected actual
        ]


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
