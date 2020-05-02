module Game.BoardTests exposing (..)

import Dict
import Expect
import Game.Board as Board
import Game.Cell as Cell
import Game.ExpProgression
import Game.Variant
import Random
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
