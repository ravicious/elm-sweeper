module Game exposing (State, Action(..), init, listCells, update)

import Random
import Game.Cell as Cell
import Game.Board as Board
import Game.Variant as Variant
import Game.RevealNeighborsWithZeroPower


type alias State =
    { board : Board.State
    }


type Action
    = TouchCell Board.CellIndex


init : Variant.Identifier -> Random.Seed -> State
init identifier seed =
    let
        variant =
            Variant.get identifier
    in
        { board = Board.init variant seed
        }


listCells : (( Board.CellIndex, Cell.Cell ) -> b) -> State -> List b
listCells f state =
    Board.listCells f state.board


revealNeighborsWithZeroPowerIfZeroSurroundingPower : Board.CellIndex -> Board.State -> Board.State
revealNeighborsWithZeroPowerIfZeroSurroundingPower index boardState =
    Board.indexToCell boardState index
        |> Maybe.map
            (\cell ->
                if Cell.hasZeroSurroundingPower cell then
                    Game.RevealNeighborsWithZeroPower.run index boardState
                else
                    boardState
            )
        |> Maybe.withDefault boardState


update : Action -> State -> State
update action state =
    case action of
        TouchCell index ->
            { state
                | board =
                    Board.touchCell index state.board
                        |> revealNeighborsWithZeroPowerIfZeroSurroundingPower index
            }
