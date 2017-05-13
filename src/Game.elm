module Game exposing (State, init, listCells)

import Dict exposing (Dict)
import Game.Cell as Cell
import Game.Board as Board
import Game.Variant as Variant


type alias State =
    { board : Board.State
    }


init : Variant.Identifier -> State
init identifier =
    let
        variant =
            Variant.get identifier
    in
        { board = Board.init variant
        }


listCells : (( Board.CellIndex, Cell.State ) -> b) -> State -> List b
listCells f state =
    Board.listCells f state.board
