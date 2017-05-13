module Game exposing (State, Action(..), init, listCells, update)

import Random
import Game.Cell as Cell
import Game.Board as Board
import Game.Variant as Variant


type alias State =
    { board : Board.State
    }


type Action
    = RevealCell Board.CellIndex


init : Variant.Identifier -> Random.Seed -> State
init identifier seed =
    let
        variant =
            Variant.get identifier
    in
        { board = Board.init variant seed
        }


listCells : (( Board.CellIndex, Cell.State ) -> b) -> State -> List b
listCells f state =
    Board.listCells f state.board


update : Action -> State -> State
update action state =
    case action of
        RevealCell index ->
            { state | board = Board.revealCell index state.board }
