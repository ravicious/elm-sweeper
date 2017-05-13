module Game.Board exposing (State, CellIndex, init, listCells)

import Dict exposing (Dict)
import Game.Cell as Cell
import Game.Variant as Variant exposing (Variant)


type alias CellIndex =
    Int


type alias State =
    { rows : Int
    , columns : Int
    , cells : Dict CellIndex Cell.State
    }


init : Variant -> State
init variant =
    { rows = variant.rows
    , columns = variant.columns
    , cells =
        List.range 0 ((variant.rows * variant.columns) - 1)
            |> List.map
                (\cellIndex ->
                    ( cellIndex, (Cell.init 1) )
                )
            |> Dict.fromList
    }


listCells : (( CellIndex, Cell.State ) -> b) -> State -> List b
listCells f state =
    Dict.toList state.cells |> List.map f
