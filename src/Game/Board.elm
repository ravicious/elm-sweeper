module Game.Board exposing (State, CellIndex, init, listCells, revealCell)

import Dict exposing (Dict)
import Game.Cell as Cell
import Game.Variant as Variant exposing (Variant)


type alias CellIndex =
    Int


type alias Cells =
    Dict CellIndex Cell.State


type alias State =
    { rows : Int
    , columns : Int
    , cells : Cells
    }


init : Variant -> State
init variant =
    { rows = variant.rows
    , columns = variant.columns
    , cells = initCells variant
    }


initCells : Variant -> Cells
initCells variant =
    let
        totalNumberOfCells =
            variant.rows * variant.columns

        cellsFromConfiguration =
            variant.cellConfiguration
                |> List.concatMap (\( power, n ) -> List.repeat n (Cell.init power))

        numberOfZeroCells =
            totalNumberOfCells - (List.length cellsFromConfiguration)

        zeroCells =
            List.repeat numberOfZeroCells (Cell.init 0)
    in
        cellsFromConfiguration
            ++ zeroCells
            |> List.indexedMap (\index cell -> ( index, cell ))
            |> Dict.fromList


listCells : (( CellIndex, Cell.State ) -> b) -> State -> List b
listCells f state =
    Dict.toList state.cells |> List.map f


revealCell : CellIndex -> State -> State
revealCell index state =
    let
        cell =
            Dict.get index state.cells
    in
        cell
            |> Maybe.map
                (\cell ->
                    if Cell.isVisible cell then
                        state
                    else
                        { state
                            | cells =
                                Dict.update
                                    index
                                    (Maybe.map Cell.reveal)
                                    state.cells
                        }
                )
            |> Maybe.withDefault state
