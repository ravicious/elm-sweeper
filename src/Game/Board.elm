module Game.Board exposing (State, CellIndex, init, listCells, revealCell)

import Dict exposing (Dict)
import Random
import Random.List
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



-- Init


init : Variant -> Random.Seed -> State
init variant seed =
    { rows = variant.rows
    , columns = variant.columns
    , cells = initCells variant seed
    }


initCells : Variant -> Random.Seed -> Cells
initCells variant seed =
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

        cells =
            cellsFromConfiguration
                ++ zeroCells

        ( shuffledCells, _ ) =
            Random.List.shuffle cells |> (flip Random.step) seed
    in
        shuffledCells
            |> List.indexedMap (,)
            |> Dict.fromList



-- Helpers for views


listCells : (( CellIndex, Cell.State ) -> b) -> State -> List b
listCells f state =
    Dict.toList state.cells |> List.map f



-- Actions


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
