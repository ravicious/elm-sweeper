module Game.RevealNeighborsWithZeroPower exposing (run)

import Game.Board as Board exposing (CellIndex)
import Game.Cell as Cell
import List.Extra
import Set exposing (Set)



-- State & operations on state


type alias ScanState =
    { toVisit : Set CellIndex
    , visited : Set CellIndex
    , toReveal : Set CellIndex
    }


initScanState : List CellIndex -> ScanState
initScanState indexes =
    { toVisit = Set.fromList indexes
    , visited = Set.empty
    , toReveal = Set.empty
    }


addIndexToVisitIfNotVisited : CellIndex -> ScanState -> ScanState
addIndexToVisitIfNotVisited index state =
    if hasIndexBeenVisited index state then
        state

    else
        { state | toVisit = Set.insert index state.toVisit }


addIndexToVisited : CellIndex -> ScanState -> ScanState
addIndexToVisited index state =
    { state | visited = Set.insert index state.visited }


addIndexToReveal : CellIndex -> ScanState -> ScanState
addIndexToReveal index state =
    { state | toReveal = Set.insert index state.toReveal }


hasIndexBeenVisited : CellIndex -> ScanState -> Bool
hasIndexBeenVisited index state =
    Set.member index state.visited


replaceToVisit : List CellIndex -> ScanState -> ScanState
replaceToVisit indexesToVisit state =
    { state | toVisit = Set.fromList indexesToVisit }


updateScanState :
    (Board.State -> CellIndex -> Cell.Cell -> ScanState -> ScanState)
    -> Board.State
    -> CellIndex
    -> ScanState
    -> ScanState
updateScanState f boardState index scanState =
    Board.indexToCell boardState index
        |> Maybe.map (\cell -> f boardState index cell scanState)
        |> Maybe.withDefault scanState



-- Algorithm


addIndexToRevealIfShouldBeRevealed : Board.State -> CellIndex -> ScanState -> ScanState
addIndexToRevealIfShouldBeRevealed =
    updateScanState <|
        \boardState index cell scanState ->
            if Cell.hasZeroPower cell then
                addIndexToReveal index scanState

            else
                scanState


addNeighborsToVisitIfCellHasZeroSurroundingPower :
    Board.State
    -> CellIndex
    -> ScanState
    -> ScanState
addNeighborsToVisitIfCellHasZeroSurroundingPower =
    updateScanState <|
        \boardState index cell scanState ->
            if Cell.hasZeroSurroundingPower cell then
                Board.getNeighborIndexes boardState index
                    |> List.foldl addIndexToVisitIfNotVisited scanState

            else
                scanState


visitIndex : Board.State -> CellIndex -> ScanState -> ScanState
visitIndex boardState index scanState =
    scanState
        |> addIndexToVisited index
        |> addIndexToRevealIfShouldBeRevealed boardState index
        |> addNeighborsToVisitIfCellHasZeroSurroundingPower boardState index


scan : Board.State -> ScanState -> ScanState
scan boardState scanState =
    case List.Extra.uncons (Set.toList scanState.toVisit) of
        Nothing ->
            scanState

        Just ( nextIndexToVisit, restOfToVisit ) ->
            scanState
                |> replaceToVisit restOfToVisit
                |> visitIndex boardState nextIndexToVisit
                |> scan boardState


run : CellIndex -> Board.State -> Board.State
run index boardState =
    let
        neighborIndexes =
            Board.getNeighborIndexes boardState index

        scanState =
            initScanState neighborIndexes
                |> scan boardState
    in
    Set.toList scanState.toReveal
        |> List.foldl Board.revealCell boardState
