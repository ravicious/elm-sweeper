module Game.RevealNeighborsWithZeroPower exposing (run)

import Set exposing (Set)
import List.Extra
import Game.Board as Board exposing (CellIndex)
import Game.Cell as Cell


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


addIndexToVisit : CellIndex -> ScanState -> ScanState
addIndexToVisit index state =
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


updateToVisit : List CellIndex -> ScanState -> ScanState
updateToVisit indexesToVisit state =
    { state | toVisit = Set.fromList indexesToVisit }



-- Algorithm


addIndexToRevealIfShouldBeRevealed : Board.State -> CellIndex -> ScanState -> ScanState
addIndexToRevealIfShouldBeRevealed boardState index scanState =
    Board.indexToCell boardState index
        |> Maybe.map
            (\cell ->
                if Cell.hasZeroPower cell then
                    addIndexToReveal index scanState
                else
                    scanState
            )
        |> Maybe.withDefault scanState


addNeighborsToVisit : Board.State -> CellIndex -> ScanState -> ScanState
addNeighborsToVisit boardState index scanState =
    Board.indexToCell boardState index
        |> Maybe.map
            (\cell ->
                if not <| Cell.hasZeroSurroundingPower cell then
                    scanState
                else
                    let
                        neighborIndexes =
                            Board.getNeighborIndexes boardState index
                    in
                        List.foldr
                            (\neighborIndex newScanState ->
                                if hasIndexBeenVisited neighborIndex newScanState then
                                    newScanState
                                else
                                    addIndexToVisit neighborIndex newScanState
                            )
                            scanState
                            neighborIndexes
            )
        |> Maybe.withDefault scanState


scanIndex : Board.State -> CellIndex -> ScanState -> ScanState
scanIndex boardState index scanState =
    scanState
        |> addIndexToVisited index
        |> addIndexToRevealIfShouldBeRevealed boardState index
        |> addNeighborsToVisit boardState index


scan : Board.State -> ScanState -> ScanState
scan boardState scanState =
    case List.Extra.uncons (Set.toList scanState.toVisit) of
        Nothing ->
            scanState

        Just ( indexToScan, restOfToVisit ) ->
            scanState
                |> updateToVisit restOfToVisit
                |> scanIndex boardState indexToScan
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
            |> List.foldr Board.revealCell boardState
