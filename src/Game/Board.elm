module Game.Board
    exposing
        ( State
        , CellIndex
        , init
        , initWithZeroPower
        , listCells
        , touchCell
        , indexToPoint
        , pointToIndex
        )

import Dict exposing (Dict)
import Random
import Random.List
import Maybe.Extra
import Game.Cell as Cell
import Game.Variant as Variant exposing (Variant)
import Game.Point as Point exposing (Point)


type alias CellIndex =
    Int


type alias Cells =
    Dict CellIndex Cell.Cell


type alias Neighbors =
    List Cell.Cell


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
        |> calculateSurroundingPowerOfCells


{-| Generates a board with given dimension with all zero-power cells. Useful in testing.
-}
initWithZeroPower : Int -> Int -> Maybe State
initWithZeroPower rows columns =
    if rows > 0 && columns > 0 then
        Just <|
            { rows = rows
            , columns = columns
            , cells = List.repeat (rows * columns) (Cell.init 0) |> listOfCellsToDictOfCells
            }
    else
        Nothing


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
        listOfCellsToDictOfCells shuffledCells


listOfCellsToDictOfCells : List Cell.Cell -> Cells
listOfCellsToDictOfCells =
    List.indexedMap (,) >> Dict.fromList


calculateSurroundingPowerOfCells : State -> State
calculateSurroundingPowerOfCells state =
    { state | cells = Dict.map (calculateSurroundingPowerForCell state) state.cells }


calculateSurroundingPowerForCell : State -> CellIndex -> Cell.Cell -> Cell.Cell
calculateSurroundingPowerForCell state index cell =
    let
        neighbors =
            getNeighbors state index

        surroundingPower =
            List.foldr (\neighbor sumOfPower -> Cell.power neighbor |> (+) sumOfPower)
                0
                neighbors
    in
        Cell.setSurroundingPower surroundingPower cell



-- Cell getters


pointToCell : State -> Point -> Maybe Cell.Cell
pointToCell state point =
    pointToIndex state point
        |> Maybe.andThen ((flip Dict.get) state.cells)


getNeighbors : State -> CellIndex -> Neighbors
getNeighbors state index =
    indexToPoint state index
        |> Maybe.map
            (\point ->
                [ transformPoint -1 -1 state point
                , transformPoint 0 -1 state point
                , transformPoint 1 -1 state point
                , transformPoint -1 0 state point
                , transformPoint 1 0 state point
                , transformPoint -1 1 state point
                , transformPoint 0 1 state point
                , transformPoint 1 1 state point
                ]
            )
        |> Maybe.map
            (List.foldr
                -- Resolve transformed points to cells.
                (\point neighborCells ->
                    point
                        |> Maybe.map (pointToCell state)
                        |> Maybe.map ((flip (::)) neighborCells)
                        |> Maybe.withDefault neighborCells
                )
                []
            )
        |> Maybe.map Maybe.Extra.values
        |> Maybe.withDefault []


indexToPoint : State -> CellIndex -> Maybe Point
indexToPoint state index =
    if isValidIndex state index then
        let
            x =
                (index % state.columns)

            y =
                (index // state.columns)
        in
            Just <| Point x y
    else
        Nothing


pointToIndex : State -> Point -> Maybe CellIndex
pointToIndex state point =
    if isValidPoint state point then
        Just <| point.x + (point.y * state.columns)
    else
        Nothing


transformPoint : Int -> Int -> State -> Point -> Maybe Point
transformPoint transformX transformY state point =
    let
        newPoint =
            Point.transform transformX transformY point
    in
        if isValidPoint state newPoint then
            Just newPoint
        else
            Nothing


isValidIndex : State -> CellIndex -> Bool
isValidIndex state index =
    index < (Dict.size state.cells)


isValidPoint : State -> Point -> Bool
isValidPoint state point =
    point.x < state.columns && point.y < state.rows



-- Helpers for views


listCells : (( CellIndex, Cell.Cell ) -> b) -> State -> List b
listCells f state =
    Dict.toList state.cells |> List.map f



-- Actions


touchCell : CellIndex -> State -> State
touchCell index state =
    let
        cell =
            Dict.get index state.cells
    in
        cell
            |> Maybe.map
                (\cell ->
                    { state
                        | cells =
                            Dict.update
                                index
                                (Maybe.map Cell.touch)
                                state.cells
                    }
                )
            |> Maybe.withDefault state
