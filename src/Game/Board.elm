module Game.Board exposing
    ( CellIndex
    , MonsterSummary
    , State
    , changeBet
    , getNeighborIndexes
    , indexToCell
    , indexToPoint
    , init
    , initWithZeroPower
    , listCells
    , pointToIndex
    , revealCell
    , toMonsterSummary
    , touchCell
    )

import Dict exposing (Dict)
import Game.Cell as Cell
import Game.Direction exposing (Direction(..))
import Game.Point as Point exposing (Point)
import Game.Variant as Variant exposing (Variant)
import Maybe.Extra
import Random
import Random.List
import Tagged


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


type alias Power =
    Int


type alias Count =
    Int


type alias MonsterSummary =
    Dict Power Count



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
            totalNumberOfCells - List.length cellsFromConfiguration

        zeroCells =
            List.repeat numberOfZeroCells (Cell.init 0)

        cells =
            cellsFromConfiguration
                ++ zeroCells

        ( shuffledCells, _ ) =
            Random.List.shuffle cells |> (\a -> Random.step a seed)
    in
    listOfCellsToDictOfCells shuffledCells


listOfCellsToDictOfCells : List Cell.Cell -> Cells
listOfCellsToDictOfCells =
    List.indexedMap (\a b -> ( a, b )) >> Dict.fromList


calculateSurroundingPowerOfCells : State -> State
calculateSurroundingPowerOfCells state =
    { state | cells = Dict.map (calculateSurroundingPowerForCell state) state.cells }


calculateSurroundingPowerForCell : State -> CellIndex -> Cell.Cell -> Cell.Cell
calculateSurroundingPowerForCell state index cell =
    let
        neighbors =
            getNeighbors state index
    in
    Cell.setSurroundingPowerFromNeighbors neighbors cell



-- Cell operations


pointToCell : State -> Point -> Maybe Cell.Cell
pointToCell state point =
    pointToIndex state point
        |> Maybe.andThen (indexToCell state)


getNeighbors : State -> CellIndex -> Neighbors
getNeighbors =
    transformNeighborPoints pointToCell


getNeighborIndexes : State -> CellIndex -> List CellIndex
getNeighborIndexes =
    transformNeighborPoints pointToIndex


transformNeighborPoints : (State -> Point -> Maybe a) -> State -> CellIndex -> List a
transformNeighborPoints f state index =
    indexToNeighborPoints state index
        |> List.foldl
            (\point xs ->
                f state point
                    |> Maybe.map (\a -> (::) a xs)
                    |> Maybe.withDefault xs
            )
            []


indexToNeighborPoints : State -> CellIndex -> List Point
indexToNeighborPoints state index =
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
        |> Maybe.map Maybe.Extra.values
        |> Maybe.withDefault []


indexToPoint : State -> CellIndex -> Maybe Point
indexToPoint state index =
    if isValidIndex state index then
        let
            x =
                modBy state.columns index

            y =
                index // state.columns
        in
        Just <| Point x y

    else
        Nothing


indexToCell : State -> CellIndex -> Maybe Cell.Cell
indexToCell state index =
    Dict.get index state.cells


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
    index < Dict.size state.cells


isValidPoint : State -> Point -> Bool
isValidPoint state point =
    point.x < state.columns && point.x >= 0 && point.y < state.rows && point.y >= 0


updateCell : (Cell.Cell -> Cell.Cell) -> CellIndex -> State -> State
updateCell f index state =
    if isValidIndex state index then
        { state | cells = Dict.update index (Maybe.map f) state.cells }

    else
        state



-- Helpers for views


listCells : (( CellIndex, Cell.Cell ) -> b) -> State -> List b
listCells f state =
    Dict.toList state.cells |> List.map f



-- Actions


touchCell : CellIndex -> State -> State
touchCell =
    updateCell <|
        \cell ->
            if Cell.isTouchable cell then
                Cell.touch cell

            else
                cell


revealCell : CellIndex -> State -> State
revealCell =
    updateCell Cell.reveal


changeBet : ( Int, Int ) -> Direction -> CellIndex -> State -> State
changeBet betThresholds direction =
    updateCell <| \cell -> Cell.changeBet betThresholds direction cell



-- Monster summary


toMonsterSummary : State -> MonsterSummary
toMonsterSummary { cells } =
    Dict.foldl
        (\index cell monsterSummary ->
            if Cell.isMonster cell then
                let
                    powerInt =
                        Cell.getPower cell |> Tagged.untag

                    monsterSummaryWithPowerIntInitialized =
                        Dict.update powerInt (\i -> Maybe.Extra.or i (Just 0)) monsterSummary
                in
                -- If there's a monster cell still hidden, that's going to count towards the number
                -- of those cells left.
                -- Otherwise we want to show 0, hence why we ensure the given power int is
                -- initialized in monsterSummary.
                if Cell.isHidden cell then
                    Dict.update powerInt (Maybe.map ((+) 1)) monsterSummaryWithPowerIntInitialized

                else
                    monsterSummaryWithPowerIntInitialized

            else
                monsterSummary
        )
        Dict.empty
        cells
