module Game exposing (VariantIdentifier(..), State, init, mapCells)

import Dict exposing (Dict)


type alias CellIndex =
    Int


type alias Power =
    Int


type alias CellState =
    { power : Power
    }


type alias State =
    { cells : Dict CellIndex CellState
    }


type VariantIdentifier
    = SixteenByThirty


type alias Variant =
    { rows : Int
    , columns : Int
    }


getVariant : VariantIdentifier -> Variant
getVariant identifier =
    case identifier of
        SixteenByThirty ->
            { rows = 16
            , columns = 30
            }


init : VariantIdentifier -> State
init variantIdentifier =
    let
        variant =
            getVariant variantIdentifier
    in
        { cells =
            List.range 0 ((variant.rows * variant.columns) - 1)
                |> List.map
                    (\cellIndex ->
                        ( cellIndex, { power = 1 } )
                    )
                |> Dict.fromList
        }


mapCells : (( CellIndex, CellState ) -> b) -> State -> List b
mapCells f state =
    Dict.toList state.cells |> List.map f
