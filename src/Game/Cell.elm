module Game.Cell
    exposing
        ( Cell
        , init
        , setSurroundingPowerFromNeighbors
        , touch
        , reveal
        , getPower
        , hasZeroPower
        , hasZeroSurroundingPower
        , isMonster
        , isRevealed
        , isTouchable
        , toDisplayedValue
        , describeDisplayedValue
        )

import Tagged exposing (Tagged)
import Tagged.Extra


type PowerTag
    = PowerTag


type SurroundingPowerTag
    = SurroundingPowerTag


type alias Power =
    Tagged PowerTag Int


type alias SurroundingPower =
    Tagged SurroundingPowerTag Int


type MonsterCellDisplayedValue
    = Power
    | SurroundingPower
    | None


type alias ZeroPowerCellState =
    { isRevealed : Bool
    , surroundingPower : SurroundingPower
    }


type alias MonsterCellState =
    { surroundingPower : SurroundingPower
    , power : Power
    , displayedValue : MonsterCellDisplayedValue
    }


type Cell
    = ZeroPowerCell ZeroPowerCellState
    | MonsterCell MonsterCellState


init : Int -> Cell
init power =
    if power == 0 then
        ZeroPowerCell
            { isRevealed = False
            , surroundingPower = Tagged.tag 0
            }
    else
        MonsterCell
            { surroundingPower = Tagged.tag 0
            , power = Tagged.tag power
            , displayedValue = None
            }


setSurroundingPowerFromNeighbors : List Cell -> Cell -> Cell
setSurroundingPowerFromNeighbors neighbors cell =
    let
        surroundingPower =
            neighbors
                |> List.foldr
                    (\neighbor sumOfPower ->
                        getPower neighbor |> Tagged.map2 (+) sumOfPower
                    )
                    (Tagged.tag 0)
                |> Tagged.retag
    in
        case cell of
            MonsterCell state ->
                MonsterCell { state | surroundingPower = surroundingPower }

            ZeroPowerCell state ->
                ZeroPowerCell { state | surroundingPower = surroundingPower }


getSurroundingPower : Cell -> SurroundingPower
getSurroundingPower cell =
    case cell of
        MonsterCell state ->
            state.surroundingPower

        ZeroPowerCell state ->
            state.surroundingPower


touch : Cell -> Cell
touch cell =
    case cell of
        ZeroPowerCell state ->
            ZeroPowerCell { state | isRevealed = True }

        MonsterCell state ->
            let
                nextDisplayedValue =
                    case state.displayedValue of
                        None ->
                            Power

                        Power ->
                            SurroundingPower

                        SurroundingPower ->
                            Power
            in
                MonsterCell { state | displayedValue = nextDisplayedValue }


reveal : Cell -> Cell
reveal cell =
    case cell of
        ZeroPowerCell state ->
            ZeroPowerCell { state | isRevealed = True }

        MonsterCell state ->
            MonsterCell { state | displayedValue = Power }


hasZeroPower : Cell -> Bool
hasZeroPower cell =
    case cell of
        ZeroPowerCell _ ->
            True

        MonsterCell _ ->
            False


hasZeroSurroundingPower : Cell -> Bool
hasZeroSurroundingPower =
    getSurroundingPower >> Tagged.Extra.is ((==) 0)


isMonster : Cell -> Bool
isMonster cell =
    case cell of
        ZeroPowerCell _ ->
            False

        MonsterCell _ ->
            True


isRevealed : Cell -> Bool
isRevealed cell =
    case cell of
        ZeroPowerCell state ->
            state.isRevealed

        MonsterCell state ->
            case state.displayedValue of
                None ->
                    False

                Power ->
                    True

                SurroundingPower ->
                    True


{-| Determines whether touching the cell will have any effect.
-}
isTouchable : Cell -> Bool
isTouchable cell =
    case cell of
        ZeroPowerCell state ->
            not <| state.isRevealed

        MonsterCell _ ->
            True


getPower : Cell -> Power
getPower cell =
    case cell of
        ZeroPowerCell _ ->
            Tagged.tag 0

        MonsterCell state ->
            state.power


toDisplayedValue : Cell -> String
toDisplayedValue cell =
    case cell of
        ZeroPowerCell state ->
            if state.isRevealed then
                if state.surroundingPower |> Tagged.Extra.is ((==) 0) then
                    ""
                else
                    state.surroundingPower |> Tagged.untag |> toString
            else
                ""

        MonsterCell state ->
            case state.displayedValue of
                None ->
                    ""

                Power ->
                    state.power |> Tagged.untag |> toString

                SurroundingPower ->
                    state.surroundingPower |> Tagged.untag |> toString


describeDisplayedValue : Cell -> String
describeDisplayedValue cell =
    case cell of
        ZeroPowerCell state ->
            if state.isRevealed then
                "surroundingPower"
            else
                "none"

        MonsterCell state ->
            case state.displayedValue of
                None ->
                    "none"

                Power ->
                    "power"

                SurroundingPower ->
                    "surroundingPower"
