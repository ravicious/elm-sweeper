module Game.Cell
    exposing
        ( Cell
        , init
        , setSurroundingPowerFromNeighbors
        , touch
        , reveal
        , changeBet
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
import Game.Direction exposing (Direction(..))


type PowerTag
    = PowerTag


type SurroundingPowerTag
    = SurroundingPowerTag


type alias Power =
    Tagged PowerTag Int


type alias SurroundingPower =
    Tagged SurroundingPowerTag Int


type alias Bet =
    Maybe Int


type MonsterCellDisplayedValue
    = Power
    | SurroundingPower
    | None


type alias ZeroPowerCellState =
    { isRevealed : Bool
    , surroundingPower : SurroundingPower
    , bet : Bet
    }


type alias MonsterCellState =
    { surroundingPower : SurroundingPower
    , power : Power
    , displayedValue : MonsterCellDisplayedValue
    , bet : Bet
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
            , bet = Nothing
            }
    else
        MonsterCell
            { surroundingPower = Tagged.tag 0
            , power = Tagged.tag power
            , displayedValue = None
            , bet = Nothing
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


getBet : Cell -> Bet
getBet cell =
    case cell of
        MonsterCell state ->
            state.bet

        ZeroPowerCell state ->
            state.bet


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


changeBet : ( Int, Int ) -> Direction -> Cell -> Cell
changeBet ( minBet, maxBet ) direction cell =
    let
        newBet =
            case direction of
                Left ->
                    case getBet cell of
                        Just currentBet ->
                            if currentBet == minBet then
                                Nothing
                            else
                                Just <| currentBet - 1

                        Nothing ->
                            Just maxBet

                Right ->
                    case getBet cell of
                        Just currentBet ->
                            if currentBet == maxBet then
                                Nothing
                            else
                                Just <| currentBet + 1

                        Nothing ->
                            Just minBet
    in
        case cell of
            ZeroPowerCell state ->
                ZeroPowerCell { state | bet = newBet }

            MonsterCell state ->
                MonsterCell { state | bet = newBet }


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
                getBet cell |> Maybe.map toString |> Maybe.withDefault ""

        MonsterCell state ->
            case state.displayedValue of
                None ->
                    getBet cell |> Maybe.map toString |> Maybe.withDefault ""

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
                case getBet cell of
                    Just _ ->
                        "bet"

                    Nothing ->
                        "none"

        MonsterCell state ->
            case state.displayedValue of
                None ->
                    case getBet cell of
                        Just _ ->
                            "bet"

                        Nothing ->
                            "none"

                Power ->
                    "power"

                SurroundingPower ->
                    "surroundingPower"
