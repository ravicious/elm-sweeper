module Game.Cell
    exposing
        ( Cell
        , changeBet
        , describeDisplayedValue
        , getHitPower
        , getPower
        , getXpReward
        , hasZeroPower
        , hasZeroSurroundingPower
        , init
        , isMonster
        , isRevealed
        , isTouchable
        , reveal
        , setSurroundingPowerFromNeighbors
        , toDisplayedValue
        , touch
        )

import Game.Direction exposing (Direction(..))
import Game.ExpProgression exposing (Xp)
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


type alias Bet =
    Maybe Int


type MonsterCellDisplayedValue
    = Power
    | SurroundingPower
    | None


type alias CommonState =
    { surroundingPower : SurroundingPower
    , bet : Bet
    }


type alias ZeroPowerCellState =
    { isRevealed : Bool
    }


type alias MonsterCellState =
    { power : Power
    , displayedValue : MonsterCellDisplayedValue
    }


type SpecificState
    = ZeroPowerCell ZeroPowerCellState
    | MonsterCell MonsterCellState


type Cell
    = Cell CommonState SpecificState


init : Int -> Cell
init power =
    let
        commonState =
            { surroundingPower = Tagged.tag 0
            , bet = Nothing
            }

        specificState =
            if power == 0 then
                ZeroPowerCell
                    { isRevealed = False
                    }
            else
                MonsterCell
                    { power = Tagged.tag power
                    , displayedValue = None
                    }
    in
    Cell commonState specificState


setSurroundingPowerFromNeighbors : List Cell -> Cell -> Cell
setSurroundingPowerFromNeighbors neighbors (Cell commonState specificState) =
    let
        surroundingPower =
            neighbors
                |> List.foldl
                    (\neighbor sumOfPower ->
                        getPower neighbor |> Tagged.map2 (+) sumOfPower
                    )
                    (Tagged.tag 0)
                |> Tagged.retag
    in
    Cell { commonState | surroundingPower = surroundingPower } specificState


getSurroundingPower : Cell -> SurroundingPower
getSurroundingPower (Cell commonState _) =
    commonState.surroundingPower


getBet : Cell -> Bet
getBet (Cell commonState _) =
    commonState.bet


touch : Cell -> Cell
touch (Cell commonState specificState) =
    Cell commonState <|
        case specificState of
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
reveal (Cell commonState specificState) =
    Cell commonState <|
        case specificState of
            ZeroPowerCell state ->
                ZeroPowerCell { state | isRevealed = True }

            MonsterCell state ->
                MonsterCell { state | displayedValue = Power }


changeBet : ( Int, Int ) -> Direction -> Cell -> Cell
changeBet ( minBet, maxBet ) direction (Cell commonState specificState) =
    let
        newBet =
            case direction of
                Left ->
                    case commonState.bet of
                        Just currentBet ->
                            if currentBet == minBet then
                                Nothing
                            else
                                Just <| currentBet - 1

                        Nothing ->
                            Just maxBet

                Right ->
                    case commonState.bet of
                        Just currentBet ->
                            if currentBet == maxBet then
                                Nothing
                            else
                                Just <| currentBet + 1

                        Nothing ->
                            Just minBet
    in
    Cell { commonState | bet = newBet } specificState


hasZeroPower : Cell -> Bool
hasZeroPower (Cell _ specificState) =
    case specificState of
        ZeroPowerCell _ ->
            True

        MonsterCell _ ->
            False


hasZeroSurroundingPower : Cell -> Bool
hasZeroSurroundingPower =
    getSurroundingPower >> Tagged.Extra.is ((==) 0)


isMonster : Cell -> Bool
isMonster (Cell _ specificState) =
    case specificState of
        ZeroPowerCell _ ->
            False

        MonsterCell _ ->
            True


isRevealed : Cell -> Bool
isRevealed (Cell _ specificState) =
    case specificState of
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
isTouchable (Cell _ specificState) =
    case specificState of
        ZeroPowerCell state ->
            not <| state.isRevealed

        MonsterCell _ ->
            True


getPower : Cell -> Power
getPower (Cell _ specificState) =
    case specificState of
        ZeroPowerCell _ ->
            Tagged.tag 0

        MonsterCell state ->
            state.power


getHitPower : Cell -> Power
getHitPower =
    getPower
        >> Tagged.map
            (\power ->
                if power > 1 then
                    power * (power - 1)
                else
                    power
            )


getXpReward : Cell -> Xp
getXpReward =
    getPower
        >> Tagged.map
            (\power ->
                if power > 2 then
                    2 ^ (power - 1)
                else
                    power
            )
        >> Tagged.retag


toDisplayedValue : Cell -> String
toDisplayedValue (Cell commonState specificState) =
    let
        betToString =
            Maybe.map toString >> Maybe.withDefault ""

        surroundingPowerToString =
            Tagged.untag >> toString
    in
    case specificState of
        ZeroPowerCell state ->
            if state.isRevealed then
                if commonState.surroundingPower |> Tagged.Extra.is ((==) 0) then
                    ""
                else
                    surroundingPowerToString commonState.surroundingPower
            else
                betToString commonState.bet

        MonsterCell state ->
            case state.displayedValue of
                None ->
                    betToString commonState.bet

                Power ->
                    state.power |> Tagged.untag |> toString

                SurroundingPower ->
                    surroundingPowerToString commonState.surroundingPower


describeDisplayedValue : Cell -> String
describeDisplayedValue (Cell commonState specificState) =
    let
        betToString =
            Maybe.map (always "bet") >> Maybe.withDefault "none"
    in
    case specificState of
        ZeroPowerCell state ->
            if state.isRevealed then
                "surroundingPower"
            else
                betToString commonState.bet

        MonsterCell state ->
            case state.displayedValue of
                None ->
                    betToString commonState.bet

                Power ->
                    "power"

                SurroundingPower ->
                    "surroundingPower"
