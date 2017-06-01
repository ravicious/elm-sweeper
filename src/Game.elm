module Game
    exposing
        ( State
        , Action(..)
        , init
        , listCells
        , getPlayerLevel
        , getPlayerXp
        , getPlayerHp
        , hasBeenLost
        , update
        )

import Random
import Tagged
import Game.Cell as Cell
import Game.Board as Board
import Game.Variant as Variant
import Game.Player as Player
import Game.RevealNeighborsWithZeroPower


type alias State =
    { board : Board.State
    , player : Player.Player
    , variantIdentifier : Variant.Identifier
    , status : Status
    }


type Status
    = InProgress
    | Won
    | Lost


type Action
    = TouchCell Board.CellIndex


init : Variant.Identifier -> Random.Seed -> State
init identifier seed =
    let
        variant =
            Variant.get identifier
    in
        { board = Board.init variant seed
        , player = Player.init
        , variantIdentifier = identifier
        , status = InProgress
        }


getPlayerLevel : State -> Int
getPlayerLevel =
    .player >> .level >> Tagged.untag


getPlayerXp : State -> Int
getPlayerXp =
    .player >> .xp >> Tagged.untag


getPlayerHp : State -> Int
getPlayerHp =
    .player >> .hp >> Tagged.untag


hasEnded : State -> Bool
hasEnded state =
    case state.status of
        InProgress ->
            False

        Won ->
            True

        Lost ->
            True


hasBeenLost : State -> Bool
hasBeenLost state =
    case state.status of
        InProgress ->
            False

        Won ->
            False

        Lost ->
            True


listCells : (( Board.CellIndex, Cell.Cell ) -> b) -> State -> List b
listCells f state =
    Board.listCells f state.board


endGameIfPlayerIsDead : State -> State
endGameIfPlayerIsDead state =
    if Player.isDead state.player then
        { state | status = Lost }
    else
        state


revealNeighborsWithZeroPowerIfZeroSurroundingPower : Board.CellIndex -> Board.State -> Board.State
revealNeighborsWithZeroPowerIfZeroSurroundingPower index boardState =
    Board.indexToCell boardState index
        |> Maybe.map
            (\cell ->
                if Cell.hasZeroSurroundingPower cell then
                    Game.RevealNeighborsWithZeroPower.run index boardState
                else
                    boardState
            )
        |> Maybe.withDefault boardState


update : Action -> State -> State
update action state =
    if not <| hasEnded state then
        case action of
            TouchCell index ->
                let
                    variant =
                        Variant.get state.variantIdentifier
                in
                    Board.indexToCell state.board index
                        |> Maybe.map
                            (\cell ->
                                { state
                                    | board =
                                        Board.touchCell index state.board
                                            |> revealNeighborsWithZeroPowerIfZeroSurroundingPower
                                                index
                                    , player =
                                        if not <| Cell.isRevealed cell then
                                            Player.touchCell
                                                variant.expProgression
                                                cell
                                                state.player
                                        else
                                            state.player
                                }
                                    |> endGameIfPlayerIsDead
                            )
                        |> Maybe.withDefault state
    else
        state
