module Game exposing
    ( Action(..)
    , State
    , getPlayerHp
    , getPlayerLevel
    , getPlayerXp
    , getXpNeededForNextLevel
    , hasBeenLost
    , hasEnded
    , init
    , isInProgress
    , listCells
    , toMonsterSummary
    , update
    )

import Game.Board as Board
import Game.Cell as Cell
import Game.Direction exposing (Direction(..))
import Game.Event as Event exposing (Event(..))
import Game.ExpProgression as ExpProgression
import Game.Player as Player
import Game.RevealNeighborsWithZeroPower
import Game.Variant as Variant
import Maybe.Extra
import Random
import Tagged


type alias State =
    { board : Board.State
    , player : Player.Player
    , variant : Variant.Variant
    , status : Status
    }


type Status
    = InProgress
    | Won
    | Lost


type Action
    = TouchCell Board.CellIndex
    | TouchCellAndItsNeighbors Board.CellIndex
    | ChangeBet Direction Board.CellIndex


init : Variant.Identifier -> Random.Seed -> State
init identifier seed =
    let
        variant =
            Variant.get identifier
    in
    { board = Board.init variant seed
    , player = Player.init variant.initialPlayerHp
    , variant = variant
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


getXpNeededForNextLevel : State -> Maybe Int
getXpNeededForNextLevel state =
    state.variant.expProgression
        |> ExpProgression.getXpNeededForNextLevel state.player.level state.player.xp
        |> Maybe.map Tagged.untag


hasEnded : State -> Bool
hasEnded state =
    case state.status of
        InProgress ->
            False

        Won ->
            True

        Lost ->
            True


isInProgress : State -> Bool
isInProgress =
    hasEnded >> not


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
        { state | status = Lost, board = Board.revealAllCells state.board }

    else
        state


emitEvents : State -> State -> Cell.Cell -> List Event
emitEvents oldState newState oldTouchedCell =
    if Cell.isRevealed oldTouchedCell then
        []

    else
        [ if hasEnded newState then
            if hasBeenLost newState then
                Just GameOver

            else
                Just GameWon

          else
            Nothing
        , if Tagged.untag newState.player.level > Tagged.untag oldState.player.level then
            Just LevelUp

          else
            Nothing
        , if Cell.isMonster oldTouchedCell then
            if Player.isMorePowerfulThanCell oldState.player oldTouchedCell then
                Just MonsterKilled

            else
                Just HitByMonster

          else
            Nothing
        ]
            |> Maybe.Extra.values


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


update : Action -> State -> ( State, List Event )
update action state =
    if not <| hasEnded state then
        case action of
            TouchCell index ->
                Board.indexToCell state.board index
                    |> Maybe.map
                        (\cell ->
                            let
                                updatedBoard =
                                    Board.touchCell index state.board
                                        |> revealNeighborsWithZeroPowerIfZeroSurroundingPower index

                                updatedPlayer =
                                    if not <| Cell.isRevealed cell then
                                        Player.touchCell state.variant.expProgression cell state.player

                                    else
                                        state.player

                                newState =
                                    { state | board = updatedBoard, player = updatedPlayer }
                                        |> endGameIfPlayerIsDead
                            in
                            ( newState
                            , emitEvents state newState cell
                            )
                        )
                    |> Maybe.withDefault ( state, [] )

            TouchCellAndItsNeighbors index ->
                let
                    neighborIndexes =
                        Board.getNeighborIndexes state.board index

                    indexesToTouch =
                        index :: neighborIndexes
                in
                List.foldl
                    (\indexToTouch ( accState, accEvents ) ->
                        Board.indexToCell accState.board indexToTouch
                            |> Maybe.map
                                (\cell ->
                                    if not <| Cell.isRevealed cell then
                                        let
                                            ( newState, newEvents ) =
                                                update (TouchCell indexToTouch) accState
                                        in
                                        ( newState, List.append accEvents newEvents )

                                    else
                                        ( accState, accEvents )
                                )
                            |> Maybe.withDefault ( accState, accEvents )
                    )
                    ( state, [] )
                    indexesToTouch

            ChangeBet direction index ->
                ( { state
                    | board =
                        Board.changeBet
                            ( state.variant.minPower, state.variant.maxPower )
                            direction
                            index
                            state.board
                  }
                , []
                )

    else
        ( state, [] )


toMonsterSummary : State -> Board.MonsterSummary
toMonsterSummary { board } =
    Board.toMonsterSummary board
