module Game.Cell exposing
    ( Cell
    , RevealedBy(..)
    , changeBet
    , getPower
    , getXpReward
    , hasZeroPower
    , hasZeroSurroundingPower
    , init
    , isHidden
    , isMonster
    , isRevealed
    , isTouchable
    , reveal
    , revealedBy
    , setSurroundingPowerFromNeighbors
    , toContent
    , touch
    )

import Game.Cell.Content as Content
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


type RevealedBy
    = RevealedByPlayer
    | RevealedByGameOver


type alias CommonState =
    { surroundingPower : SurroundingPower
    , bet : Bet
    }


type alias ZeroPowerCellState =
    { isRevealed : Bool
    }


type alias MonsterCellState =
    { power : Power
    , visibility : MonsterCellVisibility
    }


type MonsterCellVisibility
    = Hidden
    | Revealed RevealedBy MonsterCellDisplayedValue


type MonsterCellDisplayedValue
    = Power
    | SurroundingPower


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
                    , visibility = Hidden
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


touch : Cell -> Cell
touch (Cell commonState specificState) =
    Cell commonState <|
        case specificState of
            ZeroPowerCell state ->
                ZeroPowerCell { state | isRevealed = True }

            MonsterCell state ->
                let
                    nextVisibility =
                        case state.visibility of
                            Hidden ->
                                Revealed RevealedByPlayer Power

                            Revealed wasRevealedBy Power ->
                                Revealed wasRevealedBy SurroundingPower

                            Revealed wasRevealedBy SurroundingPower ->
                                Revealed wasRevealedBy Power
                in
                MonsterCell { state | visibility = nextVisibility }


reveal : RevealedBy -> Cell -> Cell
reveal wasRevealedBy (Cell commonState specificState) =
    let
        cell =
            Cell commonState specificState
    in
    if isRevealed cell then
        -- For already revealed cells, we need to not reveal them again so that we don't overwrite
        -- their revealedBy value.
        cell

    else
        Cell commonState <|
            case specificState of
                ZeroPowerCell state ->
                    ZeroPowerCell { state | isRevealed = True }

                MonsterCell state ->
                    MonsterCell { state | visibility = Revealed wasRevealedBy Power }


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
            case state.visibility of
                Hidden ->
                    False

                Revealed _ _ ->
                    True


revealedBy : Cell -> Maybe RevealedBy
revealedBy (Cell _ specificState) =
    case specificState of
        ZeroPowerCell state ->
            if state.isRevealed then
                Just RevealedByPlayer

            else
                Nothing

        MonsterCell state ->
            case state.visibility of
                Hidden ->
                    Nothing

                Revealed wasRevealedBy _ ->
                    Just wasRevealedBy


isHidden : Cell -> Bool
isHidden =
    isRevealed >> not


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


toContent : Cell -> Content.Content
toContent (Cell commonState specificState) =
    let
        betToContent =
            Maybe.map Content.Bet >> Maybe.withDefault Content.Nothing
    in
    case specificState of
        ZeroPowerCell state ->
            if state.isRevealed then
                if commonState.surroundingPower |> Tagged.Extra.is ((==) 0) then
                    Content.Nothing

                else
                    Content.SurroundingPower <| Tagged.untag commonState.surroundingPower

            else
                betToContent commonState.bet

        MonsterCell state ->
            case state.visibility of
                Hidden ->
                    betToContent commonState.bet

                Revealed _ Power ->
                    Content.Power <| Tagged.untag state.power

                Revealed _ SurroundingPower ->
                    Content.SurroundingPower <| Tagged.untag commonState.surroundingPower
