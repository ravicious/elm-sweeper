module Game.Cell
    exposing
        ( Cell
        , init
        , setSurroundingPower
        , touch
        , reveal
        , hasZeroPower
        , hasZeroSurroundingPower
        , isMonster
        , isRevealed
        , isTouchable
        , power
        , toDisplayedValue
        , describeDisplayedValue
        )


type alias Power =
    Int


type MonsterCellDisplayedValue
    = Power
    | SurroundingPower
    | None


type alias ZeroPowerCellState =
    { isRevealed : Bool
    , surroundingPower : Power
    }


type alias MonsterCellState =
    { surroundingPower : Power
    , power : Power
    , displayedValue : MonsterCellDisplayedValue
    }


type Cell
    = ZeroPowerCell ZeroPowerCellState
    | MonsterCell MonsterCellState


init : Power -> Cell
init power =
    if power == 0 then
        ZeroPowerCell
            { isRevealed = False
            , surroundingPower = 0
            }
    else
        MonsterCell
            { surroundingPower = 0
            , power = power
            , displayedValue = None
            }


setSurroundingPower : Power -> Cell -> Cell
setSurroundingPower surroundingPower cell =
    case cell of
        MonsterCell state ->
            MonsterCell { state | surroundingPower = surroundingPower }

        ZeroPowerCell state ->
            ZeroPowerCell { state | surroundingPower = surroundingPower }


surroundingPower : Cell -> Power
surroundingPower cell =
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
    surroundingPower >> ((==) 0)


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


power : Cell -> Power
power cell =
    case cell of
        ZeroPowerCell _ ->
            0

        MonsterCell state ->
            state.power


toDisplayedValue : Cell -> String
toDisplayedValue cell =
    case cell of
        ZeroPowerCell state ->
            if state.isRevealed then
                if state.surroundingPower == 0 then
                    ""
                else
                    toString state.surroundingPower
            else
                ""

        MonsterCell state ->
            case state.displayedValue of
                None ->
                    ""

                Power ->
                    toString state.power

                SurroundingPower ->
                    toString state.surroundingPower


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
