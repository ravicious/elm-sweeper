module Game.Cell
    exposing
        ( State
        , init
        , setSurroundingPower
        , reveal
        , isVisible
        , hasZeroPower
        , isMonster
        )


type alias Power =
    Int


type alias State =
    { power : Power
    , visible : Bool
    , surroundingPower : Power
    }


init : Power -> State
init power =
    { power = power
    , visible = False
    , surroundingPower = 0
    }


setSurroundingPower : Power -> State -> State
setSurroundingPower surroundingPower state =
    { state | surroundingPower = surroundingPower }


reveal : State -> State
reveal state =
    { state | visible = True }


isVisible : State -> Bool
isVisible =
    .visible


hasZeroPower : State -> Bool
hasZeroPower =
    .power >> ((==) 0)


isMonster : State -> Bool
isMonster =
    not << hasZeroPower
