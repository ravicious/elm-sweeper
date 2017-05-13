module Game.Cell exposing (State, init, reveal, isVisible)


type alias Power =
    Int


type alias State =
    { power : Power
    , visible : Bool
    }


init : Power -> State
init power =
    { power = power
    , visible = False
    }


reveal : State -> State
reveal state =
    { state | visible = True }


isVisible : State -> Bool
isVisible =
    .visible
