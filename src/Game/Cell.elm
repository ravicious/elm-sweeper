module Game.Cell exposing (init, State)


type alias Power =
    Int


type alias State =
    { power : Power
    }


init : Power -> State
init power =
    { power = power
    }
