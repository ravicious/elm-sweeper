module Game.GameResult exposing (GameResult, encode)

import Game.Variant
import Json.Encode as Encode
import Random
import Time


type alias GameResult =
    { variant : Game.Variant.Identifier
    , name : String
    , seed : Int
    , startedAt : Time.Posix
    , endedAt : Time.Posix
    }


encode : GameResult -> Encode.Value
encode gameResult =
    Encode.object
        [ ( "variant", Encode.string <| Game.Variant.identifierToString gameResult.variant )
        , ( "name", Encode.string gameResult.name )
        , ( "seed", Encode.int gameResult.seed )
        , ( "startedAt", Encode.int <| Time.posixToMillis gameResult.startedAt )
        , ( "endedAt", Encode.int <| Time.posixToMillis gameResult.endedAt )
        ]
