module Game.GameResult exposing (GameResult, decoder, encode)

import DecodeHelpers as DecodeH
import Game.Variant
import Json.Decode as Decode
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


decoder : Decode.Decoder GameResult
decoder =
    Decode.map5 GameResult
        (Decode.field "variant" Game.Variant.identifierDecoder)
        (Decode.field "name" Decode.string)
        (Decode.field "seed" Decode.int)
        (Decode.field "startedAt" DecodeH.millisDecoder)
        (Decode.field "endedAt" DecodeH.millisDecoder)
