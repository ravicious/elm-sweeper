module Game.GameResult exposing
    ( GameResult
    , Placing(..)
    , Timing
    , calculatePlacing
    , decoder
    , encode
    , encodePlacing
    )

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


type alias Timing =
    { startedAt : Time.Posix
    , endedAt : Time.Posix
    }


type Placing
    = Place Int
    | Tie Int
    | NoPreviousResults


encode : GameResult -> Encode.Value
encode gameResult =
    Encode.object
        [ ( "variant", Encode.string <| Game.Variant.identifierToString gameResult.variant )
        , ( "name", Encode.string gameResult.name )
        , ( "seed", Encode.int gameResult.seed )
        , ( "startedAt", Encode.int <| Time.posixToMillis gameResult.startedAt )
        , ( "endedAt", Encode.int <| Time.posixToMillis gameResult.endedAt )
        ]


encodePlacing : Placing -> Encode.Value
encodePlacing placing =
    case placing of
        NoPreviousResults ->
            Encode.object [ ( "type", Encode.string "NoPreviousResults" ) ]

        Place place ->
            Encode.object [ ( "type", Encode.string "Place" ), ( "place", Encode.int place ) ]

        Tie place ->
            Encode.object [ ( "type", Encode.string "Tie" ), ( "place", Encode.int place ) ]


decoder : Decode.Decoder GameResult
decoder =
    Decode.map5 GameResult
        (Decode.field "variant" Game.Variant.identifierDecoder)
        (Decode.field "name" Decode.string)
        (Decode.field "seed" Decode.int)
        (Decode.field "startedAt" DecodeH.millisDecoder)
        (Decode.field "endedAt" DecodeH.millisDecoder)


calculatePlacing : List GameResult -> { startedAt : Time.Posix, endedAt : Time.Posix } -> Placing
calculatePlacing gameResults timing =
    List.foldl
        (\gameResult placing ->
            case compareResults timing gameResult of
                LT ->
                    case placing of
                        NoPreviousResults ->
                            Place 1

                        _ ->
                            placing

                EQ ->
                    case placing of
                        Place place ->
                            Tie place

                        Tie _ ->
                            placing

                        NoPreviousResults ->
                            Tie 1

                GT ->
                    case placing of
                        Place place ->
                            Place (place + 1)

                        Tie place ->
                            Tie (place + 1)

                        NoPreviousResults ->
                            Place 2
        )
        NoPreviousResults
        gameResults


compareResults :
    { a | startedAt : Time.Posix, endedAt : Time.Posix }
    -> { b | startedAt : Time.Posix, endedAt : Time.Posix }
    -> Order
compareResults a b =
    compare (calculateTime a) (calculateTime b)


calculateTime : { a | startedAt : Time.Posix, endedAt : Time.Posix } -> Int
calculateTime x =
    (Time.posixToMillis x.endedAt - Time.posixToMillis x.startedAt) // 1000
