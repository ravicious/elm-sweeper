module Game.GameResult exposing
    ( GameResult
    , Placing(..)
    , RemoteGameResults
    , compare
    , decoder
    , encode
    , toLeaderboard
    , updateName
    )

import Array
import DecodeHelpers as DecodeH
import Game.Variant
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import RemoteData exposing (RemoteData)
import Time


type alias GameResult =
    { variant : Game.Variant.Identifier
    , name : String
    , seed : Int
    , startedAt : Time.Posix
    , endedAt : Time.Posix
    }


type alias RemoteGameResults =
    RemoteData Decode.Error (List GameResult)


type Placing
    = Place Int
    | Tie Int


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


compare : GameResult -> GameResult -> Order
compare a b =
    Basics.compare (calculateTime a) (calculateTime b)


isEqual : GameResult -> GameResult -> Bool
isEqual a b =
    case compare a b of
        EQ ->
            True

        _ ->
            False


calculateTime : { a | startedAt : Time.Posix, endedAt : Time.Posix } -> Int
calculateTime x =
    (Time.posixToMillis x.endedAt - Time.posixToMillis x.startedAt) // 1000


toLeaderboard : List GameResult -> List ( Placing, GameResult )
toLeaderboard gameResults =
    gameResults
        |> List.Extra.stableSortWith compare
        |> List.Extra.groupWhile isEqual
        |> List.foldl
            (\( firstInGroup, rest ) ( accResults, currentPlace ) ->
                case rest of
                    [] ->
                        ( Array.push ( Place currentPlace, firstInGroup ) accResults, currentPlace + 1 )

                    _ ->
                        ( firstInGroup
                            :: rest
                            |> List.map (Tuple.pair <| Tie currentPlace)
                            |> Array.fromList
                            |> Array.append accResults
                        , List.length rest + 1 + currentPlace
                        )
            )
            ( Array.empty, 1 )
        |> Tuple.first
        |> Array.toList


updateName : String -> GameResult -> GameResult
updateName newName gameResult =
    { gameResult | name = newName }
