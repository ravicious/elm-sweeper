module DecodeHelpers exposing (mapAndPair, millisDecoder)

import Json.Decode as Decode
import Time


mapAndPair : (a -> b) -> Decode.Decoder a -> Decode.Decoder ( a, b )
mapAndPair f decoder =
    decoder |> Decode.map (\a -> ( a, a )) |> Decode.map (Tuple.mapSecond f)


millisDecoder : Decode.Decoder Time.Posix
millisDecoder =
    Decode.int
        |> Decode.map Time.millisToPosix
