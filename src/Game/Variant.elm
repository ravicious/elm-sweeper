module Game.Variant exposing
    ( Identifier(..)
    , Variant
    , get
    , identifierDecoder
    , identifierToString
    , playableVariants
    , stringToIdentifier
    , toIdentifier
    )

import DecodeHelpers as DecodeH
import Game.ExpProgression as ExpProgression
import Json.Decode as Decode
import List.Extra


type Identifier
    = Normal
    | Huge
    | Tiny


type alias Variant =
    { rows : Int
    , columns : Int
    , minPower : Int
    , maxPower : Int
    , cellConfiguration : List ( Int, Int )
    , expProgression : ExpProgression.ExpProgression
    , initialPlayerHp : Int
    }


get : Identifier -> Variant
get identifier =
    case identifier of
        Normal ->
            normal

        Huge ->
            huge

        Tiny ->
            tiny


allVariants : List ( Identifier, Variant )
allVariants =
    [ ( Normal, normal ), ( Huge, huge ), ( Tiny, tiny ) ]


playableVariants : List ( Identifier, Variant )
playableVariants =
    let
        filteredOutVariants =
            []

        -- [ Tiny ]
    in
    List.filter (Tuple.first >> (\a -> List.member a filteredOutVariants) >> not) allVariants


toIdentifier : Variant -> Maybe Identifier
toIdentifier variant =
    List.Extra.find (Tuple.second >> (==) variant) allVariants
        |> Maybe.map Tuple.first


identifierToString : Identifier -> String
identifierToString identifier =
    case identifier of
        Normal ->
            "Normal"

        Huge ->
            "Huge"

        Tiny ->
            "Tiny"


stringToIdentifier : String -> Maybe Identifier
stringToIdentifier string =
    case string of
        "Normal" ->
            Just Normal

        "Huge" ->
            Just Huge

        "Tiny" ->
            Just Tiny

        _ ->
            Nothing


identifierDecoder : Decode.Decoder Identifier
identifierDecoder =
    Decode.string
        |> DecodeH.mapAndPair stringToIdentifier
        |> Decode.andThen
            (\( originalString, maybeIdentifier ) ->
                case maybeIdentifier of
                    Just identifier ->
                        Decode.succeed identifier

                    Nothing ->
                        Decode.fail <|
                            "Couldn't decode given string into game variant identifier: "
                                ++ originalString
            )


tiny : Variant
tiny =
    { rows = 6
    , columns = 10
    , minPower = 1
    , maxPower = 1
    , initialPlayerHp = 10
    , cellConfiguration = [ ( 1, 5 ) ]
    , expProgression = ExpProgression.init [ ( 1, 5 ) ]
    }


normal : Variant
normal =
    { rows = 16
    , columns = 30
    , minPower = 1
    , maxPower = 5
    , initialPlayerHp = 10
    , cellConfiguration =
        -- The configuration has been taken from the original game.
        [ ( 1, 33 ), ( 2, 27 ), ( 3, 20 ), ( 4, 13 ), ( 5, 6 ) ]
    , expProgression =
        ExpProgression.init [ ( 1, 10 ), ( 2, 50 ), ( 3, 167 ), ( 4, 271 ) ]
    }


huge : Variant
huge =
    { rows = 25
    , columns = 50
    , minPower = 1
    , maxPower = 9
    , initialPlayerHp = 30
    , cellConfiguration =
        [ ( 1, 52 ), ( 2, 46 ), ( 3, 40 ), ( 4, 36 ), ( 5, 30 ), ( 6, 24 ), ( 7, 18 ), ( 8, 13 ), ( 9, 1 ) ]
    , expProgression =
        ExpProgression.init [ ( 1, 10 ), ( 2, 90 ), ( 3, 202 ), ( 4, 400 ), ( 5, 1072 ), ( 6, 1840 ), ( 7, 2992 ), ( 8, 4656 ) ]
    }
