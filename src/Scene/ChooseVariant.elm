module Scene.ChooseVariant exposing (view)

import DecodeHelpers
import Game.Variant
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode


view : (Game.Variant.Identifier -> msg) -> Html msg
view initializeGameMsg =
    let
        variantIdentifierFromFormDecoder =
            Decode.at [ "currentTarget", "variantIdentifier", "value" ] Decode.string
                |> DecodeHelpers.mapAndPair Game.Variant.stringToIdentifier
                |> Decode.andThen
                    (\( originalString, maybeIdentifier ) ->
                        maybeIdentifier
                            |> Maybe.map initializeGameMsg
                            |> Maybe.map Decode.succeed
                            |> Maybe.withDefault
                                (Decode.fail ("Unknown variant identifier " ++ originalString))
                    )
                |> Decode.map (\a -> ( a, True ))
    in
    div [ class "cover" ]
        [ div [ class "centered center" ]
            [ h1 [] [ text "Elm Sweeper" ]
            , Html.form
                [ class "stack smaller"
                , preventDefaultOn "submit" variantIdentifierFromFormDecoder
                ]
                (List.concat
                    [ [ p [] [ text "Choose mode:" ] ]
                    , viewVariants
                    , [ div [] [ button [ type_ "submit" ] [ text "Start game" ] ]
                      ]
                    ]
                )
            ]
        ]


viewVariants : List (Html msg)
viewVariants =
    let
        variants =
            [ ( Game.Variant.Normal, "Normal" ), ( Game.Variant.Huge, "Huge" ) ]
    in
    List.map
        (\( variantIdentifier, labelText ) ->
            let
                variant =
                    Game.Variant.get variantIdentifier
            in
            label [ class "cluster smaller" ]
                [ div []
                    [ input
                        [ type_ "radio"
                        , name "variantIdentifier"
                        , value (Game.Variant.identifierToString variantIdentifier)
                        , required True
                        ]
                        []
                    , p [] [ text labelText ]
                    , p [] [ text <| " " ++ String.fromInt variant.columns ++ "×" ++ String.fromInt variant.rows ]
                    ]
                ]
        )
        variants
