module Scene.ChooseVariant exposing (view)

import DecodeHelpers
import Game.Variant
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Maybe.Extra


view : { initializeGameMsg : Game.Variant.Identifier -> msg, viewHighScoresMsg : msg } -> Html msg
view options =
    let
        variantIdentifierFromFormDecoder =
            Decode.at [ "currentTarget", "variantIdentifier", "value" ] Decode.string
                |> DecodeHelpers.mapAndPair Game.Variant.stringToIdentifier
                |> Decode.andThen
                    (\( originalString, maybeIdentifier ) ->
                        Maybe.Extra.unwrap
                            (Decode.fail ("Unknown variant identifier " ++ originalString))
                            (Decode.succeed << options.initializeGameMsg)
                            maybeIdentifier
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
                    , [ div []
                            [ button
                                [ type_ "button"
                                , onClick options.viewHighScoresMsg
                                ]
                                [ text "High scores" ]
                            ]
                      ]
                    ]
                )
            ]
        ]


viewVariants : List (Html msg)
viewVariants =
    List.map
        (\( variantIdentifier, variant ) ->
            let
                labelText =
                    Game.Variant.identifierToString variantIdentifier
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
        Game.Variant.playableVariants
