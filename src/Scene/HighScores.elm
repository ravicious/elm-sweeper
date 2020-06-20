module Scene.HighScores exposing (Model, Msg(..), update, view)

import Game
import Game.GameResult as GameResult exposing (GameResult, Placing(..))
import Game.Variant
import Game.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import RemoteData
import Time


type alias Model =
    Game.Variant.Identifier


type Msg
    = ShowHighScoresFor String
    | GoBack


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        ShowHighScoresFor string ->
            ( Game.Variant.stringToIdentifier string |> Maybe.withDefault model
            , Cmd.none
            )

        GoBack ->
            -- Handled by Main.
            noop


view : GameResult.RemoteGameResults -> Model -> Html Msg
view gameResults variant =
    let
        emptyGame =
            Game.empty variant
    in
    Game.View.view
        { variant = Game.Variant.get variant
        , viewOverlay = Just ( "game-won", viewOverlay gameResults variant )
        , onCellClickMsg = Nothing
        , statusBarOptions = Nothing
        }
        emptyGame


viewOverlay : GameResult.RemoteGameResults -> Model -> List (Html Msg)
viewOverlay allGameResults variantIdentifier =
    let
        variantGameResults =
            allGameResults
                |> RemoteData.map (List.filter (.variant >> (==) variantIdentifier))
    in
    case variantGameResults of
        RemoteData.Success gameResults ->
            [ div [ class "cover whole-parent" ]
                [ h1 [ class "center grid-overlay__text" ] [ text "High Scores" ]
                , viewVariantSelect variantIdentifier
                , div [ class "centered center", style "overflow-y" "scroll", style "width" "100%" ] <|
                    if List.isEmpty gameResults then
                        [ p [ class "grid-overlay__text" ] [ text "No results yet, be the first to beat the game!" ] ]

                    else
                        [ viewResults gameResults ]
                , div [ class "center" ] [ button [ onClick GoBack, type_ "button" ] [ text "Main menu" ] ]
                ]
            ]

        RemoteData.Failure _ ->
            [ div [ class "cover whole-parent" ]
                [ h1 [ class "centered center grid-overlay__text" ] [ text "Something went wrong" ]
                ]
            ]

        _ ->
            [ div [ class "cover whole-parent" ]
                [ h1 [ class "centered center grid-overlay__text" ] [ text "Loading…" ]
                ]
            ]


viewVariantSelect : Game.Variant.Identifier -> Html Msg
viewVariantSelect currentVariantIdentifier =
    select [ onInput ShowHighScoresFor, class "center" ] <|
        List.map
            (\( variantIdentifier, _ ) ->
                option
                    [ value (Game.Variant.identifierToString variantIdentifier)
                    , selected (variantIdentifier == currentVariantIdentifier)
                    ]
                    [ text (Game.Variant.identifierToString variantIdentifier) ]
            )
            Game.Variant.playableVariants


viewResults : List GameResult -> Html Msg
viewResults gameResults =
    Html.Keyed.ul [ class "results grid-overlay__text" ] <| List.map viewResult <| GameResult.toLeaderboard gameResults


viewResult : ( Placing, GameResult ) -> ( String, Html Msg )
viewResult ( placing, gameResult ) =
    let
        placeText =
            case placing of
                Place place ->
                    String.fromInt place

                Tie place ->
                    "T-" ++ String.fromInt place
    in
    ( String.fromInt <| Time.posixToMillis gameResult.startedAt
    , li []
        [ span [ class "result__place" ]
            [ text placeText
            , text "."
            ]
        , span [ class "result__name" ] [ text gameResult.name ]
        , span [] [ Game.View.viewGameDuration gameResult.startedAt gameResult.endedAt ]
        ]
    )
