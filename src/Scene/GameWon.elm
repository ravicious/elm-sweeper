port module Scene.GameWon exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Dom
import Game
import Game.GameResult as GameResult exposing (GameResult, Placing(..))
import Game.Variant
import Game.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import HtmlHelpers exposing (..)
import Json.Decode as Decode
import RemoteData
import Task
import Time


type alias Model =
    { game : Game.State
    , gameResult : GameResult
    , name : Maybe String
    }


type Msg
    = GameResultsReceived GameResult.RemoteGameResults
    | UpdateName String
    | PlayAgain
    | NoOp


init : Game.State -> GameResult -> ( Model, Cmd Msg )
init game gameResult =
    let
        saveGameResultCmd =
            gameResult
                |> GameResult.encode
                |> saveGameResult
    in
    ( { game = game
      , gameResult = gameResult
      , name = Nothing
      }
    , saveGameResultCmd
    )



-- Ports for commands


port saveGameResult : Decode.Value -> Cmd msg


port updateNameInGameResult : ( Decode.Value, String ) -> Cmd msg



-- Ports for subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameResultsReceived gameResults ->
            let
                scrollToInputCmd =
                    if RemoteData.isSuccess gameResults then
                        Task.attempt (always NoOp) (Browser.Dom.focus resultNameInputId)

                    else
                        Cmd.none
            in
            ( model, scrollToInputCmd )

        UpdateName newName ->
            ( { model | gameResult = GameResult.updateName newName model.gameResult }
            , updateNameInGameResult ( GameResult.encode model.gameResult, newName )
            )

        PlayAgain ->
            -- This message is intercepted by Main before it gets passed to this update function.
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : GameResult.RemoteGameResults -> Model -> Html Msg
view gameResults model =
    Game.View.view
        { variant = Game.Variant.get model.gameResult.variant
        , viewOverlay = Just ( "game-won", viewOverlay gameResults model )
        , onCellClickMsg = Nothing
        , statusBarOptions =
            Just
                { intSeed = model.gameResult.seed
                , duration = model.gameResult
                }
        }
        model.game


viewOverlay : GameResult.RemoteGameResults -> Model -> List (Html Msg)
viewOverlay gameResults model =
    let
        viewGameResults =
            gameResults
                |> RemoteData.map (List.filter (.variant >> (==) model.gameResult.variant))
                |> RemoteData.unwrap (text "") (viewResults model.gameResult)
    in
    [ div [ class "cover whole-parent" ]
        [ h1 [ class "center grid-overlay__text" ] [ text "You did it! Game won!" ]
        , div [ class "centered center", style "overflow-y" "scroll", style "width" "100%" ]
            [ viewGameResults ]
        , div [ class "center", hideIf (String.isEmpty model.gameResult.name) ]
            [ button [ onClick PlayAgain, type_ "button" ] [ text "Play again" ]
            ]
        ]
    ]


viewResults : GameResult -> List GameResult -> Html Msg
viewResults currentGameResult gameResults =
    Html.Keyed.ul [ class "results grid-overlay__text" ] <| List.map (viewResult currentGameResult) <| GameResult.toLeaderboard gameResults


resultNameInputId : String
resultNameInputId =
    "result-name"


viewResult : GameResult -> ( Placing, GameResult ) -> ( String, Html Msg )
viewResult currentGameResult ( placing, gameResult ) =
    let
        isCurrentGameResult =
            gameResult == currentGameResult

        placeText =
            case placing of
                Place place ->
                    String.fromInt place

                Tie place ->
                    "T-" ++ String.fromInt place

        nameFromFormDecoder =
            Decode.at [ "currentTarget", "name", "value" ] Decode.string
                |> Decode.map UpdateName
                |> Decode.map (\a -> ( a, True ))
    in
    ( String.fromInt <| Time.posixToMillis gameResult.startedAt
    , li [ classList [ ( "result--current", isCurrentGameResult ) ] ]
        [ span [ class "result__place" ]
            [ text placeText
            , text "."
            ]
        , span [ class "result__name" ]
            [ if isCurrentGameResult && String.isEmpty gameResult.name then
                Html.form
                    [ preventDefaultOn "submit" nameFromFormDecoder
                    ]
                    [ input
                        [ name "name"
                        , id resultNameInputId
                        , required True
                        ]
                        []
                    ]

              else
                text gameResult.name
            ]
        , span [] [ Game.View.viewGameDuration gameResult.startedAt gameResult.endedAt ]
        ]
    )
