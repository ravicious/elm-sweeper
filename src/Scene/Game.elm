port module Scene.Game exposing
    ( KeyDirection
    , KeyEvent
    , Model
    , Msg
    , init
    , modelToGameResult
    , modelToGameStatus
    , subscriptions
    , update
    , view
    )

import Game
import Game.Board as Board
import Game.Direction exposing (Direction(..))
import Game.Event
import Game.GameResult as GameResult exposing (GameResult)
import Game.Variant
import Game.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra
import Random
import Task
import Time


type alias Model =
    { game : Game.State
    , intSeed : Int
    , touchNeighbours : Bool
    , startedAt : Maybe Time.Posix
    , endedAt : Maybe Time.Posix
    , timeNow : Maybe Time.Posix
    }


type Msg
    = ClickCell Board.CellIndex
    | KeyEventReceived KeyEvent
    | StartTimer Time.Posix
    | UpdateTimeNow Time.Posix


type KeyEvent
    = KeyEvent KeyDirection String (Maybe Board.CellIndex)


type KeyDirection
    = Up
    | Down


type alias Flags =
    { intSeed : Int
    , variantIdentifier : Game.Variant.Identifier
    }


init : Flags -> ( Model, Cmd Msg )
init { intSeed, variantIdentifier } =
    let
        seed =
            Random.initialSeed intSeed
    in
    ( { game = Game.init variantIdentifier seed
      , intSeed = intSeed
      , touchNeighbours = False
      , startedAt = Nothing
      , endedAt = Nothing
      , timeNow = Nothing
      }
    , Cmd.none
    )



-- Ports for commands


port emitGameEvents : List String -> Cmd msg



-- Ports for subscriptions


port keyUp : (( String, Maybe Board.CellIndex ) -> msg) -> Sub msg


port keyDown : (( String, Maybe Board.CellIndex ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        timerSubscription =
            if Maybe.Extra.isJust model.startedAt && Game.isInProgress model.game then
                Time.every 1000 UpdateTimeNow

            else
                Sub.none
    in
    Sub.batch
        [ keyUp (KeyEventReceived << makeKeyEvent Up)
        , keyDown (KeyEventReceived << makeKeyEvent Down)
        , timerSubscription
        ]


makeKeyEvent : KeyDirection -> ( String, Maybe Board.CellIndex ) -> KeyEvent
makeKeyEvent keyDirection ( keyCode, maybeIndex ) =
    KeyEvent keyDirection keyCode maybeIndex


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickCell index ->
            let
                action =
                    if model.touchNeighbours then
                        Game.TouchCellAndItsNeighbors

                    else
                        Game.TouchCell

                ( updatedGame, emittedEvents ) =
                    Game.update (action index) model.game

                updatedEndedAt =
                    case model.endedAt of
                        Just _ ->
                            model.endedAt

                        Nothing ->
                            if Game.hasEnded updatedGame then
                                -- Use `startedAt` in case the game ends after the first click,
                                -- `timeNow` would be `Nothing` at in that scenario.
                                Maybe.Extra.orList [ model.timeNow, model.startedAt ]

                            else
                                Nothing

                newModel =
                    { model | game = updatedGame, endedAt = updatedEndedAt }

                startTimerCmd =
                    if Maybe.Extra.isNothing model.startedAt then
                        Task.perform StartTimer Time.now

                    else
                        Cmd.none
            in
            ( newModel
            , Cmd.batch
                [ emitGameEvents <| List.map Game.Event.toString emittedEvents
                , startTimerCmd
                ]
            )

        KeyEventReceived keyEvent ->
            case keyEvent of
                KeyEvent keyDirection "KeyW" _ ->
                    updateTouchNeighbours keyDirection model

                KeyEvent keyDirection "KeyE" _ ->
                    updateTouchNeighbours keyDirection model

                KeyEvent keyDirection "ArrowUp" _ ->
                    updateTouchNeighbours keyDirection model

                KeyEvent Down keyCode (Just index) ->
                    keyCodeToDirection keyCode
                        |> Maybe.map
                            (\direction ->
                                ( { model | game = Game.update (Game.ChangeBet direction index) model.game |> Tuple.first }
                                , Cmd.none
                                )
                            )
                        |> Maybe.withDefault
                            ( model
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        StartTimer time ->
            ( { model | startedAt = Just time }, Cmd.none )

        UpdateTimeNow time ->
            ( { model | timeNow = Just time }, Cmd.none )


updateTouchNeighbours : KeyDirection -> Model -> ( Model, Cmd Msg )
updateTouchNeighbours keyDirection model =
    let
        touchNeighbours =
            case keyDirection of
                Up ->
                    False

                Down ->
                    True
    in
    ( { model | touchNeighbours = touchNeighbours }, Cmd.none )


modelToGameResult : Model -> Maybe GameResult
modelToGameResult model =
    Just GameResult
        |> Maybe.Extra.andMap (Game.Variant.toIdentifier model.game.variant)
        |> Maybe.Extra.andMap (Just "")
        |> Maybe.Extra.andMap (Just model.intSeed)
        |> Maybe.Extra.andMap model.startedAt
        |> Maybe.Extra.andMap model.endedAt


modelToGameStatus : Model -> Game.Status
modelToGameStatus =
    .game >> .status


keyCodeToDirection : String -> Maybe Direction
keyCodeToDirection string =
    case string of
        "KeyA" ->
            Just Left

        "KeyS" ->
            Just Left

        "ArrowLeft" ->
            Just Left

        "KeyD" ->
            Just Right

        "KeyF" ->
            Just Right

        "ArrowRight" ->
            Just Right

        _ ->
            Nothing


view : Model -> Html Msg
view model =
    let
        duration =
            case ( model.startedAt, model.endedAt, model.timeNow ) of
                ( Just startedAt, Just endedAt, _ ) ->
                    { startedAt = startedAt, endedAt = endedAt }

                ( Just startedAt, Nothing, Just timeNow ) ->
                    { startedAt = startedAt, endedAt = timeNow }

                _ ->
                    { startedAt = Time.millisToPosix 0, endedAt = Time.millisToPosix 0 }
    in
    Game.View.view
        { variant = model.game.variant
        , viewOverlay = Nothing
        , onCellClickMsg = Just ClickCell
        , statusBarOptions =
            Just
                { intSeed = model.intSeed
                , duration = duration
                }
        }
        model.game
