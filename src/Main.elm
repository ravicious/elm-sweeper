port module Main exposing (main)

import Assets
import Browser
import Dict
import Game
import Game.Board as Board
import Game.Cell as Cell
import Game.Cell.Content as Content
import Game.Direction exposing (Direction(..))
import Game.Event
import Game.GameResult as GameResult exposing (GameResult)
import Game.Variant
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra
import Random
import Task
import Time


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { randomNumber : Int
    }


type alias Model =
    { game : Game.State
    , initialNumber : Int
    , touchNeighbours : Bool
    , startedAt : Maybe Time.Posix
    , endedAt : Maybe Time.Posix
    , timeNow : Maybe Time.Posix
    }


type KeyDirection
    = Up
    | Down


type KeyEvent
    = KeyEvent KeyDirection String (Maybe Board.CellIndex)


makeKeyEvent : KeyDirection -> ( String, Maybe Board.CellIndex ) -> KeyEvent
makeKeyEvent keyDirection ( keyCode, maybeIndex ) =
    KeyEvent keyDirection keyCode maybeIndex


type Msg
    = ClickCell Board.CellIndex
    | InitializeWithSeed Int
    | KeyEventReceived KeyEvent
    | StartTimer Time.Posix
    | UpdateTimeNow Time.Posix
    | GameResultNameReceived String


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        seed =
            Random.initialSeed flags.randomNumber
    in
    ( { game = Game.init Game.Variant.Tiny seed
      , initialNumber = flags.randomNumber
      , touchNeighbours = False
      , startedAt = Nothing
      , endedAt = Nothing
      , timeNow = Nothing
      }
    , Cmd.none
    )


port initializeWithSeed : (Int -> msg) -> Sub msg


port keyUp : (( String, Maybe Board.CellIndex ) -> msg) -> Sub msg


port keyDown : (( String, Maybe Board.CellIndex ) -> msg) -> Sub msg


port receiveGameResultName : (String -> msg) -> Sub msg


port emitGameEvents : List String -> Cmd msg


port saveGameResult : Json.Decode.Value -> Cmd msg


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
        [ initializeWithSeed InitializeWithSeed
        , keyUp (KeyEventReceived << makeKeyEvent Up)
        , keyDown (KeyEventReceived << makeKeyEvent Down)
        , receiveGameResultName GameResultNameReceived
        , timerSubscription
        ]


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

        InitializeWithSeed randomNumber ->
            init { randomNumber = randomNumber }

        StartTimer time ->
            ( { model | startedAt = Just time }, Cmd.none )

        UpdateTimeNow time ->
            ( { model | timeNow = Just time }, Cmd.none )

        GameResultNameReceived name ->
            let
                saveGameResultCmd =
                    modelToGameResult name model
                        |> Maybe.map GameResult.encode
                        |> Maybe.map saveGameResult
                        |> Maybe.withDefault Cmd.none
            in
            ( model, saveGameResultCmd )


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


modelToGameResult : String -> Model -> Maybe GameResult
modelToGameResult name model =
    Just GameResult
        |> Maybe.Extra.andMap (Game.Variant.toIdentifier model.game.variant)
        |> Maybe.Extra.andMap (Just name)
        |> Maybe.Extra.andMap (Just model.initialNumber)
        |> Maybe.Extra.andMap model.startedAt
        |> Maybe.Extra.andMap model.endedAt


view : Model -> Html.Html Msg
view model =
    div []
        [ gridStyle model.game.variant
        , div [ class "stack" ]
            [ div [ id "grid", class "grid" ] <|
                renderCells model.game
            , div [ class "cluster bar" ]
                [ div [ style "align-items" "flex-start", style "justify-content" "space-evenly" ]
                    [ viewStatus model
                    , viewMonsterSummary (Game.toMonsterSummary model.game)
                    ]
                ]
            , span [ class "seed" ] [ text <| "Seed: " ++ String.fromInt model.initialNumber ]
            ]
        ]


gridStyle : Game.Variant.Variant -> Html Msg
gridStyle variant =
    let
        fontSize =
            if variant.columns > 30 then
                "1.5vw"

            else
                "2vw"

        styles =
            """
        .grid {
          font-size: <font-size>;
          grid-template-rows: repeat(<rows>, <rows>fr);
          grid-template-columns: repeat(<columns>, <columns>fr);
          /* Make sure that we render square cells */
          height: calc((100vw / <columns>) * <rows>);
        }
      """
                |> String.replace "<font-size>" fontSize
                |> String.replace "<rows>" (String.fromInt variant.rows)
                |> String.replace "<columns>" (String.fromInt variant.columns)
    in
    node "style" [] [ text styles ]


viewStatus : Model -> Html Msg
viewStatus model =
    let
        game =
            model.game
    in
    div [ class "cluster" ]
        [ ul [ class "status" ]
            [ li [ class "status-item" ]
                [ text "Lvl"
                , span [] [ text <| String.fromInt <| Game.getPlayerLevel game ]
                ]
            , li [ class "status-item" ]
                [ text "XP"
                , span (minSpanWidth "3ch") [ text <| String.fromInt <| Game.getPlayerXp game ]
                ]
            , li [ class "status-item" ]
                [ text "Next"
                , span (minSpanWidth "3ch")
                    [ Game.getXpNeededForNextLevel game
                        |> Maybe.map String.fromInt
                        |> Maybe.withDefault "0"
                        |> text
                    ]
                ]
            , li [ class "status-item" ]
                [ text "HP"
                , span (minSpanWidth "2ch") [ text <| String.fromInt <| Game.getPlayerHp game ]
                ]
            , li [ class "status-item" ]
                [ text "Time"
                , viewGameDuration model.startedAt model.endedAt model.timeNow
                ]
            ]
        ]


minSpanWidth : String -> List (Html.Attribute msg)
minSpanWidth s =
    [ style "display" "inline-block", style "min-width" s ]


viewMonsterSummary : Board.MonsterSummary -> Html Msg
viewMonsterSummary monsterSummary =
    div [ class "cluster" ]
        [ ul [ class "monster-summary", style "justify-content" "center" ]
            (Dict.toList
                monsterSummary
                |> List.map
                    (\( power, count ) ->
                        li [ class "monster-summary-item" ]
                            [ text <| "Lvl " ++ String.fromInt power
                            , span [ class "monster-summary-item-count" ]
                                [ img [ src (Assets.monsterSrc power), alt "" ] []
                                , span [ class "monster-summary-item-count-int" ] [ text <| " " ++ String.fromInt count ]
                                ]
                            ]
                    )
            )
        ]


renderCells : Game.State -> List (Html.Html Msg)
renderCells game =
    game
        |> Game.listCells
            (\( index, cell ) ->
                let
                    content =
                        Cell.toContent cell

                    contentDescription =
                        Content.toDescription content

                    displayedValueClass =
                        "grid-cell--displayed-value-" ++ contentDescription
                in
                div
                    [ classList
                        [ ( "grid-cell", True )
                        , ( displayedValueClass, True )
                        , ( "grid-cell--zero-power", Cell.isRevealed cell && Cell.hasZeroPower cell )
                        , ( "grid-cell--zero-surrounding-power", Cell.isRevealed cell && Cell.hasZeroSurroundingPower cell )
                        , ( "grid-cell--monster", Cell.isRevealed cell && Cell.isMonster cell )
                        , ( "is-revealed", Cell.isRevealed cell )
                        , ( "is-not-revealed", not <| Cell.isRevealed cell )
                        , ( "is-touchable", Cell.isTouchable cell )
                        , ( "is-not-touchable", not <| Cell.isTouchable cell )
                        ]

                    -- Revealed cells should still be clickable, otherwise we wouldn't be able to
                    -- toggle between showing power and surrounding power of monster cells.
                    , onClick (ClickCell index)
                    , attribute "data-index" (String.fromInt index)
                    ]
                    [ contentToHtml content ]
            )


contentToHtml : Content.Content -> Html Msg
contentToHtml content =
    case content of
        Content.Power power ->
            if power <= 9 then
                img [ src (Assets.monsterSrc power), alt ("Lvl " ++ String.fromInt power) ] []

            else
                text <| String.fromInt power

        Content.SurroundingPower surroundingPower ->
            text <| String.fromInt surroundingPower

        Content.Bet bet ->
            text <| String.fromInt bet

        Content.Nothing ->
            text ""


viewGameDuration : Maybe Time.Posix -> Maybe Time.Posix -> Maybe Time.Posix -> Html Msg
viewGameDuration maybeStartedAt maybeEndedAt maybeTimeNow =
    let
        formatNumber =
            String.fromInt >> String.padLeft 2 '0'

        calculateDuration from to =
            let
                secondsSinceStart =
                    (Time.posixToMillis to - Time.posixToMillis from) // 1000
            in
            ( secondsSinceStart // 60, remainderBy 60 secondsSinceStart )

        ( minutes, seconds ) =
            case ( maybeStartedAt, maybeEndedAt, maybeTimeNow ) of
                ( Just startedAt, Just endedAt, _ ) ->
                    calculateDuration startedAt endedAt

                ( Just startedAt, Nothing, Just timeNow ) ->
                    calculateDuration startedAt timeNow

                _ ->
                    ( 0, 0 )
    in
    span []
        [ text <| formatNumber minutes
        , small [] [ text <| formatNumber seconds ]
        ]
