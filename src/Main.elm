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
import Game.Variant
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


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


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        seed =
            Random.initialSeed flags.randomNumber
    in
    ( { game = Game.init Game.Variant.Normal seed
      , initialNumber = flags.randomNumber
      , touchNeighbours = False
      }
    , Cmd.none
    )


port initializeWithSeed : (Int -> msg) -> Sub msg


port keyUp : (( String, Maybe Board.CellIndex ) -> msg) -> Sub msg


port keyDown : (( String, Maybe Board.CellIndex ) -> msg) -> Sub msg


port gameHasBeenLost : () -> Cmd msg


port emitGameEvents : List String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ initializeWithSeed InitializeWithSeed
        , keyUp (KeyEventReceived << makeKeyEvent Up)
        , keyDown (KeyEventReceived << makeKeyEvent Down)
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

                newModel =
                    { model | game = updatedGame }
            in
            ( newModel
            , Cmd.batch
                [ emitGameEvents <| List.map Game.Event.toString emittedEvents
                , if Game.hasBeenLost newModel.game then
                    gameHasBeenLost ()

                  else
                    Cmd.none
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


view : Model -> Html.Html Msg
view model =
    div []
        [ gridStyle model.game.variant
        , div [ class "stack" ]
            [ div [ id "grid", class "grid" ] <|
                renderCells model.game
            , div [ class "cluster bar" ]
                [ div [ style "align-items" "flex-start", style "justify-content" "space-evenly" ]
                    [ viewStatus model.game
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


viewStatus : Game.State -> Html Msg
viewStatus game =
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
                        |> Maybe.withDefault ""
                        |> text
                    ]
                ]
            , li [ class "status-item" ]
                [ text "HP"
                , span (minSpanWidth "2ch") [ text <| String.fromInt <| Game.getPlayerHp game ]
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
