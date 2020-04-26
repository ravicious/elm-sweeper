port module Main exposing (main)

import Browser
import Game
import Game.Board
import Game.Cell as Cell
import Game.Direction exposing (Direction(..))
import Game.Event
import Game.Variant
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


renderCells : Game.State -> List (Html.Html Msg)
renderCells game =
    game
        |> Game.listCells
            (\( index, cell ) ->
                let
                    displayedValueDescriptor =
                        Cell.describeDisplayedValue cell

                    displayedValueClass =
                        "grid-cell--displayed-value-" ++ displayedValueDescriptor
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
                    , onClick (ClickCell index)
                    , attribute "data-index" (String.fromInt index)
                    ]
                    [ text <| Cell.toDisplayedValue cell ]
            )


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
    }


type Msg
    = ClickCell Game.Board.CellIndex
    | InitializeWithSeed Int
    | KeyPressedOverCell ( Game.Board.CellIndex, String )


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        seed =
            Random.initialSeed flags.randomNumber
    in
    ( { game = Game.init Game.Variant.SixteenByThirty seed
      , initialNumber = flags.randomNumber
      }
    , Cmd.none
    )


port initializeWithSeed : (Int -> msg) -> Sub msg


port keyPressedOverCell : (( Game.Board.CellIndex, String ) -> msg) -> Sub msg


port gameHasBeenLost : () -> Cmd msg


port emitGameEvents : List String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ initializeWithSeed InitializeWithSeed
        , keyPressedOverCell KeyPressedOverCell
        ]


keyCodeToDirection : String -> Maybe Direction
keyCodeToDirection string =
    case string of
        "KeyA" ->
            Just Left

        "ArrowLeft" ->
            Just Left

        "KeyD" ->
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
                ( updatedGame, emittedEvents ) =
                    Game.update (Game.TouchCell index) model.game

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

        KeyPressedOverCell ( index, keyCode ) ->
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

        InitializeWithSeed randomNumber ->
            init { randomNumber = randomNumber }


view : Model -> Html.Html Msg
view model =
    div []
        [ div [ class "grid grid--16x30" ] <|
            renderCells model.game
        , span []
            [ text <|
                "Seed: "
                    ++ String.fromInt model.initialNumber
                    ++ " Lvl: "
                    ++ (String.fromInt <| Game.getPlayerLevel model.game)
                    ++ " XP: "
                    ++ (String.fromInt <| Game.getPlayerXp model.game)
                    ++ " Next Lvl: "
                    ++ (Game.getXpNeededForNextLevel model.game
                            |> Maybe.map String.fromInt
                            |> Maybe.withDefault ""
                       )
                    ++ " HP: "
                    ++ (String.fromInt <| Game.getPlayerHp model.game)
            ]
        ]
