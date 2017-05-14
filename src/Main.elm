module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


-- Our imports

import Game
import Game.Variant
import Game.Board
import Game.Cell as Cell


renderCells : Game.State -> List (Html.Html Msg)
renderCells game =
    game
        |> Game.listCells
            (\( index, cell ) ->
                let
                    textToDisplay =
                        if Cell.isVisible cell then
                            if Cell.isMonster cell then
                                toString cell.power
                            else
                                toString cell.surroundingPower
                        else
                            ""
                in
                    div
                        [ classList
                            [ ( "grid-cell", True )
                            , ( "grid-cell--zero-power", (Cell.hasZeroPower cell) )
                            , ( "grid-cell--monster", (Cell.isMonster cell) )
                            , ( "is-visible", (Cell.isVisible cell) )
                            ]
                        , onClick (ClickCell index)
                        ]
                        [ text textToDisplay ]
            )


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
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


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        seed =
            Random.initialSeed flags.randomNumber
    in
        { game = Game.init Game.Variant.SixteenByThirty seed
        , initialNumber = flags.randomNumber
        }
            ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickCell index ->
            { model | game = Game.update (Game.RevealCell index) model.game } ! []


view : Model -> Html.Html Msg
view model =
    let
        game =
            Game.init Game.Variant.SixteenByThirty
    in
        div []
            [ div [ class "grid grid--16x30" ] <|
                renderCells model.game
            , span [] [ text <| "Seed: " ++ (toString model.initialNumber) ]
            ]
