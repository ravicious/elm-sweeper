module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Game
import Game.Variant
import Game.Board
import Game.Cell


renderCells : Game.State -> List (Html.Html Msg)
renderCells game =
    game
        |> Game.listCells
            (\( index, cell ) ->
                let
                    textToDisplay =
                        if Game.Cell.isVisible cell then
                            toString cell.power
                        else
                            ""
                in
                    div
                        [ classList
                            [ ( "grid-cell", True )
                            , ( "is-visible", (Game.Cell.isVisible cell) )
                            ]
                        , onClick (ClickCell index)
                        ]
                        [ text textToDisplay ]
            )


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }


type alias Model =
    { game : Game.State
    }


type Msg
    = ClickCell Game.Board.CellIndex


init : Model
init =
    { game = Game.init Game.Variant.SixteenByThirty
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickCell index ->
            { model | game = Game.update (Game.RevealCell index) model.game }


view : Model -> Html.Html Msg
view model =
    let
        game =
            Game.init Game.Variant.SixteenByThirty
    in
        div [ class "grid grid--16x30" ] <|
            renderCells model.game
