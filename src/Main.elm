module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Game


renderCells : Game.State -> List (Html.Html msg)
renderCells game =
    game
        |> Game.mapCells
            (\( index, cellState ) ->
                div [ class "grid-cell" ]
                    [ text (toString cellState.power)
                    ]
            )


main : Html.Html msg
main =
    let
        game =
            Game.init Game.SixteenByThirty
    in
        div [ class "grid grid--16x30" ] <|
            renderCells game
