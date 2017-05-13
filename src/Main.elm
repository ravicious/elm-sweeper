module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)


cells =
    List.range 1 (16 * 30)
        |> List.map
            (\n ->
                let
                    power =
                        n % 6
                in
                    div [ class "grid-cell" ]
                        [ if power == 0 then
                            text
                                ""
                          else
                            text (toString power)
                        ]
            )


main : Html.Html msg
main =
    div [ class "grid grid--16x30" ]
        cells
