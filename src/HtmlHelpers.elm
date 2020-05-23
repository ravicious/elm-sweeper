module HtmlHelpers exposing (hideIf)

import Html
import Html.Attributes exposing (..)


hideIf : Bool -> Html.Attribute msg
hideIf bool =
    if bool then
        style "visibility" "hidden"

    else
        classList []
