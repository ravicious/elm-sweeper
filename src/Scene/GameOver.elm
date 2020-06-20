module Scene.GameOver exposing (Model, view)

import Game
import Game.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time


type alias Model =
    { game : Game.State
    , intSeed : Int
    , startedAt : Time.Posix
    , endedAt : Time.Posix
    }


type alias Options msg =
    { playAgainMsg : msg
    }


view : Options msg -> Model -> Html msg
view options model =
    Game.View.view
        { variant = model.game.variant
        , viewOverlay = Just ( "game-won", viewOverlay options model )
        , onCellClickMsg = Nothing
        , statusBarOptions =
            Just
                { intSeed = model.intSeed
                , duration = model
                }
        }
        model.game


viewOverlay : Options msg -> Model -> List (Html msg)
viewOverlay options _ =
    [ div [ class "cover whole-parent" ]
        [ h1 [ class "center grid-overlay__text" ] [ text "Game over!" ]
        , div [ class "centered center" ]
            [ button [ onClick options.playAgainMsg, type_ "button" ] [ text "Play again" ]
            ]
        ]
    ]
