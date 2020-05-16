module Main exposing (main)

import Browser
import Game.Direction exposing (Direction(..))
import Game.Variant
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Scene.Game


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
    { scene : Scene
    , seed : Random.Seed
    }


type Scene
    = ChooseVariant
    | Game Scene.Game.Model


type Msg
    = GameSceneMsg Scene.Game.Msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( sceneModel, sceneCmd ) =
            Scene.Game.init { intSeed = flags.randomNumber, variantIdentifier = Game.Variant.Normal }
    in
    ( { scene = Game sceneModel

      -- TODO: Rewrite seed and intSeed generation.
      , seed = Random.initialSeed flags.randomNumber
      }
    , Cmd.map GameSceneMsg sceneCmd
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.scene of
        ChooseVariant ->
            Sub.none

        Game sceneModel ->
            Sub.map GameSceneMsg (Scene.Game.subscriptions sceneModel)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case ( model.scene, msg ) of
        ( ChooseVariant, _ ) ->
            noop

        ( Game gameSceneModal, GameSceneMsg gameSceneMsg ) ->
            let
                ( newSceneModel, cmds ) =
                    Scene.Game.update gameSceneMsg gameSceneModal
            in
            ( { model | scene = Game newSceneModel }, Cmd.map GameSceneMsg cmds )


view : Model -> Html Msg
view model =
    case model.scene of
        ChooseVariant ->
            Debug.todo "Implement choose variant view"

        Game sceneModel ->
            Html.map GameSceneMsg <| Scene.Game.view sceneModel
