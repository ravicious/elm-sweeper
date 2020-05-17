module Main exposing (main)

import Browser
import Game.Direction exposing (Direction(..))
import Game.Variant
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Scene.ChooseVariant
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
    = InitializeGame Game.Variant.Identifier
    | GameSceneMsg Scene.Game.Msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { scene = ChooseVariant
      , seed = Random.initialSeed flags.randomNumber
      }
    , Cmd.none
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
        ( ChooseVariant, InitializeGame variantIdentifier ) ->
            let
                ( intSeed, nextSeed ) =
                    Random.step intSeedGenerator model.seed

                ( sceneModel, sceneCmd ) =
                    Scene.Game.init { intSeed = intSeed, variantIdentifier = variantIdentifier }
            in
            ( { scene = Game sceneModel
              , seed = nextSeed
              }
            , Cmd.map GameSceneMsg sceneCmd
            )

        ( ChooseVariant, _ ) ->
            noop

        ( Game gameSceneModal, GameSceneMsg gameSceneMsg ) ->
            let
                ( newSceneModel, cmds ) =
                    Scene.Game.update gameSceneMsg gameSceneModal
            in
            ( { model | scene = Game newSceneModel }, Cmd.map GameSceneMsg cmds )

        ( Game _, _ ) ->
            noop


intSeedGenerator : Random.Generator Int
intSeedGenerator =
    Random.int 0 Random.maxInt


view : Model -> Html Msg
view model =
    case model.scene of
        ChooseVariant ->
            Scene.ChooseVariant.view InitializeGame

        Game sceneModel ->
            Html.map GameSceneMsg <| Scene.Game.view sceneModel
