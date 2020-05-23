port module Main exposing (main)

import Browser
import Game
import Game.Direction exposing (Direction(..))
import Game.GameResult as GameResult
import Game.Variant
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Random
import RemoteData exposing (RemoteData)
import Scene.ChooseVariant
import Scene.Game
import Scene.GameWon


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
    , gameResults : GameResult.RemoteGameResults
    }


type Scene
    = ChooseVariant
    | Game Scene.Game.Model
    | GameWon Scene.GameWon.Model


type Msg
    = InitializeGame Game.Variant.Identifier
    | GameSceneMsg Scene.Game.Msg
    | GameWonSceneMsg Scene.GameWon.Msg
    | GameResultsReceived Decode.Value


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { scene = ChooseVariant
      , seed = Random.initialSeed flags.randomNumber
      , gameResults = RemoteData.NotAsked
      }
    , Cmd.none
    )



-- Ports for subscriptions


port receiveGameResults : (Decode.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        sceneSubs =
            case model.scene of
                ChooseVariant ->
                    Sub.none

                Game sceneModel ->
                    Sub.map GameSceneMsg (Scene.Game.subscriptions sceneModel)

                GameWon sceneModel ->
                    Sub.map GameWonSceneMsg (Scene.GameWon.subscriptions sceneModel)
    in
    Sub.batch
        [ sceneSubs
        , receiveGameResults GameResultsReceived
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case ( model.scene, msg ) of
        ( _, GameResultsReceived rawGameResults ) ->
            let
                gameResults =
                    Decode.decodeValue (Decode.list GameResult.decoder) rawGameResults
                        |> RemoteData.fromResult

                ( newSceneModel, sceneCmds ) =
                    case model.scene of
                        GameWon sceneModel ->
                            Scene.GameWon.update (Scene.GameWon.GameResultsReceived gameResults) sceneModel
                                |> Tuple.mapFirst GameWon
                                |> Tuple.mapSecond (Cmd.map GameWonSceneMsg)

                        _ ->
                            ( model.scene, Cmd.none )
            in
            ( { model | gameResults = gameResults, scene = newSceneModel }, sceneCmds )

        ( ChooseVariant, InitializeGame variantIdentifier ) ->
            let
                ( intSeed, nextSeed ) =
                    Random.step intSeedGenerator model.seed

                ( sceneModel, sceneCmd ) =
                    Scene.Game.init { intSeed = intSeed, variantIdentifier = variantIdentifier }
            in
            ( { model
                | scene = Game sceneModel
                , seed = nextSeed
              }
            , Cmd.map GameSceneMsg sceneCmd
            )

        ( ChooseVariant, _ ) ->
            noop

        ( Game gameSceneModel, GameSceneMsg gameSceneMsg ) ->
            let
                ( newGameSceneModel, gameSceneCmds ) =
                    Scene.Game.update gameSceneMsg gameSceneModel
                        |> Tuple.mapSecond (Cmd.map GameSceneMsg)
            in
            case Scene.Game.modelToGameStatus newGameSceneModel of
                Game.Won ->
                    case Scene.Game.modelToGameResult newGameSceneModel of
                        Just gameResult ->
                            let
                                ( gameWonModel, gameWonCmds ) =
                                    Scene.GameWon.init newGameSceneModel.game gameResult
                                        |> Tuple.mapSecond (Cmd.map GameWonSceneMsg)
                            in
                            ( { model | scene = GameWon gameWonModel }
                            , Cmd.batch
                                [ gameSceneCmds
                                , gameWonCmds
                                ]
                            )

                        Nothing ->
                            ( { model | scene = Game newGameSceneModel }, gameSceneCmds )

                _ ->
                    ( { model | scene = Game newGameSceneModel }, gameSceneCmds )

        ( Game _, _ ) ->
            noop

        ( GameWon _, GameWonSceneMsg Scene.GameWon.PlayAgain ) ->
            ( { model | scene = ChooseVariant }, Cmd.none )

        ( GameWon sceneModel, GameWonSceneMsg sceneMsg ) ->
            let
                ( newSceneModel, sceneCmds ) =
                    Scene.GameWon.update sceneMsg sceneModel
                        |> Tuple.mapFirst GameWon
                        |> Tuple.mapSecond (Cmd.map GameWonSceneMsg)
            in
            ( { model | scene = newSceneModel }, sceneCmds )

        ( GameWon _, _ ) ->
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

        GameWon sceneModel ->
            Html.map GameWonSceneMsg <| Scene.GameWon.view model.gameResults sceneModel
