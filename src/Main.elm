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
import RemoteData
import Scene.ChooseVariant
import Scene.Game
import Scene.GameOver
import Scene.GameWon
import Scene.HighScores


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
    , rawGameResults : Decode.Value
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
    | GameOver Scene.GameOver.Model
    | HighScores Scene.HighScores.Model


type Msg
    = InitializeGame Game.Variant.Identifier
    | GameSceneMsg Scene.Game.Msg
    | GameWonSceneMsg Scene.GameWon.Msg
    | GameResultsReceived Decode.Value
    | HighScoresSceneMsg Scene.HighScores.Msg
    | ViewHighScores
    | OpenChooseVariantScene


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        gameResults =
            Decode.decodeValue (Decode.list GameResult.decoder) flags.rawGameResults
                |> RemoteData.fromResult
    in
    ( { scene = ChooseVariant
      , seed = Random.initialSeed flags.randomNumber
      , gameResults = gameResults
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
                Game sceneModel ->
                    Sub.map GameSceneMsg (Scene.Game.subscriptions sceneModel)

                GameWon sceneModel ->
                    Sub.map GameWonSceneMsg (Scene.GameWon.subscriptions sceneModel)

                _ ->
                    Sub.none
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
                    -- For certain scenes, we need to fire some commands after the results get
                    -- loaded.
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

        ( ChooseVariant, ViewHighScores ) ->
            ( { model | scene = HighScores Game.Variant.Tiny }, Cmd.none )

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

                Game.Lost ->
                    case ( newGameSceneModel.startedAt, newGameSceneModel.endedAt ) of
                        ( Just startedAt, Just endedAt ) ->
                            ( { model
                                | scene =
                                    GameOver
                                        { game = newGameSceneModel.game
                                        , intSeed = newGameSceneModel.intSeed
                                        , startedAt = startedAt
                                        , endedAt = endedAt
                                        }
                              }
                            , gameSceneCmds
                            )

                        _ ->
                            ( { model | scene = Game newGameSceneModel }, gameSceneCmds )

                Game.InProgress ->
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

        ( HighScores _, HighScoresSceneMsg Scene.HighScores.GoBack ) ->
            ( { model | scene = ChooseVariant }, Cmd.none )

        ( HighScores sceneModel, HighScoresSceneMsg sceneMsg ) ->
            let
                ( newSceneModel, sceneCmds ) =
                    Scene.HighScores.update sceneMsg sceneModel
                        |> Tuple.mapFirst HighScores
                        |> Tuple.mapSecond (Cmd.map HighScoresSceneMsg)
            in
            ( { model | scene = newSceneModel }, sceneCmds )

        ( HighScores _, _ ) ->
            noop

        ( GameOver _, OpenChooseVariantScene ) ->
            ( { model | scene = ChooseVariant }, Cmd.none )

        ( GameOver _, _ ) ->
            noop


intSeedGenerator : Random.Generator Int
intSeedGenerator =
    Random.int 0 Random.maxInt


view : Model -> Html Msg
view model =
    case model.scene of
        ChooseVariant ->
            Scene.ChooseVariant.view
                { initializeGameMsg = InitializeGame
                , viewHighScoresMsg = ViewHighScores
                }

        Game sceneModel ->
            Html.map GameSceneMsg <| Scene.Game.view sceneModel

        GameWon sceneModel ->
            Html.map GameWonSceneMsg <| Scene.GameWon.view model.gameResults sceneModel

        HighScores sceneModel ->
            Html.map HighScoresSceneMsg <| Scene.HighScores.view model.gameResults sceneModel

        GameOver sceneModel ->
            Scene.GameOver.view { playAgainMsg = OpenChooseVariantScene } sceneModel
