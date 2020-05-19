module Main exposing (main)

import Browser
import Game
import Game.Direction exposing (Direction(..))
import Game.GameResult as GameResult exposing (GameResult)
import Game.Variant
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Scene.ChooseVariant
import Scene.Game
import Scene.GameWon
import Time


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
    | GameWon Scene.GameWon.Model


type Msg
    = InitializeGame Game.Variant.Identifier
    | GameSceneMsg Scene.Game.Msg
    | GameWonSceneMsg Scene.GameWon.Msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    -- ( { scene = ChooseVariant
    --   , seed = Random.initialSeed flags.randomNumber
    --   }
    -- , Cmd.none
    -- )
    let
        tenYearsInMillis =
            315360000000

        seed =
            Random.initialSeed flags.randomNumber

        timeNowInMillis =
            1589827565000

        startedAtGenerator =
            Random.int (tenYearsInMillis * -1) tenYearsInMillis
                |> Random.map ((+) timeNowInMillis)

        oneHourInMillis =
            3600000

        durationGenerator =
            Random.int 1001 oneHourInMillis

        ( ( startedAt, endedAt ), nextSeed ) =
            Random.step
                (Random.map2 Tuple.pair startedAtGenerator durationGenerator
                    |> Random.map
                        (\( startedAtInMillis, durationInMillis ) ->
                            ( Time.millisToPosix startedAtInMillis, Time.millisToPosix (startedAtInMillis + durationInMillis) )
                        )
                )
                seed

        game =
            Scene.Game.init { intSeed = 2, variantIdentifier = Game.Variant.Tiny } |> Tuple.first

        gameResult =
            GameResult Game.Variant.Tiny
                (startedAt
                    |> Time.posixToMillis
                    |> String.fromInt
                    |> String.right 3
                    |> (++) "Someone"
                )
                2
                startedAt
                endedAt

        ( sceneModel, sceneMsgs ) =
            Scene.GameWon.init game.game gameResult
    in
    ( { scene = GameWon sceneModel
      , seed = nextSeed
      }
    , Cmd.map GameWonSceneMsg sceneMsgs
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.scene of
        ChooseVariant ->
            Sub.none

        Game sceneModel ->
            Sub.map GameSceneMsg (Scene.Game.subscriptions sceneModel)

        GameWon sceneModel ->
            Sub.map GameWonSceneMsg (Scene.GameWon.subscriptions sceneModel)


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
            Html.map GameWonSceneMsg <| Scene.GameWon.view sceneModel
