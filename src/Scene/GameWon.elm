port module Scene.GameWon exposing (Model, Msg, init, subscriptions, update, view)

import Assets
import Browser.Dom
import Dict
import Game
import Game.Board as Board
import Game.Cell as Cell
import Game.Cell.Content as Content
import Game.GameResult as GameResult exposing (GameResult, Placing(..))
import Game.Variant
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Json.Decode as Decode
import RemoteData exposing (RemoteData)
import Task
import Time


type alias Model =
    { game : Game.State
    , gameResult : GameResult
    , name : Maybe String
    , gameResults : GameResults
    }


type alias GameResults =
    RemoteData Decode.Error (List GameResult)


type Msg
    = GameResultsReceived Decode.Value
    | NoOp


init : Game.State -> GameResult -> ( Model, Cmd Msg )
init game gameResult =
    let
        saveGameResultCmd =
            gameResult
                |> GameResult.encode
                |> saveGameResult
    in
    ( { game = game
      , gameResult = gameResult
      , name = Nothing
      , gameResults = RemoteData.NotAsked
      }
    , saveGameResultCmd
    )



-- Ports for commands


port saveGameResult : Decode.Value -> Cmd msg



-- Ports for subscriptions


port receiveGameResults : (Decode.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveGameResults GameResultsReceived


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameResultsReceived rawGameResults ->
            let
                gameResults =
                    Decode.decodeValue (Decode.list GameResult.decoder) rawGameResults
                        |> Result.map (List.filter (.variant >> (==) model.gameResult.variant))
                        |> RemoteData.fromResult

                scrollToInputCmd =
                    if RemoteData.isSuccess gameResults then
                        Task.attempt (always NoOp) (Browser.Dom.focus resultNameInputId)

                    else
                        Cmd.none
            in
            ( { model | gameResults = gameResults }, scrollToInputCmd )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    viewBoard model


viewBoard : Model -> Html Msg
viewBoard model =
    div []
        [ gridStyle (Game.Variant.get model.gameResult.variant)
        , div [ class "stack" ]
            [ div [ class "grid-container" ]
                [ div [ class "grid-overlay game-won" ] (viewOverlay model)
                , div [ id "grid", class "grid" ] <|
                    renderCells model.game
                ]
            , div [ class "cluster bar" ]
                [ div [ style "align-items" "flex-start", style "justify-content" "space-evenly" ]
                    [ viewStatus model
                    , viewMonsterSummary (Game.toMonsterSummary model.game)
                    ]
                ]
            , span [ class "seed" ] [ text <| "Seed: " ++ String.fromInt model.gameResult.seed ]
            ]
        ]


viewOverlay : Model -> List (Html Msg)
viewOverlay model =
    let
        gameResults =
            model.gameResults
                |> RemoteData.map (viewResults model.gameResult)
                |> RemoteData.withDefault (text "")
    in
    [ h1 [] [ text "You did it! Game won!" ]
    , gameResults
    ]


viewResults : GameResult -> List GameResult -> Html Msg
viewResults currentGameResult gameResults =
    Html.Keyed.ul [ class "results" ] <| List.map (viewResult currentGameResult) <| GameResult.toLeaderboard gameResults


resultNameInputId =
    "result-name"


viewResult : GameResult -> ( Placing, GameResult ) -> ( String, Html Msg )
viewResult currentGameResult ( placing, gameResult ) =
    let
        name =
            String.right
                (gameResult.startedAt
                    |> Time.posixToMillis
                    |> String.fromInt
                    |> String.right 1
                    |> String.toInt
                    |> Maybe.map ((+) 2)
                    |> Maybe.withDefault 1
                )
                gameResult.name
                |> List.repeat 2
                |> String.join " "

        placeText =
            case placing of
                Place place ->
                    String.fromInt place

                Tie place ->
                    "T-" ++ String.fromInt place
    in
    ( String.fromInt <| Time.posixToMillis gameResult.startedAt
    , li []
        [ span [ class "result__place" ]
            [ text placeText
            , text "."
            ]
        , span []
            [ if gameResult == currentGameResult then
                Html.form []
                    [ input [ Html.Attributes.name "name", id resultNameInputId ] []
                    ]

              else
                text name
            ]
        , span [] [ viewGameDuration gameResult.startedAt gameResult.endedAt ]
        ]
    )


gridStyle : Game.Variant.Variant -> Html Msg
gridStyle variant =
    let
        fontSize =
            if variant.columns > 30 then
                "1.5vw"

            else
                "2vw"

        styles =
            """
        .grid {
          font-size: <font-size>;
          grid-template-rows: repeat(<rows>, <rows>fr);
          grid-template-columns: repeat(<columns>, <columns>fr);
          /* Make sure that we render square cells */
          height: calc((100vw / <columns>) * <rows>);
        }
      """
                |> String.replace "<font-size>" fontSize
                |> String.replace "<rows>" (String.fromInt variant.rows)
                |> String.replace "<columns>" (String.fromInt variant.columns)
    in
    node "style" [] [ text styles ]


viewStatus : Model -> Html Msg
viewStatus model =
    let
        game =
            model.game
    in
    div [ class "cluster" ]
        [ ul [ class "status" ]
            [ li [ class "status-item" ]
                [ text "Lvl"
                , span [] [ text <| String.fromInt <| Game.getPlayerLevel game ]
                ]
            , li [ class "status-item" ]
                [ text "XP"
                , span (minSpanWidth "3ch") [ text <| String.fromInt <| Game.getPlayerXp game ]
                ]
            , li [ class "status-item" ]
                [ text "Next"
                , span (minSpanWidth "3ch")
                    [ Game.getXpNeededForNextLevel game
                        |> Maybe.map String.fromInt
                        |> Maybe.withDefault "0"
                        |> text
                    ]
                ]
            , li [ class "status-item" ]
                [ text "HP"
                , span (minSpanWidth "2ch") [ text <| String.fromInt <| Game.getPlayerHp game ]
                ]
            , li [ class "status-item" ]
                [ text "Time"
                , viewGameDuration model.gameResult.startedAt model.gameResult.endedAt
                ]
            ]
        ]


minSpanWidth : String -> List (Html.Attribute msg)
minSpanWidth s =
    [ style "display" "inline-block", style "min-width" s ]


viewMonsterSummary : Board.MonsterSummary -> Html Msg
viewMonsterSummary monsterSummary =
    div [ class "cluster" ]
        [ ul [ class "monster-summary", style "justify-content" "center" ]
            (Dict.toList
                monsterSummary
                |> List.map
                    (\( power, count ) ->
                        li [ class "monster-summary-item" ]
                            [ text <| "Lvl " ++ String.fromInt power
                            , span [ class "monster-summary-item-count" ]
                                [ img [ src (Assets.monsterSrc power), alt "" ] []
                                , span [ class "monster-summary-item-count-int" ] [ text <| " " ++ String.fromInt count ]
                                ]
                            ]
                    )
            )
        ]


renderCells : Game.State -> List (Html.Html Msg)
renderCells game =
    game
        |> Game.listCells
            (\( index, cell ) ->
                let
                    content =
                        Cell.toContent cell

                    contentDescription =
                        Content.toDescription content

                    displayedValueClass =
                        "grid-cell--displayed-value-" ++ contentDescription
                in
                div
                    [ classList
                        [ ( "grid-cell", True )
                        , ( displayedValueClass, True )
                        , ( "grid-cell--zero-power", Cell.isRevealed cell && Cell.hasZeroPower cell )
                        , ( "grid-cell--zero-surrounding-power", Cell.isRevealed cell && Cell.hasZeroSurroundingPower cell )
                        , ( "grid-cell--monster", Cell.isRevealed cell && Cell.isMonster cell )
                        , ( "is-revealed", Cell.isRevealed cell )
                        , ( "is-not-revealed", not <| Cell.isRevealed cell )
                        , ( "is-touchable", Cell.isTouchable cell )
                        , ( "is-not-touchable", not <| Cell.isTouchable cell )
                        ]
                    , attribute "data-index" (String.fromInt index)
                    ]
                    [ contentToHtml content ]
            )


contentToHtml : Content.Content -> Html Msg
contentToHtml content =
    case content of
        Content.Power power ->
            if power <= 9 then
                img [ src (Assets.monsterSrc power), alt ("Lvl " ++ String.fromInt power) ] []

            else
                text <| String.fromInt power

        Content.SurroundingPower surroundingPower ->
            text <| String.fromInt surroundingPower

        Content.Bet bet ->
            text <| String.fromInt bet

        Content.Nothing ->
            text ""


viewGameDuration : Time.Posix -> Time.Posix -> Html Msg
viewGameDuration startedAt endedAt =
    let
        formatNumber =
            String.fromInt >> String.padLeft 2 '0'

        calculateDuration from to =
            let
                secondsSinceStart =
                    (Time.posixToMillis to - Time.posixToMillis from) // 1000
            in
            ( secondsSinceStart // 60, remainderBy 60 secondsSinceStart )

        ( minutes, seconds ) =
            calculateDuration startedAt endedAt
    in
    span []
        [ text <| formatNumber minutes
        , small [] [ text <| formatNumber seconds ]
        ]
