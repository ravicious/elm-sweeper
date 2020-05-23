module Game.View exposing (view, viewGameDuration)

import Assets
import Dict
import Game
import Game.Board as Board
import Game.Cell as Cell
import Game.Cell.Content as Content
import Game.Variant as Variant
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time


type alias Options msg a =
    { variant : Variant.Variant
    , viewOverlay : Maybe ( String, List (Html msg) )
    , onCellClickMsg : Maybe (Int -> msg)
    , intSeed : Int
    , duration :
        { a
            | startedAt : Time.Posix
            , endedAt : Time.Posix
        }
    }


view : Options msg a -> Game.State -> Html msg
view options game =
    div []
        [ viewGridStyle options.variant
        , div [ class "stack" ]
            [ div [ class "grid-container" ]
                [ case options.viewOverlay of
                    Just ( className, viewOverlay ) ->
                        div [ class <| "grid-overlay " ++ className ] viewOverlay

                    Nothing ->
                        text ""
                , div [ id "grid", class "grid" ] <|
                    viewCells { onClickMsg = options.onCellClickMsg } game
                ]
            , div [ class "cluster bar" ]
                [ div [ style "align-items" "flex-start", style "justify-content" "space-evenly" ]
                    [ viewStatus options.duration game
                    , viewMonsterSummary (Game.toMonsterSummary game)
                    ]
                ]
            , span [ class "seed" ] [ text <| "Seed: " ++ String.fromInt options.intSeed ]
            ]
        ]


viewGameDuration : Time.Posix -> Time.Posix -> Html msg
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


viewCells : { onClickMsg : Maybe (Int -> msg) } -> Game.State -> List (Html.Html msg)
viewCells options game =
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
                    , case options.onClickMsg of
                        Nothing ->
                            classList []

                        Just onClickMsg ->
                            onClick (onClickMsg index)
                    ]
                    [ viewContent content ]
            )


viewContent : Content.Content -> Html msg
viewContent content =
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


viewStatus : { a | startedAt : Time.Posix, endedAt : Time.Posix } -> Game.State -> Html msg
viewStatus duration game =
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
                , viewGameDuration duration.startedAt duration.endedAt
                ]
            ]
        ]


minSpanWidth : String -> List (Html.Attribute msg)
minSpanWidth s =
    [ style "display" "inline-block", style "min-width" s ]


viewMonsterSummary : Board.MonsterSummary -> Html msg
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


viewGridStyle : Variant.Variant -> Html msg
viewGridStyle variant =
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