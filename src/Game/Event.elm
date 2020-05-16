module Game.Event exposing (Event(..), encode, toString)

import Game.GameResult
import Json.Encode as Encode
import Json.Encode.Extra as EncodeExtra


type Event
    = MonsterKilled
    | HitByMonster
    | LevelUp
    | GameOver
    | GameWon


toString : Event -> String
toString event =
    case event of
        MonsterKilled ->
            "MonsterKilled"

        HitByMonster ->
            "HitByMonster"

        LevelUp ->
            "LevelUp"

        GameOver ->
            "GameOver"

        GameWon ->
            "GameWon"


encode : { calculatePlacing : () -> Maybe Game.GameResult.Placing } -> Event -> Encode.Value
encode context event =
    case event of
        GameWon ->
            Encode.object
                [ ( "type", Encode.string (toString event) )
                , ( "place", EncodeExtra.maybe Game.GameResult.encodePlacing (context.calculatePlacing ()) )
                ]

        _ ->
            Encode.object [ ( "type", Encode.string (toString event) ) ]
