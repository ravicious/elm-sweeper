module Game.Event exposing (Event(..), toString)


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
