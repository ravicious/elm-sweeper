module Game.ExpProgression exposing (ExpProgression, Level, Xp, init, isEnoughXpForNextLevel)

import Tagged exposing (Tagged)
import Tagged.Dict as TaggedDict exposing (TaggedDict)


type XpTag
    = XpTag


type alias Xp =
    Tagged XpTag Int


type LevelTag
    = LevelTag


type alias Level =
    Tagged LevelTag Int


type alias ExpProgression =
    TaggedDict LevelTag Int Xp


init : List ( Int, Int ) -> ExpProgression
init levelAndXpNeededForNextLevel =
    TaggedDict.fromUntaggedList levelAndXpNeededForNextLevel
        |> TaggedDict.map (\_ xpNeededForNextLevel -> Tagged.tag xpNeededForNextLevel)


getXpNeededForNextLevel : Level -> ExpProgression -> Maybe Xp
getXpNeededForNextLevel =
    TaggedDict.get


isEnoughXpForNextLevel : ExpProgression -> Level -> Xp -> Bool
isEnoughXpForNextLevel expProgression level currentXp =
    getXpNeededForNextLevel level expProgression
        |> Maybe.map
            (\xpNeededForNextLevel ->
                Tagged.map2 ((>=)) currentXp xpNeededForNextLevel |> Tagged.untag
            )
        |> Maybe.withDefault False
