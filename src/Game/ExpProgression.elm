module Game.ExpProgression
    exposing
        ( ExpProgression
        , Level
        , Xp
        , getXpNeededForNextLevel
        , init
        , isEnoughXpForNextLevel
        )

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


getXpNeededForNextLevel : Level -> Xp -> ExpProgression -> Maybe Xp
getXpNeededForNextLevel level currentXp expProgression =
    getThresholdForNextLevel level expProgression
        |> Maybe.map (\threshold -> Tagged.map2 (-) threshold currentXp)


getThresholdForNextLevel : Level -> ExpProgression -> Maybe Xp
getThresholdForNextLevel =
    TaggedDict.get


isEnoughXpForNextLevel : ExpProgression -> Level -> Xp -> Bool
isEnoughXpForNextLevel expProgression level currentXp =
    getThresholdForNextLevel level expProgression
        |> Maybe.map
            (\xpNeededForNextLevel ->
                Tagged.map2 (>=) currentXp xpNeededForNextLevel |> Tagged.untag
            )
        |> Maybe.withDefault False
