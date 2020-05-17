module Game.Player exposing (Player, init, isDead, isMorePowerfulThanCell, touchCell)

import Game.Cell as Cell
import Game.ExpProgression as ExpProgression
import Tagged exposing (Tagged)
import Tagged.Extra


type HpTag
    = HpTag


type alias Hp =
    Tagged HpTag Int


type alias Player =
    { xp : ExpProgression.Xp
    , level : ExpProgression.Level
    , hp : Hp
    }


init : Int -> Player
init hp =
    { xp = Tagged.tag 0
    , level = Tagged.tag 1
    , hp = Tagged.tag hp
    }


isDead : Player -> Bool
isDead =
    .hp >> Tagged.Extra.is (\hp -> hp <= 0)


isMorePowerfulThanCell : Player -> Cell.Cell -> Bool
isMorePowerfulThanCell player cell =
    Tagged.untag player.level >= (Tagged.untag <| Cell.getPower cell)


touchCell : ExpProgression.ExpProgression -> Cell.Cell -> Player -> Player
touchCell expProgression cell player =
    player
        |> reduceHpIfCellIsMorePowerful cell
        |> (\playerAfterReducingHp ->
                if isDead playerAfterReducingHp then
                    playerAfterReducingHp

                else
                    playerAfterReducingHp
                        |> addXp cell
                        |> increaseLvlIfEnoughXp expProgression
           )


reduceHpIfCellIsMorePowerful : Cell.Cell -> Player -> Player
reduceHpIfCellIsMorePowerful cell player =
    if not <| isMorePowerfulThanCell player cell then
        let
            currentHpMinusCellHitPower =
                Tagged.untag player.hp - calculateCellHitPower player cell
        in
        { player | hp = Tagged.tag <| max currentHpMinusCellHitPower 0 }

    else
        player


calculateCellHitPower : Player -> Cell.Cell -> Int
calculateCellHitPower player cell =
    let
        playerLevel =
            Tagged.untag player.level

        cellPower =
            Tagged.untag <| Cell.getPower cell

        levelsDifferenceDividedByLevelRoundedUp =
            ceiling <| toFloat (cellPower - playerLevel) / toFloat playerLevel
    in
    cellPower * levelsDifferenceDividedByLevelRoundedUp


addXp : Cell.Cell -> Player -> Player
addXp cell player =
    { player | xp = Tagged.map ((+) (Tagged.untag <| Cell.getXpReward cell)) player.xp }


increaseLvlIfEnoughXp : ExpProgression.ExpProgression -> Player -> Player
increaseLvlIfEnoughXp expProgression player =
    if ExpProgression.isEnoughXpForNextLevel expProgression player.level player.xp then
        { player | level = Tagged.map ((+) 1) player.level }

    else
        player
