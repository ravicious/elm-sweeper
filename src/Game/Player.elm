module Game.Player exposing (Player, init, touchCell, isDead)

import Tagged exposing (Tagged)
import Tagged.Extra
import Game.Cell as Cell
import Game.ExpProgression as ExpProgression


type HpTag
    = HpTag


type alias Hp =
    Tagged HpTag Int


type alias Player =
    { xp : ExpProgression.Xp
    , level : ExpProgression.Level
    , hp : Hp
    }


init : Player
init =
    { xp = Tagged.tag 0
    , level = Tagged.tag 1
    , hp = Tagged.tag 10
    }


isDead : Player -> Bool
isDead =
    .hp >> Tagged.Extra.is (\hp -> hp <= 0)


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
    if (Tagged.untag player.level) < (Tagged.untag <| Cell.getPower cell) then
        let
            currentHpMinusCellPower =
                ((-) (Tagged.untag player.hp) (Tagged.untag <| Cell.getPower cell))
        in
            { player | hp = Tagged.tag <| max currentHpMinusCellPower 0 }
    else
        player


addXp : Cell.Cell -> Player -> Player
addXp cell player =
    { player | xp = Tagged.map ((+) (cell |> Cell.getPower |> Tagged.untag)) player.xp }


increaseLvlIfEnoughXp : ExpProgression.ExpProgression -> Player -> Player
increaseLvlIfEnoughXp expProgression player =
    if ExpProgression.isEnoughXpForNextLevel expProgression player.level player.xp then
        { player | level = Tagged.map ((+) 1) player.level }
    else
        player
