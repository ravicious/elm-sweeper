module Game.Player exposing (Player, init, touchCell)

import Tagged exposing (Tagged)
import Game.Cell as Cell
import Game.ExpProgression as ExpProgression


type alias Player =
    { xp : ExpProgression.Xp
    , level : ExpProgression.Level
    }


init : Player
init =
    { xp = Tagged.tag 0
    , level = Tagged.tag 1
    }


touchCell : ExpProgression.ExpProgression -> Cell.Cell -> Player -> Player
touchCell expProgression cell player =
    player
        |> addXp cell
        |> increaseLvlIfEnoughXp expProgression


addXp : Cell.Cell -> Player -> Player
addXp cell player =
    { player | xp = Tagged.map ((+) (cell |> Cell.getPower |> Tagged.untag)) player.xp }


increaseLvlIfEnoughXp : ExpProgression.ExpProgression -> Player -> Player
increaseLvlIfEnoughXp expProgression player =
    if ExpProgression.isEnoughXpForNextLevel expProgression player.level player.xp then
        { player | level = Tagged.map ((+) 1) player.level }
    else
        player
