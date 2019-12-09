import m._
import model.CustomData.{Log, PlacedText}
import model.{ColorFloat, TextAlignment, Tile, UnitAction, Vec2Double}

import scala.language.implicitConversions

class MyStrategy {
  def getAction(unit: model.Unit, game: model.Game, debug: Debug): model.UnitAction = {
    val aim = game.units.filter(_.playerId != unit.playerId).map(_.position - unit.position).minBy(_.lengthSquared)

    val pf = PotentialField(unit, game)
    pf.indices.foreach { xy =>
      if (pf(xy) !~= 0f) {
        debug.draw(PlacedText(s"${pf(xy).toInt}", xy.toV2, TextAlignment.LEFT, 10, ColorFloat(1f, 1f, 1f, 1f)))
      }
    }
    val (current, left, right, bot, top) = (
      pf(unit.position.toIntV2),
      pf(unit.position.toIntV2 + dleft),
      pf(unit.position.toIntV2 + dright),
      pf(unit.position.toIntV2 + dbottom),
      pf(unit.position.toIntV2 + dtop)
    )
    var dir = 0
    var jumpBot = false
    var jumpTop = false
    max(current, left, right, bot, top) match {
      case `left` => dir = -1
      case `right` => dir = 1
      case `bot` => jumpBot = false
      case `top` => jumpTop = false
      case `current` =>
    }

    UnitAction(dir,
      jump = jumpTop,
      jumpDown = jumpBot,
      aim = aim,
      shoot = true,
      reload = false,
      swapWeapon = false,
      plantMine = false)
  }

}