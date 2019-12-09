import PotentialField.Estimation
import m._
import model.Item
import model.Tile.WALL
import sun.security.ec.point.ProjectivePoint.Mutable

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object PotentialField{
  def apply(unit: model.Unit, game: model.Game):PotentialField = {
    val pf = new PotentialField(IntV2(game.level.tiles.size, game.level.tiles(0).size ),game)
    game.lootBoxes.foreach { lb =>
      lb.item match {
        case Item.HealthPack(health) =>
          val hDiff = game.properties.unitMaxHealth  - unit.health
          pf.fillFrom(lb.position.tv.toIntV2, hDiff, 20)
        case Item.Weapon(weaponType) =>
          game.lootBoxes.foreach{ lb =>
            pf.fillFrom(lb.position.tv.toIntV2, 10d , 10)
          }
        case Item.Mine() =>
      }
    }
    pf
  }
  type Estimation = Double

}

class PotentialField(
                    val size:IntV2,
                    game:model.Game
                    ) {
  def fillFrom(from:IntV2, value:Estimation, steps:Int):PotentialField = {
    val filled:mutable.Set[IntV2] = mutable.Set()
    fillFromRec(from, value, steps, filled)
    this
  }

  def validIndex(i:IntV2):Boolean = i.x >= 0 && i.y >= 0 && i.x < size.x && i.y < size.y

  private def fillFromRec(from:IntV2, value:Estimation, steps:Int, filled:mutable.Set[IntV2]):Unit = {
    if(steps > 0 &&  validIndex(from) && !filled.contains(from) && game.level.tiles(from.x)(from.y) != WALL){
      filled.add(from)
      modifyXY(from, value)
      val neighEst = value * (steps - 1).toDouble / steps
      fillFromRec(from - IntV2(1, 0), neighEst, steps -1, filled)
      fillFromRec(from - IntV2(0, 1), neighEst, steps -1, filled)
      fillFromRec(from - IntV2(-1, 0), neighEst, steps -1, filled)
      fillFromRec(from - IntV2(0, -1), neighEst, steps -1, filled)
    }
  }



  private val field:Array[Estimation] = new Array(size.x * size.y)

  private def toBuffferCords(xy:IntV2):Int = xy.y * size.x + xy.x
  private def fromBuffferCords(xy:Int):IntV2 = IntV2(xy % size.x, xy / size.x)

  def modifyXY(xy:IntV2, delta:Estimation):PotentialField = {
    val bCords = toBuffferCords(xy)
    if(bCords >= field.length || bCords < 0 ){
      println(s"wrong cords form $xy bCords $bCords total size ${field.length}")
    } else {
      field(toBuffferCords(xy)) += delta
    }
    this
  }

  def apply(xy:IntV2):Estimation = field(toBuffferCords(xy))

  def indices:Seq[IntV2] = for(
    i <- 0 until size.x;
    j <- 0 until size.y
  ) yield IntV2(i, j)

}
