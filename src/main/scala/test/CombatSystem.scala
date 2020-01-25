package test

trait TroopTypeInfo {
  def getName: String
  def getAttackRating: Int
  def getDefenseRating: Int
}

object Infantry extends TroopTypeInfo {
  override def getName: String = "Infantry"

  override def getAttackRating: Int = 1

  override def getDefenseRating: Int = 2
}

trait RNG {
  def nextBoundedInt(min: Int, max: Int) : (Int, RNG)
}

trait HitGenerator {
  def hit(rating: Int) : (Boolean, HitGenerator)
}

case class SixSidedHitGenerator(seed: Long) extends HitGenerator {

  override def hit(rating: Int): (Boolean, HitGenerator) = {
    val newSeed = (seed * 0x5DEECE660L + 0xBL) & 0xFFFFFFFFFFFFL
    val nextHitGenerator = SixSidedHitGenerator(newSeed)
    val roll = (newSeed >>> 16).toInt % 6 + 1
    println(roll)
    (roll <= rating, nextHitGenerator)
  }
}


object CombatSystem {

  def calculateHits(troop: TroopTypeInfo, count: Int, hitGenerator: HitGenerator ) : Int = {

    val attackRating = troop.getAttackRating
    var hg : HitGenerator = SixSidedHitGenerator(77)

    val hits = Range(1, count).map(index => {
      val hitInfo = hg.hit(attackRating)
      hg = hitInfo._2
      if(hitInfo._1) 1 else 0
    }).sum

    hits
  }


  def main(args: Array[String]): Unit = {
    val call = SixSidedHitGenerator(47).hit(2)
    println(s"Was hit: $call")

    println(calculateHits(Infantry, 101, SixSidedHitGenerator(77)))
  }




}



