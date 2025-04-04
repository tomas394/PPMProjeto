import scala.util.Random

object AtariGoPPM {
  object Stone extends Enumeration {
    type Stone = Value
    val Black, White, Empty = Value
  }

  case class MyRandom(seed: Long) {
    def nextInt(n: Int): (Int, MyRandom) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRand = MyRandom(newSeed)
      val number = (newSeed >>> 16).toInt % n
      (number, nextRand)
    }
  }
  type Coord2D = (Int, Int) 
  type Board = List[List[Stone.Value]]

  def randomMove(lstOpenCoords: List[Coord2D], rand: Random): (Coord2D, Random) = {   //T1
    val index = rand.nextInt(lstOpenCoords.length) 
    (lstOpenCoords(index), new Random(rand.nextLong())) 
  }


  def play(board: Board, player: Stone.Value, coord: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
    if (!lstOpenCoords.contains(coord)) (None, lstOpenCoords) 

    else {
      val (row, col) = coord
      val newBoard = board.updated(row, board(row).updated(col, player))   //T2
      val newOpenCoords = lstOpenCoords.filter(_ != coord) 
      (Some(newBoard), newOpenCoords)
    }
  }


}