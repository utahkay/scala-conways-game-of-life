/**
 * Created by kjohansen on 12/31/2014.
 */
case class Life(liveCells: Set[Cell]) {
  def next(): Life =
    Life(for {
        c <- AllCellsAndNeighbors(liveCells)
        if (NextGeneration(isLive(c), CountLiveNeighbors(liveCells, c)))
      } yield c)

  def isLive(c: Cell): Boolean = liveCells contains c
}

case class Cell(x: Int, y: Int) {
  def cellAndNeighbors(): Seq[Cell] =
    for { xx <- x-1 to x+1; yy <- y-1 to y+1 } yield new Cell(xx, yy)
}

object CountLiveNeighbors {
  def apply(cells: Set[Cell], origin: Cell): Int =
    (for { c <- origin.cellAndNeighbors if (c != origin && cells.contains(c)) } yield 1).size
}

object AllCellsAndNeighbors {
  def apply(cells: Set[Cell]): Set[Cell] =
    cells.foldLeft(Set[Cell]()) {(s, c) => s ++ c.cellAndNeighbors() }
}

object NextGeneration {
  def apply(isLive: Boolean, numLiveNeighbors: Int): Boolean = (isLive, numLiveNeighbors) match {
    case (true, 2) => true
    case (_, 3) => true
    case (_, _) => false
  }
}

class StringRepresentation(val life: Life) {
  val liveCells = life.liveCells

  override def toString(): String = {
    def minY: Int = liveCells.minBy(_.y).y
    def maxY: Int = liveCells.maxBy(_.y).y
    (for { y <- minY to maxY } yield rowStr(y)).mkString("\n")
  }

  private def rowStr(y: Int): String = {
    def minX: Int = liveCells.minBy(_.x).x
    def maxX: Int = liveCells.maxBy(_.x).x
    def isLive(c: Cell): Boolean = liveCells contains c
    (for { x <- minX to maxX } yield Cell(x,y) match {
      case c if isLive(c) => '*'
      case _ => '.'
    }).mkString
  }
}
