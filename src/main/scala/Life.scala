/**
 * Created by kjohansen on 12/31/2014.
 */

case class Life(liveCells: Set[Cell]) {
  def next: Life =
    Life(AllCellsAndNeighbors(liveCells).filter(IsAliveInNextGeneration))

  private def IsAliveInNextGeneration(c: Cell): Boolean =
    NextGeneration(isAlive(c), CountLiveNeighbors(liveCells, c))

  private def isAlive(c: Cell): Boolean = liveCells contains c
}

object AllCellsAndNeighbors {
  def apply(cells: Set[Cell]): Set[Cell] =
    cells.foldLeft(Set[Cell]()) {(s, c) => s ++ c.cellAndNeighbors }
}

object NextGeneration {
  def apply(isAlive: Boolean, numLiveNeighbors: Int): Boolean = (isAlive, numLiveNeighbors) match {
    case (true, 2) => true
    case (_, 3) => true
    case (_, _) => false
  }
}

object CountLiveNeighbors {
  def apply(cells: Set[Cell], origin: Cell): Int =
    origin.cellAndNeighbors.count(c => c != origin && cells.contains(c))
}

case class Cell(x: Int, y: Int) {
  def cellAndNeighbors: Seq[Cell] =
    for { xx <- x-1 to x+1; yy <- y-1 to y+1 } yield new Cell(xx, yy)
}

class StringRepresentation(val life: Life) {
  val liveCells = life.liveCells

  override def toString: String = {
    def minY: Int = liveCells.minBy(_.y).y
    def maxY: Int = liveCells.maxBy(_.y).y
    (minY to maxY).map(rowStr).mkString("\n")
  }

  private def rowStr(y: Int): String = {
    def minX: Int = liveCells.minBy(_.x).x
    def maxX: Int = liveCells.maxBy(_.x).x
    def isAlive(c: Cell): Boolean = liveCells contains c
    def cellStr(x: Int): Char = Cell(x,y) match {
      case c if isAlive(c) => '*'
      case _ => '.'
    }
    (minX to maxX).map(cellStr).mkString
  }
}
