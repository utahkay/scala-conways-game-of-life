import org.scalatest.{Matchers, FlatSpec}

class LifeTest extends FlatSpec with Matchers {
  "empty board" should "transition to empty board" in {
    val life = Life(Set())
    life.next.liveCells.size should be (0)
  }

  "board with one cell" should "transition to empty board" in {
    val life = Life(Set(Cell(0,0)))
    life.next.liveCells.size should be (0)
  }

  "board with four cells block" should "transition to board with four cells" in {
    val life = Life(Set(Cell(1,1), Cell(2,1), Cell(1,2), Cell(2,2)))
    life.next.liveCells.size should be (4)
  }
}

class CellTest extends FlatSpec with Matchers {
  "cell 0,0 and all its neighbors" should "be the expected 9 cells" in {
    Cell(0,0).cellAndNeighbors should equal (Vector(Cell(-1,-1), Cell(-1,0), Cell(-1,1), Cell(0,-1), Cell(0,0), Cell(0,1), Cell(1,-1), Cell(1,0), Cell(1,1)))
  }
}

class CountLiveNeighborsTest extends FlatSpec with Matchers {
  "live cell with 2 live neighbors" should "have 2 neighbors" in {
    val cells = Set(Cell(0,0), Cell(1,0), Cell(0,1))
    CountLiveNeighbors(cells, Cell(0,1)) should equal (2)
  }
}

class AllCellsAndNeighborsTest extends FlatSpec with Matchers {
  "2 cells" should "produce set containing the 2 cells and all their neighbors" in {
    val cells = Set(Cell(0,0), Cell(1,0))
    AllCellsAndNeighbors(cells) should equal (Set(Cell(0,0), Cell(1,0),
      Cell(-1,-1), Cell(-1,0), Cell(-1,1), Cell(0,-1), Cell(0,1), Cell(1,-1), Cell(1,1),
      Cell(2,-1), Cell(2,0), Cell(2,1)))
  }
}

class NextGenerationTest extends FlatSpec with Matchers {
  "live cell with fewer than two live neighbors" should "die" in {
    NextGeneration(true, 0) should equal (false)
    NextGeneration(true, 1) should equal (false)
  }

  "live cell with two or three live neighbors" should "live" in {
    NextGeneration(true, 2) should equal (true)
    NextGeneration(true, 3) should equal (true)
  }

  "live cell with more than three live neighbors" should "die" in {
    NextGeneration(true, 4) should equal (false)
    NextGeneration(true, 5) should equal (false)
  }

  "dead cell with three live neighbors" should "live" in {
    NextGeneration(false, 3) should equal (true)
  }

  "dead cell with other than three live neighbors" should "stay dead" in {
    NextGeneration(false, 2) should equal (false)
    NextGeneration(false, 4) should equal (false)
  }
}