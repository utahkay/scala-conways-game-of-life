/**
 * Created by kjohansen on 12/31/2014.
 */

object LifeApp extends App {
  val spinner = Set(Cell(2,1), Cell(2,2), Cell(2,3))
  var life = Life(spinner)
  while (true) {
    println(new StringRepresentation(life))
    life = life.next
    readLine("Hit Enter to continue")
  }
}
