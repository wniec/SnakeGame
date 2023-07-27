import scala.collection.mutable.ArrayBuffer

class Snake{
  val body = new ArrayBuffer[(Int,Int)]()
  var direction: Direction = Direction.Up
  def len: Int =body.length
}
object Snake{
  def apply(position:(Int,Int),direction: Direction):Snake={
    val snake = new Snake()
    snake.body.addOne(position)
    snake.direction = direction
    snake
  }
}