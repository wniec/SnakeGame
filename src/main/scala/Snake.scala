import scalafx.scene.paint.Color

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Snake(val color: SnakeColor){
  var body = new mutable.HashSet[(Int,Int)]()
  var bodyArray = new ArrayBuffer[(Int,Int)]()
  var direction: Direction = Direction.Up
  var alive = true
  private def len: Int =bodyArray.length
  def head: (Int, Int) = bodyArray(len-1)
  def grow(position:(Int,Int)): Unit = {
    body.addOne(position)
    bodyArray.addOne(position)
  }
  def turn(side:String):Unit={
    if(alive){
      side match {
        case "right" => this.direction = direction.prev
        case "left" => this.direction = direction.next
      }
    }
  }

  def move(position: (Int, Int)): Unit = {
    body.remove(bodyArray(0))
    bodyArray.remove(0)
    body.addOne(position)
    bodyArray.addOne(position)
  }
}
object Snake{
  def apply(position:(Int,Int),direction: Direction,color:SnakeColor):Snake={
    val snake = new Snake(color)
    snake.body.addOne(position)
    snake.bodyArray.addOne(position)
    snake.direction = direction
    snake
  }
}