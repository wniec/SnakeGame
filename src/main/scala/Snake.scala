import scalafx.scene.paint.Color

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

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
  def apply(map_size:Int,color:SnakeColor):Snake={
    val rand = new Random()
    val snake = new Snake(color)
    val range = color match{
      case SnakeColor.Green => (map_size/2,0)
      case SnakeColor.Blue => (0,map_size/2)
      case SnakeColor.Purple => (map_size/2,map_size/2)
      case SnakeColor.Orange => (0,0)
    }
    val position = (rand.nextInt(map_size/2)+range._1,rand.nextInt(map_size/2)+range._2)
    snake.body.addOne(position)
    snake.bodyArray.addOne(position)
    snake.direction = Direction.fromOrdinal(rand.nextInt(4))
    snake
  }
}