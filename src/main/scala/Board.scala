import scalafx.scene.paint.Color

import java.util.Random
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Board(size:Int) {
  val rand = new Random()
  val snakes: mutable.Buffer[Snake] = (0 to 3).map((i:Int) => Snake(size,SnakeColor.fromOrdinal(i))).toBuffer
  val apples: mutable.Buffer[Apple] = (0 to 8).map((_:Int) => Apple(size)).toBuffer
  private def occupied: Set[(Int, Int)] = snakes.filter((s: Snake) =>s.alive).map((s: Snake) => s.body.toSet).reduce((x:Set[(Int,Int)], y:Set[(Int,Int)]) => x.union(y))
  private def applesPositions: Set[(Int, Int)] = apples.map((a: Apple) => a.position).toSet
  private def canGoTo(position:(Int,Int)):Boolean={
    val isOccupied = snakes.exists((s: Snake) => s.alive&&s.body.contains(position))
      if(occupied.contains(position))
        return false
      true
  }
  private def crash(snake: Snake): Unit = {
    val idx =this.snakes.indexOf(snake)
    snake.alive = false
  }
  private def randomPos:(Int,Int)={
    val range = (0 until this.size).toList
    val cross_prod: Set[(Int,Int)] = range.map((x:Int) => (List(x),range)).map((x:List[Int],y:List[Int]) => x.zipAll(y,x.head,x.head).toSet).reduce((x:Set[(Int,Int)],y:Set[(Int,Int)]) => x.union(y))
    //cross_prod is a set of all coords
    val free = cross_prod.diff(occupied).diff(applesPositions).toVector
    free(rand.nextInt(free.size))
  }

  private def move(new_position: (Int, Int), snake: Snake): Unit = {
    if(!canGoTo(new_position)){
      crash(snake)
      val heads = snakes.map((s:Snake) => s.head)
      if(heads.contains(new_position)){
        val another = snakes(heads.indexOf(new_position))
        crash(another)
      }
      return
    }
    val apple_positions = apples.map((a:Apple) => a.position)
    if (apple_positions.contains(new_position) ){
      snake.grow(new_position)
      val idx = apple_positions.indexOf(new_position)
      apples.remove(idx)
      apples.append(new Apple(randomPos))
    }
    else
      snake.move(new_position)
  }

  def move(snake: Snake): Unit = {
    val move_vec = snake.direction.to_vec
    val position = snake.head
    val new_position = ((move_vec._1+position._1+size)%size,(move_vec._2+position._2+size)%size)
    move(new_position, snake)
  }
}
