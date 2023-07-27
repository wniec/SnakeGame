import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Board(size:Int) {
  private val whole_size = size*2-1
  private val black: Player = new Player(0, whole_size / 2, Color.Black)
  private val white: Player = new Player(whole_size - 1, whole_size / 2, Color.White)
  private val blue: Player = new Player(whole_size / 2, 0, Color.Blue)
  private val red: Player = new Player(whole_size / 2, whole_size - 1, Color.Red)
  var round_color: Color = Color.Black
  val players: Seq[Player] =Seq(black,blue,white,red)
  var walls: mutable.HashSet[(Int,Int)] = new mutable.HashSet[(Int, Int)]()

  def change_round(): Unit = {
    round_color = Color.fromOrdinal((round_color.ordinal + 1) % 4)
  }

  def put_wall(pos: (Int, Int), direction: Direction): Unit = walls.addAll(wall_path(pos, direction))
  def coords_to_dir(pos1: (Int, Int), pos2: (Int, Int)):Direction={
    val dx = pos2._1 - pos1._1
    val dy = pos2._2 - pos1._2
    if (dx == 0) {
      if (dy < 0)
        Direction.Left
      else
        Direction.Right
    }
    else {
      if (dx < 0)
        Direction.Up
      else
        Direction.Down
    }
  }
  private def wall_path(pos:(Int,Int), direction: Direction):ArrayBuffer[(Int,Int)]={
    val vec = direction.to_vec
    val vx = vec._1
    val vy = vec._2
    val x = pos._1
    val y = pos._2
    ArrayBuffer((x,y),(x+vx,y+vy),(x+2*vx,y+2*vy))
  }

  def can_put_wall(pos1: (Int, Int), pos2: (Int, Int)): Boolean = {
    val dx = pos2._1 - pos1._1
    val dy = pos2._2 - pos1._2
    if((dx==0)==(dy==0)||(Math.abs(dx)!=2 && Math.abs(dy)!=2)) {
      return false
    }
    if ((pos1._1 % 2 == 0) == (pos1._2 % 2 == 0))
      return false
    val direction = coords_to_dir(pos1,pos2)
    if ((pos1._1 % 2 == 0 && direction.horizontal) || (pos1._2 % 2 == 0 && !direction.horizontal))
      return false
    if (can_go_to_end_if_added(pos1, direction))
      return wall_path(pos1, direction).map((x: (Int, Int)) => !walls.contains(x)).reduceLeft((x: Boolean, y: Boolean) => x && y)
    false
  }
  private def in_boundaries(position:(Int,Int)) ={position._1 > -1 && position._1 < whole_size && position._2 > -1 && position._2 < whole_size}
  private def can_go_to(current:(Int,Int),next:(Int,Int)):Boolean={
    if(in_boundaries(next)){
      val mid_row = (current._1 + next._1)/2
      val mid_col = (current._2 + next._2)/2
      if(walls.contains((mid_row,mid_col)))
        return false
      return true
    }
    false
  }

  def available(position: (Int,Int)): ArrayBuffer[(Int, Int)] = {
    val result = new ArrayBuffer[(Int, Int)]()
    if(position!=players(round_color.ordinal).position)
      return result
    for (move <- Direction.values) {
      val vector: (Int, Int) = move.to_vec
      val new_row:Int = position._1 + vector._1 * 2
      val new_col:Int = position._2 + vector._2 * 2
      if(can_go_to(position,(new_row,new_col))){
        val is_not_other_pawn = Color.values.map((x:Color) => players(x.ordinal).position != (new_row,new_col)).reduceLeft((x:Boolean,y: Boolean) => x&&y)
        if(is_not_other_pawn){
          result.addOne((new_row, new_col))
        }
        else{
          val row_after = position._1 + vector._1 * 4
          val col_after = position._2 + vector._2 * 4
          if(can_go_to((new_row,new_col),(row_after,col_after))){
            val is_not_other_pawn2 = Color.values.map((x:Color) => players(x.ordinal).position != (row_after,col_after)).reduceLeft((x:Boolean,y: Boolean) => x&&y)
            if (is_not_other_pawn2) {
              result.addOne((row_after, col_after))
            }
          }
        }
      }
    }
    result
  }

  def available(position: (Int, Int), added: mutable.ArrayBuffer[(Int,Int)]): ArrayBuffer[(Int, Int)] = {
    val result = new ArrayBuffer[(Int, Int)]()
    for (move <- Direction.values) {
      val vector: (Int, Int) = move.to_vec
      val new_row = position._1 + vector._1 * 2
      val new_col = position._2 + vector._2 * 2
      if (new_row > -1 && new_row < whole_size && new_col > -1 && new_col < whole_size) {
        val mid_row = position._1 + vector._1
        val mid_col = position._2 + vector._2
        if (!walls.contains((mid_row, mid_col)) && !added.contains((mid_row, mid_col)))
          result.addOne((new_row, new_col))
      }
    }
    result
  }
  private def has_won(position: (Int, Int), color: Color): Boolean = {
    color match{
      case Color.Black => position._1 == whole_size - 1
      case Color.White => position._1 == 0
      case Color.Blue => position._2 == whole_size - 1
      case Color.Red => position._2 == 0
    }
  }

  def who_won: Option[Color] = {
    var winner: Option[Color] = None
    players.foreach((p: Player) => {
      if(has_won(p.position,p.color)){
        winner = Some(p.color)
      }
    })
    winner
  }

  private def can_go_to_end_if_added(position: (Int, Int), color: Color, visited: mutable.HashSet[(Int, Int)], added: mutable.ArrayBuffer[(Int, Int)]): Boolean = {
    if (has_won(position,color)) {
      return true
    }
    visited.addOne(position)
    val moves = available(position, added).filter((x: (Int, Int)) => !visited.contains(x))
    if (moves.isEmpty)
      return false
    moves.map((move: (Int, Int)) => can_go_to_end_if_added(move, color, visited, added)).reduceLeft((x: Boolean, y: Boolean) => x || y)
  }

  private def can_go_to_end_if_added(pos:(Int,Int), direction: Direction): Boolean = {
    val w_path = wall_path(pos,direction)
    players.map((x:Player) => can_go_to_end_if_added(x.position,x.color,new mutable.HashSet[(Int, Int)](),w_path)).reduceLeft((y: Boolean, z: Boolean) => y&&z)
  }
}
