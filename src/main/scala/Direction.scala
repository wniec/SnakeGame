enum Direction:
  case Up, Down, Left, Right


    def to_vec: (Int, Int) = {
      this match {
        case Up => (-1, 0)
        case Down => (1, 0)
        case Left => (0, -1)
        case Right => (0, 1)
      }
    }
  def horizontal: Boolean = {
    this match {
      case Up | Down => false
      case Left | Right => true
    }
  }