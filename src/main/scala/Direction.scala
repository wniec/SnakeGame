enum Direction:
  case Up, Right, Down, Left


    def to_vec: (Int, Int) = {
      this match {
        case Up => (-1, 0)
        case Down => (1, 0)
        case Left => (0, -1)
        case Right => (0, 1)
      }
    }

  def next: Direction = Direction.fromOrdinal((this.ordinal+1)%4)
  def prev: Direction = Direction.fromOrdinal((this.ordinal+3)%4)