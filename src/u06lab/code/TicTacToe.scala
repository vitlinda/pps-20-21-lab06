package u06lab.code

import scala.annotation.tailrec

object TicTacToe {

  sealed trait Player {
    def other: Player = this match {
      case X => O;
      case _ => X
    }

    override def toString: String = this match {
      case X => "X";
      case _ => "O"
    }
  }

  case object X extends Player

  case object O extends Player

  case class Mark(x: Int, y: Int, player: Player)

  type Board = List[Mark]
  type Game = List[Board]

  @tailrec
  def find(board: Board, x: Int, y: Int): Option[Player] = board match {
    case Nil => None
    case h :: _ if (h.x == x && h.y == y) => Some(h.player)
    case _ :: t => find(t, x, y)
  }

  def placeAnyMark(board: Board, player: Player): Seq[Board] = for {
    x <- 0 to 2
    y <- 0 to 2
    if (find(board, x, y).isEmpty)
  } yield Mark(x, y, player) :: board

  def computeAnyGame(player: Player, moves: Int): Stream[Game] = moves match {
    case 0 => Stream(List(List()))
    case m => (for {
      game <- computeAnyGame(player.other, m - 1)
      board <- placeAnyMark(game.head, player)
    } yield board :: game) // takeWhile(!someoneWon(_))
  }

  def someoneWon(game: Game): Boolean = ???

  def printBoards(game: Seq[Board]): Unit =
    for (y <- 0 to 2;
         board <- game.reverse;
         x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) {
        print(" ");
        if (board == game.head) println()
      }
    }

}
