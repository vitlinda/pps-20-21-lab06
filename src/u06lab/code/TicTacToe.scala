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

  val winCases = List(
    List((0, 0), (0, 1), (0, 2)),
    List((1, 0), (1, 1), (1, 2)),
    List((2, 0), (2, 1), (2, 2)),
    List((0, 0), (1, 0), (2, 0)),
    List((0, 1), (1, 1), (2, 1)),
    List((0, 2), (1, 2), (2, 2)),
    List((0, 0), (1, 1), (2, 2)),
    List((0, 2), (1, 1), (2, 0)),
  )

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

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match {
    case 0 => LazyList(List(List()))
    case m => for {
      game <- computeAnyGame(player.other, m - 1)
      board <- placeAnyMark(game.head, player)
    } yield if (someoneWon(game)) game else board :: game
  }

  def someoneWon(game: Game): Boolean = {
    winCases.exists(w => {
      val players = w.map(s => find(game.head, s._1, s._2))
      if (!players.contains(None) && players.distinct.length <= 1) true else false
    })
  }

  def printBoards(game: Seq[Board]): Unit = {
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

}
