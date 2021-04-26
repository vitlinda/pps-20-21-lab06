package u06lab.code

import scala.annotation.tailrec

object TicTacToe {
  sealed trait Player{
    def other: Player = this match {case X => O; case _ => X}
    override def toString: String = this match {case X => "X"; case _ => "O"}
  }
  case object X extends Player
  case object O extends Player

  case class Mark(x: Int, y: Int, player: Player)
  type Board = List[Mark]
  type Game = List[Board]

  @tailrec
  def find(board: Board, x: Int, y: Int): Option[Player] = board match {
    case Nil => None
    case h :: _ if(h.x == x && h.y == y) => Some(h.player)
    case _ :: t => find(t, x , y)
  }

//  placeAnyMark(List(Mark(0,0,O), Mark(0,1, O)),X)
  def placeAnyMark(board: Board, player: Player): Seq[Board] = board match {
    case Nil => for {
      x <- 0 to 2
      y <- 0 to 2
      if(find(board, x,y).isEmpty)
    } yield List(Mark(x, y, player))
    case h :: t => for {
      boards <- placeAnyMark(t, player)
    } yield Mark(h.x, h.y, h.player) :: boards
  }

  def computeAnyGame(player: Player, moves: Int): Stream[Game] = ???

  def printBoards(game: Seq[Board]): Unit =
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) { print(" "); if (board == game.head) println()}
    }

}
