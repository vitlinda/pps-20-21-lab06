package u06lab.code

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import u06lab.code.TicTacToe.{Mark, O, X, computeAnyGame, find, placeAnyMark, someoneWon, printBoards}

class TicTacToeTest {

  @Test
  def testFind = {
    assertEquals(Some(X), find(List(Mark(0,0,X)), 0, 0))
    assertEquals(Some(O), find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)), 0, 1))
    assertEquals(None, find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)), 1, 1))

    printBoards(Seq(List()))
  }

  @Test
  def testPlaceAnyMark = {
    // Exercise 2: implement placeAnyMark such that..
    printBoards(placeAnyMark(List(),X)) //all X's possible moves (9)
    //... ... ..X ... ... X.. ... ... X..
    //... ..X ... ... .X. ... ... X.. ...
    //..X ... ... .X. ... ... X.. ... ...
    println()
    printBoards(placeAnyMark(List(Mark(0,0,O)),X)) //all X's possible moves (8) knowing that O is in (0,0)
    //O.. O.. O.X O.. O.. OX. O.. O..
    //... ..X ... ... .X. ... ... X..
    //..X ... ... .X. ... ... X.. ...

  }

  @Test
  def testComputeAnyGames = {
//     computeAnyGame(O, 4) foreach {g => printBoards(g); println()} //assuming first player is always X
    //... X.. X.. X.. XO.
    //... ... O.. O.. O..
    //... ... ... X.. X..
    //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
    //
    //... ... .O. XO. XOO
    //... ... ... ... ...
    //... .X. .X. .X. .X.
    assertEquals(3024, computeAnyGame(O, 4).length)

    computeAnyGame(O, 6) foreach {g => printBoards(g); println()} //assuming first player is always X
  }

  @Test
  def testSomeoneWon = {
    val board1 = List(List(Mark(1, 0, O), Mark(2, 2, O), Mark(2, 1, O),Mark(0, 0, X), Mark(0, 1, X), Mark(0, 2, X)))
    val board3 = List(List(Mark(2, 0, X), Mark(2, 1, X), Mark(2, 2, X)))
    val board4 = List(List(Mark(2, 0, X), Mark(1, 1, X), Mark(0, 2, X)))
    val board5 = List(List(Mark(0, 1, X), Mark(1, 1, X), Mark(2, 1, X)))
    val board6 = List(List(Mark(0, 2, X), Mark(1, 2, X), Mark(2, 2, X)))
    val board7 = List(List(Mark(0, 2, X), Mark(1, 1, X), Mark(2, 0, X)))
    val board8 = List(List(Mark(0, 0, X), Mark(1, 1, X), Mark(2, 2, X)))
    val board9 = List(List(Mark(0, 2, X), Mark(1, 1, X), Mark(2, 0, X)))

    assertTrue(someoneWon(board1))
    assertTrue(someoneWon(board3))
    assertTrue(someoneWon(board4))
    assertTrue(someoneWon(board5))
    assertTrue(someoneWon(board6))
    assertTrue(someoneWon(board7))
    assertTrue(someoneWon(board8))
    assertTrue(someoneWon(board9))


    val board2 = List(List(Mark(1, 0, O), Mark(2, 2, O), Mark(2, 1, O)))
    assertFalse(someoneWon(board2))

  }

}
