package u06lab.code

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import u06lab.code.TicTacToe.{Mark, O, X, computeAnyGame, find, placeAnyMark, printBoards}

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

    printBoards(placeAnyMark(List(),X)) //tutte possibili mosse di X
    //... ... ..X ... ... X.. ... ... X..
    //... ..X ... ... .X. ... ... X.. ...
    //..X ... ... .X. ... ... X.. ... ...
    printBoards(placeAnyMark(List(Mark(0,0,O)),X)) //tutte le possibili mosse di X in cui c'è anche 0 nella posizione 0,0
    //O.. O.. O.X O.. O.. OX. O.. O..
    //... ..X ... ... .X. ... ... X..
    //..X ... ... .X. ... ... X.. ...
  }

  @Test
  def testComputeAnyGames = {
    // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
    computeAnyGame(O, 4) foreach {g => printBoards(g); println()}
    //... X.. X.. X.. XO.
    //... ... O.. O.. O..
    //... ... ... X.. X..
    //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
    //
    //... ... .O. XO. XOO
    //... ... ... ... ...
    //... .X. .X. .X. .X.
  }

  @Test
  def testStopEachGames = {
    // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
  }



}
