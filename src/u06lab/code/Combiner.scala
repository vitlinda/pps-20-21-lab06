package u06lab.code

/**
 * 1) Implement trait Functions with an object FunctionsImpl such that the code
 * in TryFunctions works correctly.
 */

trait Functions {
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
}

object FunctionsImpl extends Functions {

  //  override def sum(a: List[Double]): Double = a.foldRight(0.0)(_+_)
  //  override def concat(a: Seq[String]): String = a.foldRight("")(_+_)
  //  override def max(a: List[Int]): Int = a.foldRight(Int.MinValue)((e1, e2) => if(e1 > e2) e1 else e2 )

  import ImplicitCombiner._

  override def sum(a: List[Double]): Double = combine(a) /* (sumCombine) */

  override def concat(a: Seq[String]): String = combine(a) /* (concatCombine) */

  override def max(a: List[Int]): Int = combine(a) /* (maxCombine) */

  private def combine[T: Combiner](a: Iterable[T]): T = {
    var elem = implicitly[Combiner[T]].unit
    a.foreach(e => elem = implicitly[Combiner[T]].combine(elem, e))
    elem
  }

}

/*
  * 2) To apply DRY principle at the best,
  * note the three methods in Functions do something similar.
  * Use the following approach:
  * - find three implementations of Combiner that tell (for sum,concat and max) how
  *   to combine two elements, and what to return when the input list is empty
  * - implement in FunctionsImpl a single method combiner that, other than
  *   the collection of A, takes a Combiner as input
  * - implement the three methods by simply calling combiner
  *
  * When all works, note we completely avoided duplications..
 */

trait Combiner[A] {
  def unit: A
  def combine(a: A, b: A): A
}

object ImplicitCombiner {

  implicit object sumCombine extends Combiner[Double] {
    override def unit: Double = 0.0
    override def combine(a: Double, b: Double): Double = a + b
  }

  implicit object concatCombine extends Combiner[String] {
    override def unit: String = ""
    override def combine(a: String, b: String): String = a + b
  }

  implicit object maxCombine extends Combiner[Int] {
    override def unit: Int = Int.MinValue
    override def combine(a: Int, b: Int): Int = if (a > b) a else b
  }

}