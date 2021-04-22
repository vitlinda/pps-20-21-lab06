package u06lab.code

/** Consider the Parser example shown in previous lesson.
  * Analogously to NonEmpty, create a mixin NotTwoConsecutive,
  * which adds the idea that one cannot parse two consecutive
  * elements which are equal.
  * Use it (as a mixin) to build class NotTwoConsecutiveParser,
  * used in the testing code at the end.
  * Note we also test that the two mixins can work together!!
  */

abstract class Parser[T] {
  def parse(t: T): Boolean  // is the token accepted?
  def end(): Boolean        // is it ok to end here
  def parseAll(seq: Seq[T]): Boolean = (seq forall {parse(_)}) & end()
}

class BasicParser(chars: Set[Char]) extends Parser[Char] {
  override def parse(t: Char): Boolean = chars.contains(t)
  override def end(): Boolean = true
}

trait NonEmpty[T] extends Parser[T] {
  private[this] var empty = true
  abstract override def parse(t: T) = {empty = false; super.parse(t)}
  abstract override def end() = !empty && {empty = true; super.end()}
}

class NonEmptyParser(chars: Set[Char]) extends BasicParser(chars) with NonEmpty[Char]

trait NotTwoConsecutive[T] extends Parser[T] {
  private var elem: Option[T] = Option.empty
  private def consecutiveEquals(e: T): Boolean = elem match {
    case None => elem = Some(e); false
    case Some(t) if(t == e) => true
    case Some(_) => elem = Some(e); false
  }

  abstract override def parse(t: T): Boolean = {!consecutiveEquals(t) && super.parse(t)}
  abstract override def end(): Boolean = super.end()
}

class NotTwoConsecutiveParser(chars: Set[Char]) extends BasicParser(chars) with NotTwoConsecutive[Char]


object ImplicitConversions{
  implicit class CharParser(base: String) {
    def charParser(): Parser[Char] = new BasicParser(base.toSet)
  }
}


