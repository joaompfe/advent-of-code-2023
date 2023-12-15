package pt.jmpfe

import cats.implicits.*
import cats.effect.*

import scala.io.*
import scala.util.Try

object Day1Part2 extends IOApp.Simple {

  def resource(name: String): Resource[IO, BufferedSource] = 
    Resource.make(IO(Source.fromResource(name)))(file => IO(file.close()))

  def readLines(resource: Resource[IO, Source]): IO[Seq[String]] =
    (
      for {
        input <- resource
      } yield input.getLines().toSeq
    ).use(IO.pure)

  sealed trait SpelledInt { def string: String; def value: Int }

  case object One   extends SpelledInt { def string = "one";   def value = 1 }
  case object Two   extends SpelledInt { def string = "two";   def value = 2 }
  case object Three extends SpelledInt { def string = "three"; def value = 3 }
  case object Four  extends SpelledInt { def string = "four";  def value = 4 }
  case object Five  extends SpelledInt { def string = "five";  def value = 5 }
  case object Six   extends SpelledInt { def string = "six";   def value = 6 }
  case object Seven extends SpelledInt { def string = "seven"; def value = 7 }
  case object Eight extends SpelledInt { def string = "eight"; def value = 8 }
  case object Nine  extends SpelledInt { def string = "nine";  def value = 9 }

  object SpelledInt {
    val All: Set[SpelledInt] = Set(One, Two, Three, Four, Five, Six, Seven, Eight, Nine)
  }

  def digitFromChar(c: Char): Option[Int] = Try(s"$c".toInt).toOption

  def findFirstDigit(line: String): Option[Int] =
    line.headOption.flatMap(head =>
      digitFromChar(head) orElse
      SpelledInt.All.find(d => line.startsWith(d.string)).map(_.value) orElse
      findFirstDigit(line.drop(1))
    )

  def findLastDigit(line: String): Option[Int] =
    line.lastOption.flatMap(last =>
      digitFromChar(last) orElse
      SpelledInt.All.find(d => line.endsWith(d.string)).map(_.value) orElse
      findLastDigit(line.dropRight(1))
    )

  def parseLine(line: String): Option[Int] = {
    val first: Option[Int] = findFirstDigit(line)
    val last: Option[Int] = findLastDigit(line)
    (first, last).mapN { case (first, last) => first * 10 + last }
  }

  def solve(lines: Seq[String]): Int =
    lines.flatMap(parseLine).fold(0)(_ + _)

  def run: IO[Unit] = 
    for {
      lines <- readLines(resource("1-2-input.txt"))
      coordinate = solve(lines)
      _ <- IO.println(s"Coordinate is: $coordinate")
    } yield ()
}
