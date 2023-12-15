package pt.jmpfe

import cats.implicits.*
import cats.effect.IOApp
import cats.effect.IO
import cats.effect.Resource

import scala.io.*
import scala.util.Try

object Day1Part1 extends IOApp.Simple {

  def resource(name: String): Resource[IO, BufferedSource] = 
    Resource.make(IO(Source.fromResource(name)))(file => IO(file.close()))

  def readLines(resource: Resource[IO, Source]): IO[Seq[String]] =
    (
      for {
        input <- resource
      } yield input.getLines().toSeq
    ).use(IO.pure)

  def parseLine(line: String): Option[Int] = {
    val first: Option[Int] = line.find(_.isDigit).map(_.asDigit)
    val last: Option[Int] = line.findLast(_.isDigit).map(_.asDigit)
    (first, last).mapN { case (first, last) => first * 10 + last }
  }

  def solve(lines: Seq[String]): Int =
    lines.flatMap(parseLine).fold(0)(_ + _)

  def run: IO[Unit] = 
    for {
      lines <- readLines(resource("1-1-input.txt"))
      coordinate = solve(lines)
      _ <- IO.println(s"Coordinate is: $coordinate")
    } yield ()
}
