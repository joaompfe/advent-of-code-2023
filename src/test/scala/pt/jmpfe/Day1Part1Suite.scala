package pt.jmpfe

import pt.jmpfe.Day1Part1.{parseLine, solve}

import cats.effect.{IO, SyncIO}
import munit.CatsEffectSuite

class Day1Part1Suite extends CatsEffectSuite {

  test("calibration value in '1abc2' is 12") {
    assertEquals(parseLine("1abc2"), Some(12))
  }

  test("calibration value in 'pqr3stu8vwx' is 38") {
    assertEquals(parseLine("pqr3stu8vwx"), Some(38))
  }

  test("calibration value in 'a1b2c3d4e5f' is 15") {
    assertEquals(parseLine("a1b2c3d4e5f"), Some(15))
  }

  test("calibration value in 'treb7uchet' is 77") {
    assertEquals(parseLine("treb7uchet"), Some(77))
  }

  test("'1abc2' 'pqr3stu8vwx' 'a1b2c3d4e5f' 'treb7uchet' is 142") {
    val puzzle = Seq("1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet")
    assertEquals(solve(puzzle), 142)
  }
}
