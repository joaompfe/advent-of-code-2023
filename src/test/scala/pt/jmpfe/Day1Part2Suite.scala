package pt.jmpfe

import pt.jmpfe.Day1Part2.{parseLine, solve}

import cats.effect.{IO, SyncIO}
import munit.CatsEffectSuite

class Day1Part2Suite extends CatsEffectSuite {

  test("'two1nine' is 29") {
    assertEquals(parseLine("two1nine"), Some(29))
  }

  test("'eightwothree' is 83") {
    assertEquals(parseLine("eightwothree"), Some(83))
  }

  test("'abcone2threexyz' is 13") {
    assertEquals(parseLine("abcone2threexyz"), Some(13))
  }

  test("'xtwone3four' is 24") {
    assertEquals(parseLine("xtwone3four"), Some(24))
  }

  test("'4nineeightseven2' is 42") {
    assertEquals(parseLine("4nineeightseven2"), Some(42))
  }

  test("'zoneight234' is 14") {
    assertEquals(parseLine("zoneight234"), Some(14))
  }

  test("'7pqrstsixteen' is 76") {
    assertEquals(parseLine("7pqrstsixteen"), Some(76))
  }
}
