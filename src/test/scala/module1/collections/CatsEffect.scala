package module1.collections

import module1.homework.Show._
import module1.homework.Monad._
import org.scalatest.flatspec.AnyFlatSpec

class CatsEffect extends AnyFlatSpec {

  "check typeclass Show" should "ok" in {
    assert("str".show == "\"str\"")
    assert(42.show == "42")
    assert(false.show == "false")

    assert(List.empty[String].show == "List()")
    assert(List("23", "12").show == "List(\"23\", \"12\")")
    assert(List(0).show == "List(0)")

    assert(Set.empty[Boolean].show == "Set()")
    assert(Set(1, 2).show == "Set(1, 2)")
  }

  "check monad Option" should "ok" in {
    assert(Option(32).flatMap1(n => Option(n + 10)) == Option(42))
    assert(Option.empty[Int].flatMap1(n => Option(n + 10)).isEmpty)
    assert(Option(32).flatMap1(_ => Option.empty[String]).isEmpty)
    assert(Option(1).flatMap1(Option.empty[Int].pure) == Option(1))

    assert(Option(Option(100)).flatten1 == Option(100))
    assert(Option(Option.empty).flatten1.isEmpty)
  }

  "check monad List" should "ok" in {
    assert(List(1, 2).flatMap1(n => List(n + 1, n + 2)) == List(2, 3, 3, 4))
    assert(List.empty[Int].flatMap1(n => List(n + 1, n + 2)).isEmpty)
    assert(List(41, 42).flatMap1(_ => List.empty[String]).isEmpty)
    assert(List(42).flatMap1(List.empty[Int].pure) == List(42))

    assert(List(List(1, 2), List(3)).flatten1 == List(1, 2, 3))
  }
}
