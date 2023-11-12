package module1.homework

import scala.language.{higherKinds, implicitConversions}


object Show {

  trait Show[T] {
    def show(t: T): String
  }

  object Show {
    def apply[T](implicit ev: Show[T]): Show[T] = ev

    def fromFunction[A](f: A => String): Show[A] = new Show[A] {
      override def show(t: A): String = f(t)
    }
    def fromJvm[A]: Show[A] = fromFunction(_.toString)

    implicit val strToShow: Show[String] = fromFunction("\"" + _ + "\"")
    implicit val boolToShow: Show[Boolean] = fromJvm
    implicit val intToShow: Show[Int] = fromJvm
    implicit def listToShow[A:Show]: Show[List[A]] = new Show[List[A]] {
      override def show(t: List[A]): String = t.map(_.show).mkString("List(", ", ", ")")
    }

    implicit def setToShow[A: Show]: Show[Set[A]] = new Show[Set[A]] {
      override def show(t: Set[A]): String = t.map(_.show).mkString("Set(", ", ", ")")
    }
  }

  implicit class ShowSyntax[T: Show](a: T) {
    def show: String = Show[T].show(a)
  }
}

object Monad {

  trait Monad[T[_]] {
    def pure[A](a: A): T[A]

    def flatten[A](fa: T[T[A]]): T[A] = flatMap(fa)(v => v)

    def flatMap[A, B](fa: T[A])(f: A => T[B]): T[B]

    def map[A, B](fa: T[A])(f: A => B): T[B] = flatMap(fa)(f andThen pure)
  }


  object Monad {
    def apply[F[_]](implicit ev: Monad[F]): Monad[F] = ???

    implicit val optionMonad: Monad[Option] = new Monad[Option] {
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
        case None => None
        case Some(x) => f(x)
      }
      override def pure[A](a: A): Option[A] = Some(a)
    }

    implicit val listMonad: Monad[List] = new Monad[List] {
      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
      override def pure[A](a: A): List[A] = List(a)
    }

    type EitherM[T] = Either[_, T]
    implicit val eitherMonad: Monad[EitherM] = new Monad[EitherM] {
      override def flatMap[A, B](fa: EitherM[A])(f: A => EitherM[B]): EitherM[B] = fa match {
        case Left(a) => Left(a)
        case Right(x) => f(x)
      }
      override def pure[A](a: A): EitherM[A] = Right(a)
    }
  }

  implicit class MonadSyntax[F[_], A](private val fa: F[A]) {
    def flatMap1[B](f: A => F[B])(implicit M: Monad[F]): F[B] = M.flatMap(fa)(f)
    def pure(a: A)(implicit M: Monad[F]): F[A] = M.pure(a)
  }

  implicit class MonadSyntax1[F[_], A](private  val fa: F[F[A]]) {
    def flatten1()(implicit M:Monad[F]): F[A] = M.flatten(fa)
  }
}
