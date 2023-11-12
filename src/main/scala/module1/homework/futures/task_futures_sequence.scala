package module1.homework.futures

import module1.homework.futures.HomeworksUtils.TaskSyntax
import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */

  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {


    futures.foldRight(Future.successful(List.empty[A], List.empty[Throwable])) { case (f, acc) => {}
      val p = Promise[(List[A], List[Throwable])]

      acc.onSuccess({case (success, failure) => f.onComplete({
        case Failure(exception) => p.success((success, exception :: failure))
        case Success(value) => p.success(value :: success, failure)
      })})

      p.future
    }

  }
  //    task"Реализуйте метод `fullSequence`" ()
}
