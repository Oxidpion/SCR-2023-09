package module2.homework

import zio.clock.{Clock, currentTime, sleep}
import zio.console._
import zio.duration.durationInt
import zio.random._
import zio.{ExitCode, URIO, ZIO}

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.collection.immutable
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  type AppEnv = Clock with Random with Console

  lazy val guessProgram: ZIO[AppEnv, IOException, Boolean] = for {
    secret <- nextIntBetween(1, 4)
    _ <- putStrLn("Угадай число?")
    n <- getStrLn
    success <- ZIO.succeed(secret.toString == n)
    _ <- if (success) putStrLn("Угадал") else putStrLn("Не угадал")
  } yield success

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  def doWhile[R, E, A](fa: ZIO[R, E, A])(f: A => Boolean): ZIO[R, E, A] = for {
    a <- fa
    _ <- if (f(a)) ZIO.unit else doWhile(fa)(f)
  } yield a


  /**
   * 3. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 3.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: URIO[AppEnv, Int] = for {
    _ <- sleep(1 second)
    n <- nextIntBetween(0, 11)
  } yield n

  /**
   * 3.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: List[URIO[AppEnv, Int]] = List.range(0, 10).map(_ => eff)


  /**
   * 3.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  def printEffectRunningTime[A](zio: ZIO[AppEnv, IOException, A]): ZIO[AppEnv, IOException, A] = for {
    start <- currentTime(TimeUnit.MILLISECONDS)
    r <- zio
    end <- currentTime(TimeUnit.MILLISECONDS)
    _ <- putStrLn(s"Running time ${end - start}")
  } yield r

  lazy val app = ZIO.collectAll(effects).map(_.sum)


  /**
   * 3.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = ZIO.collectAllPar(effects).map(_.sum)


  /**
   * 4. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */


   /**
     * 5.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg = printEffectRunningTime(app.map(_.toString) >>= (s => putStrLn(s)))
  lazy val appWithTimeLoggSpeed = printEffectRunningTime(appSpeedUp.map(_.toString) >>= (s => putStrLn(s)))

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp:  URIO[AppEnv, ExitCode] = (appWithTimeLogg *> appWithTimeLoggSpeed).exitCode

}
