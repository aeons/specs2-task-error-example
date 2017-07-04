import cats.data.EitherT
import fs2._
import fs2.interop.cats._
import org.specs2.matcher._
import org.specs2.mutable.Specification

trait ExampleMatchers extends Matchers with TaskMatchers {
  def returnRight[A, B](m: ValueCheck[B]): Matcher[EitherT[Task, A, B]] =
    beRight(m) ^^ { et: EitherT[Task, A, B] =>
      et.value.unsafeRun aka "the either task"
    }

  def returnLeft[A, B](m: ValueCheck[A]): Matcher[EitherT[Task, A, B]] =
    beLeft(m) ^^ { et: EitherT[Task, A, B] =>
      et.value.unsafeRun aka "the either task"
    }
}

class ExampleSpec extends Specification with AnyMatchers with ExampleMatchers {

  implicit val S = Strategy.fromExecutionContext(scala.concurrent.ExecutionContext.global)

  sealed case class ExampleFailure(value: Int) extends RuntimeException

  type ExampleResult[A] = EitherT[Task, ExampleFailure, A]

  "Example" can {
    "fail" in {
      def success(value: Int): Either[ExampleFailure, Int] = Right(value)
      def failure(value: Int): Either[ExampleFailure, Int] = Left(ExampleFailure(value))

      val et: ExampleResult[Int] = EitherT(Task(success(13 * 37)))

      et.flatMap(_ => EitherT(Task(failure(42)))) must returnLeft(ExampleFailure(42))
    }
  }
}
