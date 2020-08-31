package ai.economicdatasciences.errorhandling

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}

sealed trait Either[+E, +A]
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
    def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
        if(xs.isEmpty){
            Left("mean of empty list")
        } else {
            Right(xs.sum / xs.length)
        }
    }

    def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
        try Right(x/y)
        catch {
            case e: Exception => Left(e)
        }
    }

    def Try[A](a: => A): Either[Exception, A] = {
        try Right(a)
        catch {
            case e: Exception => Left(e)
        }
    }
}
