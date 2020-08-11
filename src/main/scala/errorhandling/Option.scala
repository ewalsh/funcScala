package ai.economicdatasciences.errorhandling

sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = {
        this match {
            case None => None
            case Some(a) => Some(f(a))
        }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
        this match {
            case None => None
            case Some(a) => f(a)
        }
    }

    def getOrElse[B >: A](default: => B): B = {
        this match {
            case None => default
            case Some(a) => a
        }
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
        this match {
            case None => ob
            case Some(_) => this
        }
    }

    def filter(f: A => Boolean): Option[A] = {
        this match {
            case Some(a) if f(a) => this
            case _ => None
        }
    }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
    def failingFn(i: Int): Int = {
        val y: Int = throw new Exception("fail!")
        try {
            val x = 42 + 5
            x + y
        }
        catch { case e: Exception => 43 }
    }

    def mean(xs: Seq[Double]): Option[Double] = {
        if(xs.isEmpty) None
        else Some(xs.sum / xs.length)
    }

    def variance(xs: Seq[Double]): Option[Double] = {
        if(xs.isEmpty) None
        else {
            val m = xs.sum / xs.length
            val vs = xs.map(x => Math.pow(x - m, 2))
            Some(vs.sum / vs.length)
        }
    }

}
