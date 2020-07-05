package ai.economicdatasciences.ch2

object FirstModule {
    def abs(n: Int): Int = {
        if(n < 0){
            -n
        } else {
            n
        }
    }

    def findFirstString(ss: Array[String], key: String): Int = {
        @annotation.tailrec
        def loop(n: Int): Int = {
            if(n >= ss.length) -1
            else if (ss(n) == key) n
            else loop(n + 1)
        }

        loop(0)
    }

    def findFirst[A](as: Array[A], p: A => Boolean): Int = {
        @annotation.tailrec
        def loop(n: Int): Int = {
            if(n >= as.length) -1
            else if (p(as(n))) n
            else loop(n + 1)
        }

        loop(0)
    }

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
        @annotation.tailrec
        def go(n: Int): Boolean = {
            if(n >= as.length-1) false
            else if (ordered(as(n),as(n+1)))
            else go(n + 1)

        }

        go(0)
    }

    // from the book, but I don't think this is right
    def factorial(n: Int): Int = {
        @annotation.tailrec
        def go(n: Int, acc: Int): Int = {
            if(n <= 0) {
                acc
            } else {
                go(n-1, n*acc)
            }
        }
        go(n, 1)
    }

    private def formatAbs(x: Int): Unit = {
        val msg = "The absolute value of %d is %d"
        msg.format(x, abs(x))
    }

    private def formatFactorial(n: Int): Unit = {
        val msg = "The factorial of %d is %d"
        msg.format(n, factorial(n))
    }

    def formatResult(name: String, n: Int, f: Int => Int) = {
        val msg = "This %s of %d is %d"
        msg.format(name, n, f(n))
    }

    def main(args: Array[String]): Unit = {
        val input = args(0).toInt
        println(formatAbs(input))
        println(formatFactorial(input))
    }
}
