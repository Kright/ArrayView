package com.github.kright.arrayview // imports Quotes, Expr


private[arrayview] object ArrayViewUtil:
  private def checkRange(first: Int, last: Int, max: Int): Unit =
    require(0 <= first && first < max)
    require(0 <= last && last < max)

  inline def getFirst[T <: Int | Range](inline t: T, inline max: Int): Int =
    inline (t) match {
      case (i: Int) => {
        require(0 <= i && i < max)
        i
      }
      case (r: Range) => {
        if (r.nonEmpty) {
          val first = r.start
          val last = r.last
          checkRange(first, last, max)
          first
        } else 0
      }
    }

  inline def loop(count: Int)(f: Int => Unit): Unit =
    var i = 0
    while (i < count) {
      f(i)
      i += 1
    }  
