object ListOps extends App{
  def sum(xs: List[Int]): Int = {
    if (xs.isEmpty) 0
    else xs.head + sum(xs.tail)
  }
 //val cal = List(12,56,89,445) test list
  //println(sum(cal)) test1

  def max(xs: List[Int]): Int ={

    var max1 = 0
    if (xs.isEmpty) {
      println("NoSuchElementExceptionError")
      return max1
    }
    var result = xs.head
    if (result > max1) {
      max1 = result
      var res: Int = max(xs.tail)
    if (res>max1) max1 = res}
    return max1
   }
  //println(max(cal)) test2

  def sum2(xs: List[Int]): Int ={
    def sumCheck(ints: List[Int], check: Int): Int = {
      ints match {
        case Nil => check
        case num :: tail => sumCheck(tail, check + num)
      }
    }
    sumCheck(xs,0)
  }

  //println(sum2(cal)) test3

  def max2(xs: List[Int]): Int = {
    def maxCheck(ints: List[Int], max1: Int): Int = {
      ints match {
        case Nil => max1
        case num :: tail =>
          val max2 = if (num > max1) num else max1
          maxCheck(tail,max2)
      }
    }
    maxCheck(xs,0)
  }
  //println(max2(cal)) test3
 val result = makeString(List("will", "fill", "until"), "[", "-", "]")
  println(result)

  def makeString(xs: List[String], pre: String, sep: String, post:
  String): String = xs match {
    case Nil => "Empty"
    case head :: tail => tail.foldLeft(pre+head)((r, e) =>
      r + sep + e) + post
  }

  }

