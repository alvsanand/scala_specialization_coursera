package scalashop

import org.scalameter._
import common._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
    * starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each row, `blur` traverses the pixels by going from left to right.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
//    println(s"${(new java.util.Date()).toString} - ${Thread.currentThread().getName} - blur")
    for (x <- 0 until src.width; y <- from until end) dst(x, y) = boxBlurKernel(src, x, y, radius)
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * rows.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    def parTask(iter: List[List[Int]]): Unit = {
      if (iter.size > 0){
//        println(s"${(new java.util.Date()).toString} - ${Thread.currentThread().getName} - paralel : ${iter.size}")
        val c = iter.head

        parallel(parTask(iter.tail), blur(src, dst, c.head, c.last + 1, radius))
      }
    }

    val groups = if(dst.height/numTasks>0) dst.height/numTasks else 1
    parTask((0 until dst.height).grouped(groups).toList.map(x => x.toList))
  }

}
