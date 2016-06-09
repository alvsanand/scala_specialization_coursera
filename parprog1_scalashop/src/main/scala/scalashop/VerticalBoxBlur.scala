package scalashop

import java.util.concurrent.ForkJoinTask

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

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
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
    * `dst`, starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each column, `blur` traverses the pixels by going from top to
    * bottom.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    //    println(s"${(new java.util.Date()).toString} - ${Thread.currentThread().getName} - blur")
    for (x <- from until end; y <- 0 until src.height) dst(x, y) = boxBlurKernel(src, x, y, radius)
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * columns.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    def parTask(iter: List[List[Int]]): Unit = {
      if (iter.size > 0) {
        //        println(s"${(new java.util.Date()).toString} - ${Thread.currentThread().getName} - paralel : ${iter.size}")
        val c = iter.head

        parallel(parTask(iter.tail), blur(src, dst, c.head, c.last + 1, radius))
      }
    }

    val groups = if (dst.width / numTasks > 0) dst.width / numTasks else 1
    parTask((0 until dst.width).grouped(groups).toList.map(x => x.toList))
  }
}