package org.ddahl.commonsmath

import org.apache.commons.math3.random.RandomDataGenerator
import org.apache.commons.math3.util.FastMath.exp
import scala.collection.parallel.ParSeq

object Implicits {

  implicit val orderingVectorDouble = new Ordering[Vector[Double]]() {

    def compare(a: Vector[Double], b: Vector[Double]): Int = {
      if (a.size < b.size) -1
      else if (a.size > b.size) 1
      else {
        val ab = a.zip(b)
        var i = 0
        while ( i < ab.size ) {
          val x = ab(i)
          if (x._1 < x._2) return -1
          if (x._1 > x._2) return 1
          i += 1
        }
        0
      }
    }

  }

  implicit class RandomDataGeneratorImprovements(val rdg: RandomDataGenerator) {

    def nextRandomDataGenerator(): RandomDataGenerator = {
      val r = new RandomDataGenerator()
      r.reSeed(rdg.nextLong(Long.MinValue, Long.MaxValue))
      r
    }

    def nextRandomDataGenerators(nCores: Int = 0): ParSeq[RandomDataGenerator] = {
      val n = if (nCores == 0) Runtime.getRuntime.availableProcessors else nCores
      collection.parallel.immutable.ParSeq.fill(n) {
        val r = new RandomDataGenerator()
        r.reSeed(rdg.nextLong(Long.MinValue, Long.MaxValue))
        r
      }
    }

    def nextInt(weights: Array[Double], onLogScale: Boolean = false): Int = {
      if ( weights.length == 0 ) throw new IllegalArgumentException("Weights must not be length-zero.")
      val w = if ( onLogScale ) {
        var maxIndex = 0
        for ( i <- weights.indices ) {
          if ( weights(i) > weights(maxIndex) ) maxIndex = i
        }
        val max = weights(maxIndex)
        weights.map(y => exp(y-max))
      } else weights
      val target = w.sum * rdg.nextUniform(0,1)
      var i = 0
      var cumsum = w(i)
      while ( cumsum < target ) {
        i += 1
        cumsum += w(i)
      }
      i
    }

    def nextItem[A](x: IndexedSeq[(A,Double)], onLogScale: Boolean = false): (A,Double) = {
      if ( x.length == 0 ) throw new IllegalArgumentException("Weights must not be length-zero.")
      val w = if ( onLogScale ) {
        val xx = x.map(_._2)
        var maxIndex = 0
        for ( i <- xx.indices ) {
          if ( xx(i) > xx(maxIndex) ) maxIndex = i
        }
        val max = xx(maxIndex)
        xx.map(y => exp(y-max))
      } else x.map(_._2)
      val wsum = w.sum
      val target = wsum * rdg.nextUniform(0,1)
      var i = 0
      var cumsum = w(i)
      while ( cumsum < target ) {
        i += 1
        cumsum += w(i)
      }
      (x(i)._1, w(i)/wsum)
    }

  }

}

