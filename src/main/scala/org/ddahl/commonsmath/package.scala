package org.ddahl

import org.apache.commons.math3.random.RandomDataGenerator
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, LUDecomposition, RealMatrix, RealMatrixFormat}
import org.apache.commons.math3.util.FastMath.exp
import scala.collection.parallel.ParSeq
import java.text.DecimalFormat

package object commonsmath {

  implicit val orderingVectorDouble = new Ordering[Vector[Double]]() {

    def compare(a: Vector[Double], b: Vector[Double]): Int = {
      if (a.size < b.size) -1
      else if (a.size > b.size) 1
      else {
        a.zip(b).foreach(x => {
          if (x._1 < x._2) return -1
          if (x._1 > x._2) return 1
        })
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
      Range(0, n).map(i => {
        val r = new RandomDataGenerator()
        r.reSeed(rdg.nextLong(Long.MinValue, Long.MaxValue))
        r
      }).par
    }

    def nextInt(weights: Array[Double], onLogScale: Boolean = false): Int = {
      if ( weights.length == 0 ) throw new IllegalArgumentException("Weights must not be length-zero.")
      val w = if ( onLogScale ) weights.map(exp) else weights
      val target = w.sum * rdg.nextUniform(0,1)
      var i = 0
      var cumsum = w(i)
      while ( cumsum < target ) {
        i += 1
        cumsum += w(i)
      }
      i
    }

    def nextItem[A](x: IndexedSeq[(A,Double)], onLogScale: Boolean = false): A = {
      if ( x.length == 0 ) throw new IllegalArgumentException("Weights must not be length-zero.")
      val w = if ( onLogScale ) x.map(y => exp(y._2)) else x.map(_._2)
      val target = w.sum * rdg.nextUniform(0,1)
      var i = 0
      var cumsum = w(i)
      while ( cumsum < target ) {
        i += 1
        cumsum += w(i)
      }
      x(i)._1
    }

  }

  object MatrixFactory {

    def apply(row:Int, column: Int) = new Array2DRowRealMatrix(row, column)

    def apply(x: Array[Array[Double]], copyArray: Boolean = true): Array2DRowRealMatrix = new Array2DRowRealMatrix(x, copyArray)
    def apply(x: Array[Array[Int]]): Array2DRowRealMatrix = new Array2DRowRealMatrix(x.map(_.map(_.toDouble)))

    def ones(nRows: Int, nColumns: Int): Array2DRowRealMatrix = new Array2DRowRealMatrix(Array.fill(nRows, nColumns)(1.0), false)

    def identity(size: Int): Array2DRowRealMatrix = {
      val r = new Array2DRowRealMatrix(size, size)
      for (i <- 0 until size) r.setEntry(i, i, 1.0)
      r
    }

  }

  implicit class RichArray2DRowRealMatrix(m: Array2DRowRealMatrix) {

    def +(x: Array2DRowRealMatrix): Array2DRowRealMatrix = m.add(x)
    def +(x: RealMatrix): RealMatrix = m.add(x)
    def +(x: Array[Array[Double]]): Array2DRowRealMatrix = m.add(MatrixFactory(x,false))
    def +(x: Array[Array[Int]]): Array2DRowRealMatrix = m.add(MatrixFactory(x.map(_.map(_.toDouble)),false))

    def -(x: Array2DRowRealMatrix): Array2DRowRealMatrix = m.subtract(x)
    def -(x: RealMatrix): RealMatrix = m.subtract(x)
    def -(x: Array[Array[Double]]): Array2DRowRealMatrix = m.subtract(MatrixFactory(x,false))
    def -(x: Array[Array[Int]]): Array2DRowRealMatrix = m.subtract(MatrixFactory(x.map(_.map(_.toDouble)),false))

    def *(x: Array2DRowRealMatrix): Array2DRowRealMatrix = m.multiply(x)
    def *(x: RealMatrix): RealMatrix = m.multiply(x)
    def *(x: Array[Array[Double]]): Array2DRowRealMatrix = m.multiply(MatrixFactory(x,false))
    def *(x: Array[Array[Int]]): Array2DRowRealMatrix = m.multiply(MatrixFactory(x.map(_.map(_.toDouble)),false))

    def +:(c: Double): RealMatrix = m.scalarAdd(c)

    def :+(c: Double): RealMatrix = m.scalarAdd(c)

    def :-(c: Double): RealMatrix = m.scalarAdd(-c)

    def :*(c: RealMatrix): RealMatrix = {
      val mm = m.copy()
      for (i <- 0 until mm.getRowDimension) {
        for (j <- 0 until mm.getColumnDimension) {
          mm.multiplyEntry(i, j, c.getEntry(i, j))
        }
      }
      mm
    }

    def *:(c: Double): RealMatrix = m.scalarMultiply(c)

    def :*(c: Double): RealMatrix = m.scalarMultiply(c)

    def :/(c: Double): RealMatrix = m.scalarMultiply(1 / c)

    def t: RealMatrix = m.transpose()

    def inverse: RealMatrix = new LUDecomposition(m).getSolver().getInverse()

    def det: Double = new LUDecomposition(m).getDeterminant()

    def norm: Double = m.getNorm()

    def rows: Int = m.getRowDimension()

    def columns: Int = m.getColumnDimension()

    def dim: (Int, Int) = (m.getRowDimension(), m.getColumnDimension())

    def map(f: Double => Double): RealMatrix =  MatrixFactory(m.getData().map(_.map(f)),false)

    def sum: Double =  (MatrixFactory.ones(1,m.columns) * m * MatrixFactory.ones(m.rows,1)).getEntry(0,0)

    def apply(i: Int, j: Int): Double = m.getEntry(i, j)

    def apply(i: Nil.type, j: Int): RealMatrix = m.getColumnMatrix(j)

    def apply(i: Int, j: Nil.type): RealMatrix = m.getRowMatrix(i)

    def apply(i: Range, j: Int): RealMatrix = m.getSubMatrix(i.toArray, Array(j))

    def apply(i: Int, j: Range): RealMatrix = m.getSubMatrix(Array(i), j.toArray)

    def apply(i: Seq[Int], j: Seq[Int]): RealMatrix = m.getSubMatrix(i.toArray, j.toArray)

    def update(i: Int, j: Int, x: Double): Unit = m.setEntry(i, j, x)

    def update(i: Nil.type, j: Int, x: RealMatrix): Unit = m.setColumnMatrix(j, x)

    def update(i: Int, j: Nil.type, x: RealMatrix): Unit = m.setRowMatrix(i, x)

    def update(i: Range, j: Int, x: RealMatrix): Unit = {
      var iii = 0
      for (ii <- i) {
        m.setEntry(ii, j, x.getEntry(iii, 0))
        iii += 1
      }
    }

    def update(i: Int, j: Range, x: RealMatrix): Unit = {
      var jjj = 0
      for (jj <- j) {
        m.setEntry(i, jj, x.getEntry(0, jjj))
        jjj += 1
      }
    }

    def update(i: Seq[Int], j: Seq[Int], x: RealMatrix): Unit = {
      var iii = 0
      for (ii <- i) {
        var jjj = 0
        for (jj <- j) {
          m.setEntry(ii, jj, x.getEntry(iii, jjj))
          jjj += 1
        }
        iii += 1
      }
    }

    def toVectorOfVectors: Vector[Vector[Double]] = m.getData.map(_.toVector).toVector

    def toPrettyString(pattern: String = "#0.000000") = {
      val rmf = new RealMatrixFormat("", "", "", "", "\n", " ", new DecimalFormat(pattern))
      rmf.format(m)
    }


  }

  implicit class RichRealMatrix(m: RealMatrix) {

    def +(x: RealMatrix): RealMatrix = m.add(x)
    def +(x: Array[Array[Double]]): RealMatrix = m.add(MatrixFactory(x,false))
    def +(x: Array[Array[Int]]): RealMatrix = m.add(MatrixFactory(x.map(_.map(_.toDouble)),false))

    def -(x: RealMatrix): RealMatrix = m.subtract(x)
    def -(x: Array[Array[Double]]): RealMatrix = m.subtract(MatrixFactory(x,false))
    def -(x: Array[Array[Int]]): RealMatrix = m.subtract(MatrixFactory(x.map(_.map(_.toDouble)),false))

    def *(x: RealMatrix): RealMatrix = m.multiply(x)
    def *(x: Array[Array[Double]]): RealMatrix = m.multiply(MatrixFactory(x,false))
    def *(x: Array[Array[Int]]): RealMatrix = m.multiply(MatrixFactory(x.map(_.map(_.toDouble)),false))

    def +:(c: Double): RealMatrix = m.scalarAdd(c)

    def :+(c: Double): RealMatrix = m.scalarAdd(c)

    def :-(c: Double): RealMatrix = m.scalarAdd(-c)

    def :*(c: RealMatrix): RealMatrix = {
      val mm = m.copy()
      for (i <- 0 until mm.getRowDimension) {
        for (j <- 0 until mm.getColumnDimension) {
          mm.multiplyEntry(i, j, c.getEntry(i, j))
        }
      }
      mm
    }

    def *:(c: Double): RealMatrix = m.scalarMultiply(c)

    def :*(c: Double): RealMatrix = m.scalarMultiply(c)

    def :/(c: Double): RealMatrix = m.scalarMultiply(1 / c)

    def t: RealMatrix = m.transpose()

    def inverse: RealMatrix = new LUDecomposition(m).getSolver().getInverse()

    def det: Double = new LUDecomposition(m).getDeterminant()

    def norm: Double = m.getNorm()

    def rows: Int = m.getRowDimension()

    def columns: Int = m.getColumnDimension()

    def dim: (Int, Int) = (m.getRowDimension(), m.getColumnDimension())

    def map(f: Double => Double): RealMatrix =  MatrixFactory(m.getData().map(_.map(f)),false)

    def sum: Double =  (MatrixFactory.ones(1,m.columns) * m * MatrixFactory.ones(m.rows,1)).getEntry(0,0)

    def apply(i: Int, j: Int): Double = m.getEntry(i, j)

    def apply(i: Nil.type, j: Int): RealMatrix = m.getColumnMatrix(j)

    def apply(i: Int, j: Nil.type): RealMatrix = m.getRowMatrix(i)

    def apply(i: Range, j: Int): RealMatrix = m.getSubMatrix(i.toArray, Array(j))

    def apply(i: Int, j: Range): RealMatrix = m.getSubMatrix(Array(i), j.toArray)

    def apply(i: Seq[Int], j: Seq[Int]): RealMatrix = m.getSubMatrix(i.toArray, j.toArray)

    def update(i: Int, j: Int, x: Double): Unit = m.setEntry(i, j, x)

    def update(i: Nil.type, j: Int, x: RealMatrix): Unit = m.setColumnMatrix(j, x)

    def update(i: Int, j: Nil.type, x: RealMatrix): Unit = m.setRowMatrix(i, x)

    def update(i: Range, j: Int, x: RealMatrix): Unit = {
      var iii = 0
      for (ii <- i) {
        m.setEntry(ii, j, x.getEntry(iii, 0))
        iii += 1
      }
    }

    def update(i: Int, j: Range, x: RealMatrix): Unit = {
      var jjj = 0
      for (jj <- j) {
        m.setEntry(i, jj, x.getEntry(0, jjj))
        jjj += 1
      }
    }

    def update(i: Seq[Int], j: Seq[Int], x: RealMatrix): Unit = {
      var iii = 0
      for (ii <- i) {
        var jjj = 0
        for (jj <- j) {
          m.setEntry(ii, jj, x.getEntry(iii, jjj))
          jjj += 1
        }
        iii += 1
      }
    }

    def toVectorOfVectors: Vector[Vector[Double]] = m.getData.map(_.toVector).toVector

    def toPrettyString(pattern: String = "#0.000000") = {
      val rmf = new RealMatrixFormat("", "", "", "", "\n", " ", new DecimalFormat(pattern))
      rmf.format(m)
    }

  }

}

