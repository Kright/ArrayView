package com.github.kright.arrayview.benchmarks

import com.github.kright.arrayview.benchmarks.ArrayView4x4.matrixSize
import com.github.kright.arrayview.{ArrayView2d, ArrayView4d}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

/**
 * results on my PC:
 *
 * [info] Matrix4x4MultiplicationBenchmark.multiplicationOfArray               avgt   10  19.949 ± 1.029  ns/op
 * [info] Matrix4x4MultiplicationBenchmark.multiplicationOfArrayView           avgt   10  30.159 ± 0.542  ns/op
 * [info] Matrix4x4MultiplicationBenchmark.multiplicationOfArrayView4d         avgt   10  31.152 ± 0.079  ns/op
 * [info] Matrix4x4MultiplicationBenchmark.multiplicationOfArrayView4x4        avgt   10  24.593 ± 0.065  ns/op
 * [info] Matrix4x4MultiplicationBenchmark.multiplicationOfArrayView4x4double  avgt   10  21.221 ± 0.127  ns/op
 * [info] Matrix4x4MultiplicationBenchmark.multiplicationOfArrayViewV2         avgt   10  40.056 ± 0.322  ns/op
 * [info] Matrix4x4MultiplicationBenchmark.multiplicationOfArrays              avgt   10  40.982 ± 0.764  ns/op
 * [info] Matrix4x4MultiplicationBenchmark.multiplicationOfMatrix4x4           avgt   10  22.017 ± 0.079  ns/op
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class Matrix4x4MultiplicationBenchmark:
  var matrixArray: Array[Double] = uninitialized
  var matrixArray2: Array[Double] = uninitialized

  var matrixAsArrays: Array[Array[Double]] = uninitialized
  var matrixAsArrays2: Array[Array[Double]] = uninitialized

  var matrixArrayView: ArrayView2d[Double] = uninitialized
  var matrixArrayView2: ArrayView2d[Double] = uninitialized

  var matrix4x4: Matrix4x4 = uninitialized
  var matrix4x4v2: Matrix4x4 = uninitialized

  var arrayView4x4: ArrayView4x4[Double] = uninitialized
  var arrayView4x4v2: ArrayView4x4[Double] = uninitialized

  var arrayView4x4double: ArrayView4x4Double = uninitialized
  var arrayView4x4doubleV2: ArrayView4x4Double = uninitialized

  var arrayView4d: ArrayView4d[Double] = uninitialized
  var arrayView4dV2: ArrayView4d[Double] = uninitialized

  private def makeMatrixArray(): Array[Double] =
    val result = new Array[Double](matrixSize * matrixSize)
    for (i <- result.indices) {
      result(i) = util.Random.nextDouble()
    }
    result

  private def makeMatrixAsArrays(elems: Array[Double]): Array[Array[Double]] =
    val result = Array.ofDim[Double](matrixSize, matrixSize)
    for (i <- 0 until matrixSize; j <- 0 until matrixSize) {
      result(i)(j) = elems(i * matrixSize + j)
    }
    result

  private def makeMatrix4x4(elems: Array[Double]): Matrix4x4 =
    val result = new Matrix4x4
    for (i <- 0 until matrixSize; j <- 0 until matrixSize) {
      result(i, j) = elems(i * matrixSize + j)
    }
    result

  @Setup
  def setup(): Unit = {
    matrixArray = makeMatrixArray()
    matrixArray2 = makeMatrixArray()

    matrixAsArrays = makeMatrixAsArrays(matrixArray)
    matrixAsArrays2 = makeMatrixAsArrays(matrixArray2)

    matrixArrayView = ArrayView2d(matrixArray, matrixSize, matrixSize).copy
    matrixArrayView2 = ArrayView2d(matrixArray2, matrixSize, matrixSize).copy

    matrix4x4 = makeMatrix4x4(matrixArray)
    matrix4x4v2 = makeMatrix4x4(matrixArray2)

    arrayView4x4 = ArrayView4x4(matrixArray.clone())
    arrayView4x4v2 = ArrayView4x4(matrixArray2.clone())

    arrayView4x4double = ArrayView4x4Double(matrixArray)
    arrayView4x4doubleV2 = ArrayView4x4Double(matrixArray2)

    arrayView4d = matrixArrayView.copy.reshape(1, 1, 4, 4)
    arrayView4dV2 = matrixArrayView2.copy.reshape(1, 1, 4, 4)
  }

  @Benchmark
  def multiplicationOfArray(bh: Blackhole): Unit = {
    val a = matrixArray
    val b = matrixArray2

    val result = new Array[Double](matrixSize * matrixSize)

    loop(matrixSize) { y =>
      loop(matrixSize) { x =>
        var sum = 0.0
        loop(matrixSize) { k =>
          sum += a(y * matrixSize + k) * b(k * matrixSize + x)
        }
        result(y * matrixSize + x) = sum
      }
    }

    bh.consume(result)
  }

  @Benchmark
  def multiplicationOfArrays(bh: Blackhole): Unit = {
    val a = matrixAsArrays
    val b = matrixAsArrays2

    val result = Array.ofDim[Double](matrixSize, matrixSize)

    loop(matrixSize) { y =>
      loop(matrixSize) { x =>
        var sum = 0.0
        loop(matrixSize) { k =>
          sum += a(y)(k) * b(k)(x)
        }
        result(y)(x) = sum
      }
    }

    bh.consume(result)
  }

  @Benchmark
  def multiplicationOfArrayView(bh: Blackhole): Unit = {
    val a = matrixArrayView
    val b = matrixArrayView2

    val result = ArrayView2d[Double](matrixSize, matrixSize)

    loop(matrixSize) { y =>
      loop(matrixSize) { x =>
        var sum = 0.0
        loop(matrixSize) { k =>
          sum += a(y, k) * b(k, x)
        }
        result(y, x) = sum
      }
    }

    bh.consume(result)
  }

  @Benchmark
  def multiplicationOfArrayViewV2(bh: Blackhole): Unit = {
    val a = matrixArrayView
    val b = matrixArrayView2

    val result = ArrayView2d[Double](matrixSize, matrixSize)

    result.fill { (y, x) =>
      var sum = 0.0
      loop(matrixSize) { k =>
        sum += a(y, k) * b(k, x)
      }
      sum
    }

    bh.consume(result)
  }

  @Benchmark
  def multiplicationOfMatrix4x4(bh: Blackhole): Unit = {
    val a = matrix4x4
    val b = matrix4x4

    val result = new Matrix4x4

    loop(matrixSize) { y =>
      loop(matrixSize) { x =>
        var sum = 0.0
        loop(matrixSize) { k =>
          sum += a(y, k) * b(k, x)
        }
        result(y, x) = sum
      }
    }

    bh.consume(result)
  }

  @Benchmark
  def multiplicationOfArrayView4x4(bh: Blackhole): Unit = {
    val a = arrayView4x4
    val b = arrayView4x4v2

    val result = ArrayView4x4[Double]()

    loop(matrixSize) { y =>
      loop(matrixSize) { x =>
        var sum = 0.0
        loop(matrixSize) { k =>
          sum += a(y, k) * b(k, x)
        }
        result(y, x) = sum
      }
    }

    bh.consume(result)
  }

  @Benchmark
  def multiplicationOfArrayView4x4double(bh: Blackhole): Unit = {
    val a = arrayView4x4double
    val b = arrayView4x4doubleV2

    val result = new ArrayView4x4Double

    loop(matrixSize) { y =>
      loop(matrixSize) { x =>
        var sum = 0.0
        loop(matrixSize) { k =>
          sum += a(y, k) * b(k, x)
        }
        result(y, x) = sum
      }
    }

    bh.consume(result)
  }

  @Benchmark
  def multiplicationOfArrayView4d(bh: Blackhole): Unit = {
    val a = arrayView4d
    val b = arrayView4dV2

    val result = ArrayView4d[Double](1, 1, 4, 4)

    loop(matrixSize) { y =>
      loop(matrixSize) { x =>
        var sum = 0.0
        loop(matrixSize) { k =>
          sum += a(0, 0, y, k) * b(0, 0, k, x)
        }
        result(0, 0, y, x) = sum
      }
    }

    bh.consume(result)
  }

  private inline def loop(count: Int)(f: Int => Unit): Unit = {
    var i = 0
    while (i < count) {
      f(i)
      i += 1
    }
  }
