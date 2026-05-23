package me.kright.arrayview

import me.kright.arrayview.AxisSize.all
import org.scalatest.funsuite.AnyFunSuite

class SimpleTest extends AnyFunSuite:

  test("data array has proper type") {
    assert(ArrayView2d[Double](2, 4).data.getClass.getSimpleName == "double[]")
    assert(ArrayView3d[Int](1, 2, 3).data.getClass.getSimpleName == "int[]")
    assert(ArrayView3d[Int](1, 2, 3).data.toString.startsWith("[I@"))

    assert(ArrayView3d[Integer](1, 2, 3).data.getClass.getSimpleName == "Integer[]")
    assert(ArrayView3d[Integer](1, 2, 3).data.toString.startsWith("[Ljava.lang.Integer;"))
  }

  test("data array preserve proper type") {
    assert(ArrayView2d[Double](2, 4).map(_ + 1.0).data.getClass.getSimpleName == "double[]")
    assert(ArrayView2d[Double](2, 4).map(_.toInt).data.getClass.getSimpleName == "int[]")
  }

  test("set by view") {
    val arr = ArrayView2d[Double](2, 4)
    arr.view(0, all.reverse) := ArrayView1d(Array(1.0, 2.0, 3.0, 4.0))
    arr.view(1, all by 2) := ArrayView1d(Array(9.0, 9.0))

    assert(arr.data sameElements Array(4.0, 3.0, 2.0, 1.0, 9.0, 0.0, 9.0, 0.0))
  }

  test("mapTo") {
    val arr = ArrayView2d[Int](2, 2)
    arr.fill((i0, i1) => i0 * 10 + i1)
    val result = ArrayView2d[Int](2, 2)
    arr.mapTo(_ + 1, result)
    assert(result(0, 0) == 1)
    assert(result(0, 1) == 2)
    assert(result(1, 0) == 11)
    assert(result(1, 1) == 12)
  }

  test("mapWithIndexTo") {
    val arr = ArrayView2d[Int](2, 2)
    val result = ArrayView2d[Int](2, 2)
    arr.mapWithIndexTo((_, i0, i1) => i0 + i1, result)
    assert(result(0, 0) == 0)
    assert(result(0, 1) == 1)
    assert(result(1, 0) == 1)
    assert(result(1, 1) == 2)
  }

  test("mapTo 3d") {
    val arr = ArrayView3d[Int](2, 2, 2)
    arr.fill((i0, i1, i2) => i0 * 100 + i1 * 10 + i2)
    val result = ArrayView3d[Int](2, 2, 2)
    arr.mapTo(_ + 1, result)
    assert(result(0, 0, 0) == 1)
    assert(result(1, 1, 1) == 112)
  }

  test("mapWithIndexTo 3d") {
    val arr = ArrayView3d[Int](2, 2, 2)
    val result = ArrayView3d[Int](2, 2, 2)
    arr.mapWithIndexTo((_, i0, i1, i2) => i0 + i1 + i2, result)
    assert(result(0, 0, 0) == 0)
    assert(result(1, 1, 1) == 3)
  }

  test("mapTo 4d") {
    val arr = ArrayView4d[Int](2, 2, 2, 2)
    arr.fill((i0, i1, i2, i3) => i0 * 1000 + i1 * 100 + i2 * 10 + i3)
    val result = ArrayView4d[Int](2, 2, 2, 2)
    arr.mapTo(_ + 1, result)
    assert(result(0, 0, 0, 0) == 1)
    assert(result(1, 1, 1, 1) == 1112)
  }

  test("mapWithIndexTo 4d") {
    val arr = ArrayView4d[Int](2, 2, 2, 2)
    val result = ArrayView4d[Int](2, 2, 2, 2)
    arr.mapWithIndexTo((_, i0, i1, i2, i3) => i0 + i1 + i2 + i3, result)
    assert(result(0, 0, 0, 0) == 0)
    assert(result(1, 1, 1, 1) == 4)
  }

  test("mapTo 1d") {
    val arr = ArrayView1d[Int](5)
    arr.fill(i => i)
    val result = ArrayView1d[Int](5)
    arr.mapTo(_ + 1, result)
    assert(result.data sameElements Array(1, 2, 3, 4, 5))
  }

  test("mapWithIndexTo 1d") {
    val arr = ArrayView1d[Int](5)
    val result = ArrayView1d[Int](5)
    arr.mapWithIndexTo((_, i) => i * 2, result)
    assert(result.data sameElements Array(0, 2, 4, 6, 8))
  }

  test("diagonal elements") {
    val arr = ArrayView2d(Array(1.0, 2.0, 3.0, 4.0), 2, 2)
    arr.diagonal.fill(9.0)

    assert(arr.data sameElements Array(9.0, 2.0, 3.0, 9.0))
  }
