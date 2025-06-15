package com.github.kright.arrayview

import AxisSize.all

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

  test("diagonal elements") {
    val arr = ArrayView2d(Array(1.0, 2.0, 3.0, 4.0), 2, 2)
    arr.diagonal.fill(9.0)

    assert(arr.data sameElements Array(9.0, 2.0, 3.0, 9.0))
  }
