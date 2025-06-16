package com.github.kright.arrayview.benchmarks

final class Matrix4x4:
  inline val w = 4
  inline val h = 4
  val elements: Array[Double] = new Array[Double](16)

  def apply(y: Int, x: Int): Double =
    elements(y * w + x)

  def update(y: Int, x: Int, v: Double): Unit =
    elements(y * w + x) = v
