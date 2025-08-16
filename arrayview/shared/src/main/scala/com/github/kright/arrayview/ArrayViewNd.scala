package com.github.kright.arrayview

import scala.reflect.ClassTag

trait ArrayViewNd[T, Self]:
  def data: Array[T]

  /**
   * means that this view covers all array elements and uses row-major order
   * [[https://en.wikipedia.org/wiki/Row-_and_column-major_order]]
   */
  def hasSimpleFlatLayout: Boolean

  /**
   * total number of available elements, product of sizes along each dimension
   */
  def size: Int

  def isEmpty: Boolean =
    size == 0

  def :=(source: Self): Unit

  def flatten(using ClassTag[T]): ArrayView1dFlat[T]

  def withSimpleLayout(using ClassTag[T]): Self

  def reshape(shape0: Int, shape1: Int)(using ClassTag[T]): ArrayView2d[T] =
    flatten.reshape(shape0, shape1)

  def reshape(shape0: Int, shape1: Int, shape2: Int)(using ClassTag[T]): ArrayView3d[T] =
    flatten.reshape(shape0, shape1, shape2)

  def reshape(shape0: Int, shape1: Int, shape2: Int, shape3: Int)(using ClassTag[T]): ArrayView4d[T] =
    flatten.reshape(shape0, shape1, shape2, shape3)
