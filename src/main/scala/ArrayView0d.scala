package com.github.kright.arrayview

import scala.reflect.ClassTag

/**
 * Scalar - just one element in array
 * Difference from the element itself is broadcasting and ability to assign new element
 *
 * @tparam T
 */
trait ArrayView0d[T] extends ArrayViewNd[T, ArrayView0d[T]]:
  def offset: Int

  def apply(): T =
    data(offset)

  def update(t: T): Unit =
    data(offset) = t

  override def :=(source: ArrayView0d[T]): Unit =
    data(offset) = source()

  def copy(using ClassTag[T]): ArrayView0dFlat[T] =
    val r = ArrayView0dFlat()
    r := this
    r

  override def flatten(using ClassTag[T]): ArrayView1dFlat[T] =
    if (hasSimpleFlatLayout) ArrayView1dFlat(data)
    else copy.flatten

object ArrayView0d:
  inline def apply[T: ClassTag](): ArrayView0dFlat[T] =
    ArrayView0dFlat()

  def apply[T](data: Array[T], offset: Int): ArrayView0d[T] =
    if (data.length == 1 && offset == 0) ArrayView0dFlat(data)
    else ArrayView0dImpl(data, offset)
