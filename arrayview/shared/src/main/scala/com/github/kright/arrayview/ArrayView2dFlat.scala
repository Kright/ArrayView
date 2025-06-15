package com.github.kright.arrayview

import scala.reflect.ClassTag

class ArrayView2dFlat[T](override val data: Array[T],
                         override val shape0: Int,
                         override val shape1: Int) extends ArrayView2d[T]:
  require(data.length == shape0 * shape1)
  require(shape0 >= 0)
  require(shape1 >= 0)

  override inline val offset = 0

  override inline def stride0: Int = shape1

  override inline val stride1 = 1

  override inline val hasSimpleFlatLayout = true

  override def toString: String =
    s"ArrayView2dFlat(data=$data, shape0=$shape0, shape1=$shape1)"


object ArrayView2dFlat:
  def apply[T: ClassTag](shape0: Int, shape1: Int): ArrayView2dFlat[T] =
    new ArrayView2dFlat[T](new Array[T](shape0 * shape1), shape0, shape1)

  inline def apply[T](data: Array[T], shape0: Int, shape1: Int): ArrayView2dFlat[T] =
    new ArrayView2dFlat[T](data, shape0, shape1)
