package com.github.kright.arrayview

import scala.reflect.ClassTag

class ArrayView3dFlat[T](override val data: Array[T],
                         override val shape0: Int,
                         override val shape1: Int,
                         override val shape2: Int) extends ArrayView3d[T]:
  require(data.length == shape0 * shape1 * shape2)
  require(shape0 >= 0)
  require(shape1 >= 0)
  require(shape2 >= 0)

  override inline val offset = 0

  override val stride0: Int = shape1 * shape2 // store it, don't compute each time

  override inline def stride1: Int = shape2

  override inline val stride2 = 1

  override inline val hasSimpleFlatLayout = true

  override def toString: String =
    s"ArrayView3dFlat(data=$data, shape0=$shape0, shape1=$shape1, shape2=$shape2)"


object ArrayView3dFlat:
  def apply[T: ClassTag](shape0: Int, shape1: Int, shape2: Int): ArrayView3dFlat[T] =
    new ArrayView3dFlat[T](new Array[T](shape0 * shape1 * shape2), shape0, shape1, shape2)

  def apply[T](data: Array[T], shape0: Int, shape1: Int, shape2: Int): ArrayView3dFlat[T] =
    new ArrayView3dFlat[T](data, shape0, shape1, shape2)
