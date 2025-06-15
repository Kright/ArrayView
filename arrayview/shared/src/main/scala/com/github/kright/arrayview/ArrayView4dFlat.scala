package com.github.kright.arrayview

import scala.reflect.ClassTag

class ArrayView4dFlat[T](override val data: Array[T],
                         override val shape0: Int,
                         override val shape1: Int,
                         override val shape2: Int,
                         override val shape3: Int) extends ArrayView4d[T]:
  
  require(data.length == shape0 * shape1 * shape2 * shape3)
  require(shape0 >= 0)
  require(shape1 >= 0)
  require(shape2 >= 0)
  require(shape3 >= 0)

  override inline val offset = 0

  override val stride0: Int = shape1 * shape2 * shape3 // store it, don't compute each time

  override val stride1: Int = shape2 * shape3

  override inline def stride2: Int = shape3

  override inline val stride3 = 1

  override inline val hasSimpleFlatLayout = true

  override def toString: String =
    s"ArrayView4dFlat(data=$data, shape0=$shape0, shape1=$shape1, shape2=$shape2, shape3=$shape3)"


object ArrayView4dFlat:
  def apply[T: ClassTag](shape0: Int, shape1: Int, shape2: Int, shape3: Int): ArrayView4dFlat[T] =
    new ArrayView4dFlat[T](new Array[T](shape0 * shape1 * shape2 * shape3), shape0, shape1, shape2, shape3)

  inline def apply[T](data: Array[T], shape0: Int, shape1: Int, shape2: Int, shape3: Int): ArrayView4dFlat[T] =
    new ArrayView4dFlat[T](data, shape0, shape1, shape2, shape3)  