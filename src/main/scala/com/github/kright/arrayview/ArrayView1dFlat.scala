package com.github.kright.arrayview

import scala.reflect.ClassTag

class ArrayView1dFlat[T](override val data: Array[T]) extends ArrayView1d[T]:
  require(shape0 >= 0)

  override def shape0: Int = data.length

  override inline val offset = 0

  override inline val stride0 = 1

  override inline val hasSimpleFlatLayout = true

  override def flatten(using ClassTag[T]): ArrayView1dFlat[T] = this

  override def toString: String =
    s"ArrayView1dFlat(data=$data, shape0=$shape0)"

  override def reshape(shape0: Int, shape1: Int)(using ClassTag[T]): ArrayView2dFlat[T] =
    ArrayView2d(data, shape0, shape1)

  override def reshape(shape0: Int, shape1: Int, shape2: Int)(using ClassTag[T]): ArrayView3dFlat[T] =
    ArrayView3d(data, shape0, shape1, shape2)

  override def reshape(shape0: Int, shape1: Int, shape2: Int, shape3: Int)(using ClassTag[T]): ArrayView4dFlat[T] =
    ArrayView4d(data, shape0, shape1, shape2, shape3)


object ArrayView1dFlat:
  def apply[T: ClassTag](shape0: Int): ArrayView1dFlat[T] =
    new ArrayView1dFlat[T](new Array[T](shape0))

  inline def apply[T](data: Array[T]): ArrayView1dFlat[T] =
    new ArrayView1dFlat[T](data)