package com.github.kright.arrayview

import scala.reflect.ClassTag

class ArrayView0dFlat[T](val data: Array[T]) extends ArrayView0d[T]:
  require(data.length == 1)

  override inline val offset = 0

  override def hasSimpleFlatLayout: Boolean = true

  override def toString: String =
    s"ArrayView0dFlat(data=$data)"


object ArrayView0dFlat:
  inline def apply[T: ClassTag]() =
    new ArrayView0dFlat[T](new Array[T](1))

  inline def apply[T](data: Array[T]) =
    new ArrayView0dFlat[T](data)