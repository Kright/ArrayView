package com.github.kright.arrayview

class ArrayView0dImpl[T](val data: Array[T],
                         val offset: Int) extends ArrayView0d[T]:

  override inline val hasSimpleFlatLayout = false

  override def toString: String =
    s"ArrayView0dImpl(data=$data, offset=$offset)"
