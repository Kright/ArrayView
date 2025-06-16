package com.github.kright.arrayview.benchmarks

import com.github.kright.arrayview.ArrayView2d

import scala.reflect.ClassTag

class ArrayView4x4[T](override val data: Array[T]) extends ArrayView2d[T]:

  override def getIndex(i0: Int, i1: Int): Int = i0 * size + i1

  override inline val hasSimpleFlatLayout = true
  override inline val shape0 = size
  override inline val shape1 = size
  override inline val offset = 0
  override inline val stride0 = size
  override inline val stride1 = 1


object ArrayView4x4:
  inline def apply[T: ClassTag](): ArrayView4x4[T] =
    new ArrayView4x4[T](new Array[T](16))

  inline def apply[T](array: Array[T]): ArrayView4x4[T] = 
    require(array.size == 16)
    new ArrayView4x4[T](array)