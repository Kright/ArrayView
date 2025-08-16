package com.github.kright.arrayview.benchmarks

import com.github.kright.arrayview.ArrayView2d
import com.github.kright.arrayview.benchmarks.ArrayView4x4.matrixSize

class ArrayView4x4Double extends ArrayView2d[Double]:

  override val data = new Array[Double](matrixSize * matrixSize)

  override def getIndex(i0: Int, i1: Int): Int = i0 * matrixSize + i1

  override inline val hasSimpleFlatLayout = true
  override inline val shape0 = matrixSize
  override inline val shape1 = matrixSize
  override inline val offset = 0
  override inline val stride0 = matrixSize
  override inline val stride1 = 1


object ArrayView4x4Double:
  def apply(arr: Array[Double]): ArrayView4x4Double = 
    require(arr.size == 16)
    val r = new ArrayView4x4Double
    for(i <- 0 until 16) {
      r.data(i) = arr(i)
    }
    r
  
    