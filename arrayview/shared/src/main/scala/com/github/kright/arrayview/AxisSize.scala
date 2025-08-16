package com.github.kright.arrayview

object AxisSize:
  opaque type Size = Int

  inline def size(using size: AxisSize.Size): Int = size

  inline def all(using size: AxisSize.Size): Range = 0 until size

  inline def withAxisSize[T](size: Int, inline f: AxisSize.Size ?=> T): T = 
    f(using size)
