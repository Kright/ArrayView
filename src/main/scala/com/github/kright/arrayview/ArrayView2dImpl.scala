package com.github.kright.arrayview


class ArrayView2dImpl[T](override val data: Array[T],
                         override val shape0: Int,
                         override val shape1: Int,
                         override val offset: Int,
                         override val stride0: Int,
                         override val stride1: Int) extends ArrayView2d[T]:
  require(shape0 >= 0)
  require(shape1 >= 0)

  override inline val hasSimpleFlatLayout = false

  override def toString: String =
    s"ArrayView2dImpl(data=$data, shape0=$shape0, shape1=$shape1, offset=$offset, stride0=$stride0, stride1=$stride1)"


object ArrayView2dImpl:
  def apply[T](data: Array[T], shape0: Int, shape1: Int, offset: Int, stride0: Int, stride1: Int): ArrayView2dImpl[T] =
    new ArrayView2dImpl[T](
      data,
      shape0 = shape0,
      shape1 = shape1,
      offset = offset,
      stride0 = if (shape0 > 1) stride0 else 0,
      stride1 = if (shape1 > 1) stride1 else 0
    )
