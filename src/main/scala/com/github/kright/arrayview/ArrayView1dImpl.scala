package com.github.kright.arrayview

class ArrayView1dImpl[T](override val data: Array[T],
                         override val shape0: Int,
                         override val offset: Int,
                         override val stride0: Int) extends ArrayView1d[T]:
  require(shape0 >= 0)

  override inline val hasSimpleFlatLayout = false

  override def toString: String =
    s"ArrayView1dImpl(data=$data, shape0=$shape0, offset=$offset, stride0=$stride0)"


object ArrayView1dImpl:
  def apply[T](data: Array[T], shape0: Int, offset: Int, stride0: Int): ArrayView1dImpl[T] =
    new ArrayView1dImpl[T](
      data,
      shape0 = shape0,
      offset = offset,
      stride0 = if (shape0 > 1) stride0 else 0
    )
  