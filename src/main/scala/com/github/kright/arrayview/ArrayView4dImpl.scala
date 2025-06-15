package com.github.kright.arrayview

class ArrayView4dImpl[T](override val data: Array[T],
                         override val shape0: Int,
                         override val shape1: Int,
                         override val shape2: Int,
                         override val shape3: Int,
                         override val offset: Int,
                         override val stride0: Int,
                         override val stride1: Int,
                         override val stride2: Int,
                         override val stride3: Int) extends ArrayView4d[T]:
  require(shape0 >= 0)
  require(shape1 >= 0)
  require(shape2 >= 0)
  require(shape3 >= 0)

  override inline val hasSimpleFlatLayout = false

  override def toString: String =
    s"ArrayView4dImpl(data=$data, shape0=$shape0, shape1=$shape1, shape2=$shape2, shape3=$shape3, offset=$offset, stride0=$stride0, stride1=$stride1, stride2=$stride2, stride3=$stride3)"


object ArrayView4dImpl:
  def apply[T](data: Array[T], shape0: Int, shape1: Int, shape2: Int, shape3: Int, offset: Int, stride0: Int, stride1: Int, stride2: Int, stride3: Int): ArrayView4dImpl[T] =
    new ArrayView4dImpl[T](
      data,
      shape0 = shape0,
      shape1 = shape1,
      shape2 = shape2,
      shape3 = shape3,
      offset = offset,
      stride0 = if (shape0 > 1) stride0 else 0,
      stride1 = if (shape1 > 1) stride1 else 0,
      stride2 = if (shape2 > 1) stride2 else 0,
      stride3 = if (shape3 > 1) stride3 else 0
    )
