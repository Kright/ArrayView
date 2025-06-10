package com.github.kright.arrayview

import ArrayViewUtil.loop

import scala.reflect.ClassTag


trait ArrayView2d[T] extends ArrayViewNd[T, ArrayView2d[T]]:
  
  def shape0: Int
  def shape1: Int

  def offset: Int

  def stride0: Int
  def stride1: Int

  def getPos(i0: Int, i1: Int): Int =
    offset + stride0 * i0 + stride1 * i1

  inline def apply(i0: Int, i1: Int): T =
    data(getPos(i0, i1))

  inline def update(i0: Int, i1: Int, value: T): Unit =
    data(getPos(i0, i1)) = value

  def containsIndex(i0: Int, i1: Int): Boolean =
    0 <= i0 && i0 < shape0 &&
      0 <= i1 && i1 < shape1

  def hasSameSize(other: ArrayView2d[?]): Boolean =
    shape0 == other.shape0 && shape1 == other.shape1

  def isSquare: Boolean =
    shape0 == shape1


  inline def foreachIndex(f: (Int, Int) => Unit): Unit = {
    loop(shape0) { i0 =>
      loop(shape1) { i1 =>
        f(i0, i1)
      }
    }
  }

  inline def foreach(f: T => Unit): Unit =
    foreachIndex { (i0, i1) =>
      f(this (i0, i1))
    }

  inline def foreachWithIndex(f: (T, Int, Int) => Unit): Unit =
    foreachIndex { (i0, i1) =>
      f(this (i0, i1), i0, i1)
    }


  inline def mapInplace(f: T => T): Unit =
    foreachIndex { (i0, i1) =>
      val pos = getPos(i0, i1)
      this.data(pos) = f(this.data(pos))
    }

  inline def mapWithIndexInplace(f: (T, Int, Int) => T): Unit =
    foreachIndex { (i0, i1) =>
      val pos = getPos(i0, i1)
      this.data(pos) = f(this.data(pos), i0, i1)
    }

  inline def map[U: ClassTag](f: T => U) =
    val r = ArrayView2d[U](shape0, shape1)
    foreachWithIndex { (v, i0, i1) =>
      r(i0, i1) = f(v)
    }
    r

  inline def mapWithIndex[U: ClassTag](f: (T, Int, Int) => U) =
    val r = ArrayView2d[U](shape0, shape1)
    foreachWithIndex { (v, i0, i1) =>
      r(i0, i1) = f(v, i0, i1)
    }
    r

  override def :=(other: ArrayView2d[T]): Unit =
    require(hasSameSize(other))
    foreachIndex { (i0, i1) =>
      this (i0, i1) = other(i0, i1)
    }

  def fill(value: T): Unit =
    foreachIndex { (i0, i1) =>
      this (i0, i1) = value
    }

  def fill(f: (i0: Int, i1: Int) => T): Unit =
    foreachIndex { (i0, i1) =>
      this (i0, i1) = f(i0, i1)
    }

  def copy(using ClassTag[T]): ArrayView2dFlat[T] =
    val r = ArrayView2dFlat[T](shape0, shape1)
    r := this
    r

  private def checkRange(first: Int, last: Int, max: Int): Unit =
    require(0 <= first && first < max)
    require(0 <= last && last < max)


  def transposed(using ClassTag[T]): ArrayView2dImpl[T] =
    ArrayView2dImpl[T](
      data,
      shape0 = shape1,
      shape1 = shape0,
      offset = offset,
      stride0 = stride1,
      stride1 = stride0
    )

  def diagonal: ArrayView1d[T] =
    require(isSquare)
    ArrayView1dImpl[T](
      data,
      shape0 = shape0,
      offset = offset,
      stride0 = stride0 + stride1
    )

  override def flatten(using ClassTag[T]): ArrayView1dFlat[T] =
    if (hasSimpleFlatLayout) ArrayView1dFlat[T](data)
    else this.copy.flatten

  def broadcastTo(view: ArrayView2d[?]): ArrayView2d[T] =
    broadcast(view.shape0, view.shape1)

  def broadcast(newShape0: Int = shape0, newShape1: Int = shape1): ArrayView2d[T] =
    if (newShape0 == shape0 && newShape1 == shape1) return this

    require(newShape0 == shape0 || shape0 <= 1 || stride0 == 0)
    require(newShape1 == shape1 || shape1 <= 1 || stride1 == 0)

    ArrayView2dImpl[T](
      data,
      shape0 = newShape0,
      shape1 = newShape1,
      offset = offset,
      stride0 = if (shape0 <= 1) 0 else stride0,
      stride1 = if (shape1 <= 1) 0 else stride1,
    )


  transparent inline def view
  [T1 <: Int | Range,
    T2 <: Int | Range](inline range0: AxisSize.Size ?=> T1,
                       inline range1: AxisSize.Size ?=> T2) = {
    val t0 = AxisSize.withAxisSize(shape0, range0)
    val t1 = AxisSize.withAxisSize(shape1, range1)

    val start0 = ArrayViewUtil.getFirst(t0, shape0)
    val start1 = ArrayViewUtil.getFirst(t1, shape1)

    val offset = getPos(start0, start1)

    inline (t0, t1) match {
      case (a: Int, b: Int) => ArrayView0dImpl(data, offset = offset)
      case (a: Range, b: Int) => ArrayView1dImpl(data, shape0 = a.size, offset = offset, stride0 = stride0 * a.step)
      case (a: Int, b: Range) => ArrayView1dImpl(data, shape0 = b.size, offset = offset, stride0 = stride1 * b.step)
      case (a: Range, b: Range) => ArrayView2dImpl(data, shape0 = a.size, shape1 = b.size, offset = offset, stride0 = stride0 * a.step, stride1 = stride1 * b.step)
    }
  }


object ArrayView2d:
  inline def apply[T: ClassTag](shape0: Int, shape1: Int): ArrayView2dFlat[T] =
    ArrayView2dFlat(shape0, shape1)

  inline def apply[T](data: Array[T], shape0: Int, shape1: Int): ArrayView2dFlat[T] =
    ArrayView2dFlat(data, shape0, shape1)

  def apply[T](data: Array[T], shape0: Int, shape1: Int, offset: Int, stride0: Int, stride1: Int): ArrayView2d[T] =
    if (data.length == shape0 * shape1 && offset == 0 && stride0 == shape1 && stride1 == 1) ArrayView2dFlat(data, shape0, shape1)
    else ArrayView2dImpl(data, shape0, shape1, offset, stride0, stride1)
