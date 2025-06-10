package com.github.kright.arrayview

import scala.reflect.ClassTag
import ArrayViewUtil.loop

trait ArrayView1d[T] extends ArrayViewNd[T, ArrayView1d[T]]:
  
  def shape0: Int
  def offset: Int
  def stride0: Int

  def getPos(i0: Int): Int =
    offset + stride0 * i0

  inline def apply(i0: Int): T =
    data(getPos(i0))

  inline def update(i0: Int, value: T): Unit =
    data(getPos(i0)) = value

  def containsIndex(i0: Int): Boolean =
    0 <= i0 && i0 < shape0

  def hasSameSize(other: ArrayView1d[?]): Boolean =
    shape0 == other.shape0

  inline def foreachIndex(f: Int => Unit): Unit = {
    loop(shape0) { i0 =>
      f(i0)
    }
  }

  inline def foreach(f: T => Unit): Unit =
    foreachIndex { i0 =>
      f(this (i0))
    }

  inline def foreachWithIndex(f: (T, Int) => Unit): Unit =
    foreachIndex { i0 =>
      f(this (i0), i0)
    }

  inline def mapInplace(f: T => T): Unit =
    foreachIndex { i0 =>
      val pos = getPos(i0)
      this.data(pos) = f(this.data(pos))
    }

  inline def mapWithIndexInplace(f: (T, Int) => T): Unit =
    foreachIndex { i0 =>
      val pos = getPos(i0)
      this.data(pos) = f(this.data(pos), i0)
    }

  inline def map[U: ClassTag](f: T => U) =
    val r = ArrayView1d[U](shape0)
    foreachWithIndex { (v, i0) =>
      r(i0) = f(v)
    }
    r

  inline def mapWithIndex[U: ClassTag](f: (T, Int) => U) =
    val r = ArrayView1d[U](shape0)
    foreachWithIndex { (v, i0) =>
      r(i0) = f(v, i0)
    }
    r

  override def :=(other: ArrayView1d[T]): Unit =
    require(hasSameSize(other))
    foreachIndex { i0 =>
      this (i0) = other(i0)
    }

  def fill(value: T): Unit =
    foreachIndex { i0 =>
      this (i0) = value
    }

  def fill(f: Int => T): Unit =
    foreachIndex { i0 =>
      this (i0) = f(i0)
    }

  def copy(using ClassTag[T]): ArrayView1dFlat[T] =
    val r = ArrayView1dFlat[T](shape0)
    r := this
    r

  def broadcastTo(view: ArrayView1d[?]): ArrayView1d[T] =
    broadcast(view.shape0)

  def broadcast(newShape0: Int = shape0): ArrayView1d[T] =
    if (newShape0 == shape0) return this

    require(shape0 <= 1 || stride0 == 0)

    ArrayView1dImpl[T](data, shape0 = newShape0, offset = offset, stride0 = 0)

  override def flatten(using ClassTag[T]): ArrayView1dFlat[T] =
    if (hasSimpleFlatLayout) ArrayView1dFlat(data)
    else this.copy

  override def reshape(shape0: Int, shape1: Int)(using ClassTag[T]): ArrayView2d[T] =
    if (hasSimpleFlatLayout) {
      ArrayView2d(data, shape0, shape1)
    } else {
      require(this.shape0 == shape0 * shape1)
      ArrayView2dImpl(
        data,
        shape0 = shape0,
        shape1 = shape1,
        offset = offset,
        stride0 = stride0 * shape1,
        stride1 = stride0,
      )
    }

  override def reshape(shape0: Int, shape1: Int, shape2: Int)(using ClassTag[T]): ArrayView3d[T] =
    if (hasSimpleFlatLayout) {
      ArrayView3d(data, shape0, shape1, shape2)
    } else {
      require(this.shape0 == shape0 * shape1 * shape2)
      ArrayView3dImpl(
        data,
        shape0 = shape0,
        shape1 = shape1,
        shape2 = shape2,
        offset = offset,
        stride0 = stride0 * shape1 * shape2,
        stride1 = stride0 * shape2,
        stride2 = stride0,
      )
    }

  override def reshape(shape0: Int, shape1: Int, shape2: Int, shape3: Int)(using ClassTag[T]): ArrayView4d[T] =
    if (hasSimpleFlatLayout) {
      ArrayView4d(data, shape0, shape1, shape2, shape3)
    } else {
      require(this.shape0 == shape0 * shape1 * shape2 * shape3)
      ArrayView4dImpl(
        data,
        shape0 = shape0,
        shape1 = shape1,
        shape2 = shape2,
        shape3 = shape3,
        offset = offset,
        stride0 = stride0 * shape1 * shape2 * shape3,
        stride1 = stride0 * shape2 * shape3,
        stride2 = stride0 * shape3,
        stride3 = stride0,
      )
    }

  transparent inline def view[T1 <: Int | Range](inline range0: AxisSize.Size ?=> T1) = {
    val t0 = AxisSize.withAxisSize(shape0, range0)

    val start0 = ArrayViewUtil.getFirst(t0, shape0)

    val offset = getPos(start0)

    inline (t0) match {
      case a: Int => ArrayView0dImpl(data, offset = offset)
      case a: Range => ArrayView1dImpl(data, shape0 = a.size, offset = offset, stride0 = stride0 * a.step)
    }
  }

object ArrayView1d:
  inline def apply[T: ClassTag](shape0: Int): ArrayView1dFlat[T] =
    ArrayView1dFlat(shape0)

  inline def apply[T](data: Array[T]): ArrayView1dFlat[T] =
    ArrayView1dFlat[T](data)

  def apply[T](data: Array[T], shape0: Int, offset: Int, stride0: Int): ArrayView1d[T] =
    if (data.length == shape0 && offset == 0 && stride0 == 1) ArrayView1dFlat(data)
    else ArrayView1dImpl(data, shape0, offset, stride0)
