package com.github.kright.arrayview

import com.github.kright.arrayview.ArrayViewInternalUtil.loop

import scala.reflect.ClassTag
import scala.util.chaining.scalaUtilChainingOps


trait ArrayView3d[T] extends ArrayViewNd[T, ArrayView3d[T]]:
  
  def shape0: Int
  def shape1: Int
  def shape2: Int

  def offset: Int

  def stride0: Int
  def stride1: Int
  def stride2: Int
  
  override def size: Int = shape0 * shape1 * shape2

  override def isEmpty: Boolean =
    shape0 == 0 || shape1 == 0 || shape2 == 0

  def getIndex(i0: Int, i1: Int, i2: Int): Int =
    offset + stride0 * i0 + stride1 * i1 + stride2 * i2

  inline def apply(i0: Int, i1: Int, i2: Int): T =
    data(getIndex(i0, i1, i2))

  inline def update(i0: Int, i1: Int, i2: Int, value: T): Unit =
    data(getIndex(i0, i1, i2)) = value

  def containsIndex(i0: Int, i1: Int, i2: Int): Boolean =
    0 <= i0 && i0 < shape0 &&
      0 <= i1 && i1 < shape1 &&
      0 <= i2 && i2 < shape2

  def hasSameSize(other: ArrayView3d[?]): Boolean =
    shape0 == other.shape0 && shape1 == other.shape1 && shape2 == other.shape2
  
  def isCube: Boolean =
    shape0 == shape1 && shape1 == shape2


  inline def foreachIndex(f: (Int, Int, Int) => Unit): Unit = {
    loop(shape0) { i0 =>
      loop(shape1) { i1 =>
        loop(shape2) { i2 =>
          f(i0, i1, i2)
        }
      }
    }
  }

  inline def foreach(f: T => Unit): Unit =
    foreachIndex { (i0, i1, i2) =>
      f(this (i0, i1, i2))
    }

  inline def foreachWithIndex(f: (T, Int, Int, Int) => Unit): Unit =
    foreachIndex { (i0, i1, i2) =>
      f(this (i0, i1, i2), i0, i1, i2)
    }


  inline def mapInplace(f: T => T): Unit =
    foreachIndex { (i0, i1, i2) =>
      val pos = getIndex(i0, i1, i2)
      this.data(pos) = f(this.data(pos))
    }

  inline def mapWithIndexInplace(f: (T, Int, Int, Int) => T): Unit =
    foreachIndex { (i0, i1, i2) =>
      val pos = getIndex(i0, i1, i2)
      this.data(pos) = f(this.data(pos), i0, i1, i2)
    }

  inline def map[U: ClassTag](f: T => U) =
    val r = ArrayView3d[U](shape0, shape1, shape2)
    foreachWithIndex { (v, i0, i1, i2) =>
      r(i0, i1, i2) = f(v)
    }
    r

  inline def mapWithIndex[U: ClassTag](f: (T, Int, Int, Int) => U) =
    val r = ArrayView3d[U](shape0, shape1, shape2)
    foreachWithIndex { (v, i0, i1, i2) =>
      r(i0, i1, i2) = f(v, i0, i1, i2)
    }
    r

  override def :=(other: ArrayView3d[T]): Unit =
    val broadcastedOther = other.broadcastTo(this)
    foreachIndex { (i0, i1, i2) =>
      this (i0, i1, i2) = broadcastedOther(i0, i1, i2)
    }

  def fill(value: T): Unit =
    foreachIndex { (i0, i1, i2) =>
      this (i0, i1, i2) = value
    }

  inline def fill(f: (i0: Int, i1: Int, i2: Int) => T): Unit =
    foreachIndex { (i0, i1, i2) =>
      this (i0, i1, i2) = f(i0, i1, i2)
    }

  def copy(using ClassTag[T]): ArrayView3dFlat[T] =
    val r = ArrayView3dFlat[T](shape0, shape1, shape2)
    r := this
    r

  override def withSimpleLayout(using ClassTag[T]): ArrayView3d[T] =
    if (hasSimpleFlatLayout) this
    else copy

  transparent inline def view [T1 <: Int | Range,
                               T2 <: Int | Range,
                               T3 <: Int | Range](inline range0: AxisSize.Size ?=> T1,
                                                  inline range1: AxisSize.Size ?=> T2,
                                                  inline range2: AxisSize.Size ?=> T3) = {
    val t0 = AxisSize.withAxisSize(shape0, range0)
    val t1 = AxisSize.withAxisSize(shape1, range1)
    val t2 = AxisSize.withAxisSize(shape2, range2)

    val start0 = ArrayViewInternalUtil.getFirst(t0, shape0)
    val start1 = ArrayViewInternalUtil.getFirst(t1, shape1)
    val start2 = ArrayViewInternalUtil.getFirst(t2, shape2)

    val offset = getIndex(start0, start1, start2)

    inline (t0, t1, t2) match {
      case (_: Int, _: Int, _: Int) => ArrayView0dImpl(data, offset = offset)
      case (a: Range, _: Int, _: Int) => ArrayView1dImpl(data, shape0 = a.size, offset = offset, stride0 = stride0 * a.step)
      case (_: Int, b: Range, _: Int) => ArrayView1dImpl(data, shape0 = b.size, offset = offset, stride0 = stride1 * b.step)
      case (_: Int, _: Int, c: Range) => ArrayView1dImpl(data, shape0 = c.size, offset = offset, stride0 = stride2 * c.step)
      case (a: Range, b: Range, _: Int) => ArrayView2dImpl(data, shape0 = a.size, shape1 = b.size, offset = offset, stride0 = stride0 * a.step, stride1 = stride1 * b.step)
      case (a: Range, _: Int, c: Range) => ArrayView2dImpl(data, shape0 = a.size, shape1 = c.size, offset = offset, stride0 = stride0 * a.step, stride1 = stride2 * c.step)
      case (_: Int, b: Range, c: Range) => ArrayView2dImpl(data, shape0 = b.size, shape1 = c.size, offset = offset, stride0 = stride1 * b.step, stride1 = stride2 * c.step)
      case (a: Range, b: Range, c: Range) => ArrayView3dImpl(data, shape0 = a.size, shape1 = b.size, shape2 = c.size, offset = offset, stride0 = stride0 * a.step, stride1 = stride1 * b.step, stride2 = stride2 * c.step)
    }
  }

  def permute(dim0: Int, dim1: Int, dim2: Int): ArrayView3dImpl[T] =
    require(Set(dim0, dim1, dim2) == Set(0, 1, 2), "Permutation must contain exactly the values 0, 1, 2")

    val newShapes = Array(shape0, shape1, shape2)
    val newStrides = Array(stride0, stride1, stride2)

    ArrayView3dImpl[T](
      data,
      shape0 = newShapes(dim0),
      shape1 = newShapes(dim1),
      shape2 = newShapes(dim2),
      offset = offset,
      stride0 = newStrides(dim0),
      stride1 = newStrides(dim1),
      stride2 = newStrides(dim2)
    )

  def rotateLeft: ArrayView3dImpl[T] = permute(1, 2, 0)

  def rotateRight: ArrayView3dImpl[T] = permute(2, 0, 1)

  def swapFirstTwo: ArrayView3dImpl[T] = permute(1, 0, 2)

  def swapLastTwo: ArrayView3dImpl[T] = permute(0, 2, 1)

  def broadcastTo(view: ArrayView3d[?]): ArrayView3d[T] =
    broadcast(view.shape0, view.shape1, view.shape2)

  def broadcast(newShape0: Int = shape0, newShape1: Int = shape1, newShape2: Int = shape2): ArrayView3d[T] = 
    if (newShape0 == shape0 && newShape1 == shape1 && newShape2 == shape2) return this

    if (isEmpty && newShape0 != 0 && newShape1 != 0 && newShape2 != 0) {
      throw new IllegalArgumentException(s"Cannot broadcast empty view to shape $newShape0 x $newShape1 x $newShape2")
    }
    
    require(newShape0 == shape0 || shape0 <= 1 || stride0 == 0)
    require(newShape1 == shape1 || shape1 <= 1 || stride1 == 0)
    require(newShape2 == shape2 || shape2 <= 1 || stride2 == 0)

    ArrayView3dImpl[T](
      data,
      shape0 = newShape0,
      shape1 = newShape1,
      shape2 = newShape2,
      offset = offset,
      stride0 = if (shape0 <= 1) 0 else stride0,
      stride1 = if (shape1 <= 1) 0 else stride1,
      stride2 = if (shape2 <= 1) 0 else stride2,
    )

  override def flatten(using ClassTag[T]): ArrayView1dFlat[T] =
    if (hasSimpleFlatLayout) ArrayView1dFlat[T](data)
    else this.copy.flatten

object ArrayView3d:
  inline def apply[T: ClassTag](shape0: Int, shape1: Int, shape2: Int): ArrayView3dFlat[T] =
    ArrayView3dFlat(shape0, shape1, shape2)

  inline def apply[T](data: Array[T], shape0: Int, shape1: Int, shape2: Int): ArrayView3dFlat[T] =
    ArrayView3dFlat(data, shape0, shape1, shape2)

  def apply[T](data: Array[T], shape0: Int, shape1: Int, shape2: Int, offset: Int, stride0: Int, stride1: Int, stride2: Int): ArrayView3d[T] =
    if (data.length == shape0 * shape1 * shape2 && offset == 0 && stride0 == shape1 * shape2 && stride1 == shape2 && stride2 == 1) ArrayView3dFlat(data, shape0, shape1, shape2)
    else ArrayView3dImpl(data, shape0, shape1, shape2, offset, stride0, stride1, stride2)
    
  def concat[T: ClassTag](arrayViews: Iterable[ArrayView3d[T]], axis: Int): ArrayView3dFlat[T] =
    if (arrayViews.isEmpty) {
      throw IllegalArgumentException("For empty arrayViews shape could not be defined")
    }

    axis match
      case 0 => 
        apply[T](
          arrayViews.map(_.shape0).sum,
          arrayViews.head.shape1,
          arrayViews.head.shape2,
        ).tap { result =>
          var offset0 = 0
          for (view <- arrayViews) {
            result.view(offset0 until (offset0 + view.shape0), AxisSize.all, AxisSize.all) := view
            offset0 += view.shape0
          }
        }
      case 1 =>
        apply[T](
          arrayViews.head.shape0,
          arrayViews.map(_.shape1).sum,
          arrayViews.head.shape2
        ).tap { result =>
          var offset1 = 0
          for (view <- arrayViews) {
            result.view(AxisSize.all, offset1 until (offset1 + view.shape1), AxisSize.all) := view
            offset1 += view.shape1
          }
        }
      case 2 =>
        apply[T](
          arrayViews.head.shape0,
          arrayViews.head.shape1,
          arrayViews.map(_.shape2).sum
        ).tap { result =>
          var offset2 = 0
          for (view <- arrayViews) {
            result.view(AxisSize.all, AxisSize.all, offset2 until (offset2 + view.shape2)) := view
            offset2 += view.shape2
          }
        }
      case _ =>
        throw IllegalArgumentException(s"Invalid axis: $axis. Must be 0, 1, or 2")
