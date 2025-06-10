package com.github.kright.arrayview

import ArrayViewUtil.loop

import scala.reflect.ClassTag


trait ArrayView4d[T] extends ArrayViewNd[T, ArrayView4d[T]]:
  
  def shape0: Int
  def shape1: Int
  def shape2: Int
  def shape3: Int

  def offset: Int

  def stride0: Int
  def stride1: Int
  def stride2: Int
  def stride3: Int

  def getPos(i0: Int, i1: Int, i2: Int, i3: Int): Int =
    offset + stride0 * i0 + stride1 * i1 + stride2 * i2 + stride3 * i3

  inline def apply(i0: Int, i1: Int, i2: Int, i3: Int): T =
    data(getPos(i0, i1, i2, i3))

  inline def update(i0: Int, i1: Int, i2: Int, i3: Int, value: T): Unit =
    data(getPos(i0, i1, i2, i3)) = value

  def containsIndex(i0: Int, i1: Int, i2: Int, i3: Int): Boolean =
    0 <= i0 && i0 < shape0 &&
      0 <= i1 && i1 < shape1 &&
      0 <= i2 && i2 < shape2 &&
      0 <= i3 && i3 < shape3

  def hasSameSize(other: ArrayView4d[?]): Boolean =
    shape0 == other.shape0 && shape1 == other.shape1 && shape2 == other.shape2 && shape3 == other.shape3

  def isHypercube: Boolean =
    shape0 == shape1 && shape1 == shape2 && shape2 == shape3


  inline def foreachIndex(f: (Int, Int, Int, Int) => Unit): Unit = {
    loop(shape0) { i0 =>
      loop(shape1) { i1 =>
        loop(shape2) { i2 =>
          loop(shape3) { i3 =>
            f(i0, i1, i2, i3)
          }
        }
      }
    }
  }

  inline def foreach(f: T => Unit): Unit =
    foreachIndex { (i0, i1, i2, i3) =>
      f(this (i0, i1, i2, i3))
    }

  inline def foreachWithIndex(f: (T, Int, Int, Int, Int) => Unit): Unit =
    foreachIndex { (i0, i1, i2, i3) =>
      f(this (i0, i1, i2, i3), i0, i1, i2, i3)
    }


  inline def mapInplace(f: T => T): Unit =
    foreachIndex { (i0, i1, i2, i3) =>
      val pos = getPos(i0, i1, i2, i3)
      this.data(pos) = f(this.data(pos))
    }

  inline def mapWithIndexInplace(f: (T, Int, Int, Int, Int) => T): Unit =
    foreachIndex { (i0, i1, i2, i3) =>
      val pos = getPos(i0, i1, i2, i3)
      this.data(pos) = f(this.data(pos), i0, i1, i2, i3)
    }

  inline def map[U: ClassTag](f: T => U) =
    val r = ArrayView4d[U](shape0, shape1, shape2, shape3)
    foreachWithIndex { (v, i0, i1, i2, i3) =>
      r(i0, i1, i2, i3) = f(v)
    }
    r

  inline def mapWithIndex[U: ClassTag](f: (T, Int, Int, Int, Int) => U) =
    val r = ArrayView4d[U](shape0, shape1, shape2, shape3)
    foreachWithIndex { (v, i0, i1, i2, i3) =>
      r(i0, i1, i2, i3) = f(v, i0, i1, i2, i3)
    }
    r

  override def :=(other: ArrayView4d[T]): Unit =
    require(hasSameSize(other))
    foreachIndex { (i0, i1, i2, i3) =>
      this (i0, i1, i2, i3) = other(i0, i1, i2, i3)
    }

  def fill(value: T): Unit =
    foreachIndex { (i0, i1, i2, i3) =>
      this (i0, i1, i2, i3) = value
    }

  def fill(f: (i0: Int, i1: Int, i2: Int, i3: Int) => T): Unit =
    foreachIndex { (i0, i1, i2, i3) =>
      this (i0, i1, i2, i3) = f(i0, i1, i2, i3)
    }

  def copy(using ClassTag[T]): ArrayView4dFlat[T] =
    val r = ArrayView4dFlat[T](shape0, shape1, shape2, shape3)
    r := this
    r

  transparent inline def view
  [T1 <: Int | Range,
    T2 <: Int | Range,
    T3 <: Int | Range,
    T4 <: Int | Range](inline range0: AxisSize.Size ?=> T1,
                       inline range1: AxisSize.Size ?=> T2,
                       inline range2: AxisSize.Size ?=> T3,
                       inline range3: AxisSize.Size ?=> T4) = {

    val t0 = AxisSize.withAxisSize(shape0, range0)
    val t1 = AxisSize.withAxisSize(shape1, range1)
    val t2 = AxisSize.withAxisSize(shape2, range2)
    val t3 = AxisSize.withAxisSize(shape3, range3)

    val start0 = ArrayViewUtil.getFirst(t0, shape0)
    val start1 = ArrayViewUtil.getFirst(t1, shape1)
    val start2 = ArrayViewUtil.getFirst(t2, shape2)
    val start3 = ArrayViewUtil.getFirst(t3, shape3)

    val offset = getPos(start0, start1, start2, start3)

    inline (t0, t1, t2, t3) match {
      case (a: Int, b: Int, c: Int, d: Int) => ArrayView0dImpl(data, offset = offset)
      case (a: Range, b: Int, c: Int, d: Int) => ArrayView1dImpl(data, shape0 = a.size, offset = offset, stride0 = stride0 * a.step)
      case (a: Int, b: Range, c: Int, d: Int) => ArrayView1dImpl(data, shape0 = b.size, offset = offset, stride0 = stride1 * b.step)
      case (a: Int, b: Int, c: Range, d: Int) => ArrayView1dImpl(data, shape0 = c.size, offset = offset, stride0 = stride2 * c.step)
      case (a: Int, b: Int, c: Int, d: Range) => ArrayView1dImpl(data, shape0 = d.size, offset = offset, stride0 = stride3 * d.step)
      case (a: Range, b: Range, c: Int, d: Int) => ArrayView2dImpl(data, shape0 = a.size, shape1 = b.size, offset = offset, stride0 = stride0 * a.step, stride1 = stride1 * b.step)
      case (a: Range, b: Int, c: Range, d: Int) => ArrayView2dImpl(data, shape0 = a.size, shape1 = c.size, offset = offset, stride0 = stride0 * a.step, stride1 = stride2 * c.step)
      case (a: Range, b: Int, c: Int, d: Range) => ArrayView2dImpl(data, shape0 = a.size, shape1 = d.size, offset = offset, stride0 = stride0 * a.step, stride1 = stride3 * d.step)
      case (a: Int, b: Range, c: Range, d: Int) => ArrayView2dImpl(data, shape0 = b.size, shape1 = c.size, offset = offset, stride0 = stride1 * b.step, stride1 = stride2 * c.step)
      case (a: Int, b: Range, c: Int, d: Range) => ArrayView2dImpl(data, shape0 = b.size, shape1 = d.size, offset = offset, stride0 = stride1 * b.step, stride1 = stride3 * d.step)
      case (a: Int, b: Int, c: Range, d: Range) => ArrayView2dImpl(data, shape0 = c.size, shape1 = d.size, offset = offset, stride0 = stride2 * c.step, stride1 = stride3 * d.step)
      case (a: Range, b: Range, c: Range, d: Int) => ArrayView3dImpl(data, shape0 = a.size, shape1 = b.size, shape2 = c.size, offset = offset, stride0 = stride0 * a.step, stride1 = stride1 * b.step, stride2 = stride2 * c.step)
      case (a: Range, b: Range, c: Int, d: Range) => ArrayView3dImpl(data, shape0 = a.size, shape1 = b.size, shape2 = d.size, offset = offset, stride0 = stride0 * a.step, stride1 = stride1 * b.step, stride2 = stride3 * d.step)
      case (a: Range, b: Int, c: Range, d: Range) => ArrayView3dImpl(data, shape0 = a.size, shape1 = c.size, shape2 = d.size, offset = offset, stride0 = stride0 * a.step, stride1 = stride2 * c.step, stride2 = stride3 * d.step)
      case (a: Int, b: Range, c: Range, d: Range) => ArrayView3dImpl(data, shape0 = b.size, shape1 = c.size, shape2 = d.size, offset = offset, stride0 = stride1 * b.step, stride1 = stride2 * c.step, stride2 = stride3 * d.step)
      case (a: Range, b: Range, c: Range, d: Range) => ArrayView4dImpl(data, shape0 = a.size, shape1 = b.size, shape2 = c.size, shape3 = d.size, offset = offset, stride0 = stride0 * a.step, stride1 = stride1 * b.step, stride2 = stride2 * c.step, stride3 = stride3 * d.step)
    }
  }

  def permute(dim0: Int, dim1: Int, dim2: Int, dim3: Int): ArrayView4dImpl[T] =
    require(Set(dim0, dim1, dim2, dim3) == Set(0, 1, 2, 3), "Permutation must contain exactly the values 0, 1, 2, 3")

    val newShapes = Array(shape0, shape1, shape2, shape3)
    val newStrides = Array(stride0, stride1, stride2, stride3)

    ArrayView4dImpl[T](
      data,
      shape0 = newShapes(dim0),
      shape1 = newShapes(dim1),
      shape2 = newShapes(dim2),
      shape3 = newShapes(dim3),
      offset = offset,
      stride0 = newStrides(dim0),
      stride1 = newStrides(dim1),
      stride2 = newStrides(dim2),
      stride3 = newStrides(dim3)
    )

  def rotateLeft: ArrayView4dImpl[T] = permute(1, 2, 3, 0)

  def rotateRight: ArrayView4dImpl[T] = permute(3, 0, 1, 2)

  def swapFirstTwo: ArrayView4dImpl[T] = permute(1, 0, 2, 3)

  def swapLastTwo: ArrayView4dImpl[T] = permute(0, 1, 3, 2)

  def broadcastTo(view: ArrayView4d[?]): ArrayView4d[T] =
    broadcast(view.shape0, view.shape1, view.shape2, view.shape3)

  def broadcast(newShape0: Int = shape0, newShape1: Int = shape1, newShape2: Int = shape2, newShape3: Int = shape3): ArrayView4d[T] =
    if (newShape0 == shape0 && newShape1 == shape1 && newShape2 == shape2 && newShape3 == shape3) return this

    require(newShape0 == shape0 || shape0 <= 1 || stride0 == 0)
    require(newShape1 == shape1 || shape1 <= 1 || stride1 == 0)
    require(newShape2 == shape2 || shape2 <= 1 || stride2 == 0)
    require(newShape3 == shape3 || shape3 <= 1 || stride3 == 0)

    ArrayView4dImpl[T](
      data,
      shape0 = newShape0,
      shape1 = newShape1,
      shape2 = newShape2,
      shape3 = newShape3,
      offset = offset,
      stride0 = if (shape0 <= 1) 0 else stride0,
      stride1 = if (shape1 <= 1) 0 else stride1,
      stride2 = if (shape2 <= 1) 0 else stride2,
      stride3 = if (shape3 <= 1) 0 else stride3,
    )

  def flatten(using ClassTag[T]): ArrayView1dFlat[T] =
    if (hasSimpleFlatLayout) ArrayView1dFlat[T](data)
    else this.copy.flatten


object ArrayView4d:
  inline def apply[T: ClassTag](shape0: Int, shape1: Int, shape2: Int, shape3: Int): ArrayView4dFlat[T] =
    ArrayView4dFlat(shape0, shape1, shape2, shape3)

  inline def apply[T](data: Array[T], shape0: Int, shape1: Int, shape2: Int, shape3: Int): ArrayView4dFlat[T] =
    ArrayView4dFlat(data, shape0, shape1, shape2, shape3)

  def apply[T](data: Array[T], shape0: Int, shape1: Int, shape2: Int, shape3: Int, offset: Int, stride0: Int, stride1: Int, stride2: Int, stride3: Int): ArrayView4d[T] =
    if (data.length == shape0 * shape1 * shape2 * shape3 && offset == 0 && stride0 == shape1 * shape2 * shape3 && stride1 == shape2 * shape3 && stride2 == shape3 && stride3 == 1) ArrayView4dFlat(data, shape0, shape1, shape2, shape3)
    else ArrayView4dImpl(data, shape0, shape1, shape2, shape3, offset, stride0, stride1, stride2, stride3)
