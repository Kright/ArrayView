# ArrayView: Scala library for multidimensional tensors, packed into flat arrays

Simple and efficient library for multidimensional tensors.
Pure Scala implementation, no any external dependencies or binaries.

Code is under MIT License, feel free to use it or to suggest improvements.

## Features:

* Easy axis manipulations: changing axis order, padding, strides, reshaping, broadcasting. Numpy-inspired syntax for view, for example
  `arrayView3d.view(0, 1 to size - 1 by 2, all.reverse)` will return 2d view reusing original data.
* Memory and performance efficient array view up to 4-dimensional case.
* Underlying data is copied only when necessary, several views may reuse the same data array.
* Primitive types are stored as efficient arrays of primitive types
* Array views are extensible, it's possible to create concrete implementations, for example `FloatMatrix4x4`

## How to add to a project:

https://jitpack.io/#Kright/ArrayView

gradle:
```
implementation "com.github.Kright:ArrayView:0.1.0"
```

sbt:
```
libraryDependencies += "com.github.Kright" % "ArrayView" % "0.1.0"
```

## Examples

```Scala
import com.github.kright.arrayview._
import com.github.kright.arrayview.AxisSize.{Size, size, all}

val arr = new ArrayView3d[Int](3, 4, 5)

// access to elements
val elem: Double = arr(1, 1, 1)
arr(2, 2, 2) += 1 

// creating views
val first: ArrayView0d[Int] = arr.view(0, 0, 0)
val subArray: ArrayView1d[Int] = arr.view(0, 1 until (size - 1), all.reverse)

// reshaping
val array2d: ArrayView0d[Int] = arr.reshape(3 * 4, 5)

// assigning values
arr.view(2, 3, all) := arr.view(0, 0, all)
subArray.fill(-1)
```

## Why the library is efficient and how boxing of primitives is avoided

```Scala
trait ArrayView1d[T]:
  def data: Array[T]
  def offset: Int
  def stride0: Int
  
  def getIndex(i0: Int): Int = offset + stride0 * i0

  inline def apply(i0: Int): T = data(getPos(i0))

  inline def update(i0: Int, value: T): Unit = data(getPos(i0)) = value
```

For example, for T = Double scala compiler will create an array of doubles. So storage of primitives is efficient.

Methods `apply` and `update` are inline, so in JVM there is no need to do boxing and return object from generic method
`apply` or `update`

Simply speaking, updating value is equivalent to code

```Scala
view.data(view.getPos(i)) = value
```

In addition, Jvm still doesn't have value types.
Because of this, shapes and strides are stored as int fields in view classes to avoid another level of indirection.

## How view syntax is working

```Scala
import AxisSize.{Size, size, all}

ArrayView2d[T]:
  ...
  transparent inline def view[T1 <: Int | Range,
                              T2 <: Int | Range](inline range0: AxisSize.Size ?=> T1,
                                                 inline range1: AxisSize.Size ?=> T2) = ...
```

Each argument has implicit parameter `AxisSize.Size` with size along its axis, so instead of integer numbers it is
possible to pass something like `size - 1`,
`size / 2 until size`, `all`

With the help of transparent inline, this method may return 2d, 1d, or scalar view relative to the number of ranges
passed.

## Code structure

Types for 0d, 1d, 2d, 3d, and 4d cases are different.

For each dimension there are two implementations. For example, for 2d:

1. ArrayView2dFlat: simple view. May have only naive flat layout over the whole array.
2. ArrayView2dImpl: universal view with any shape, stride, and offset.

0d is a degenerative case: it is equivalent to a pointer to an element and has no indexing at all.

If you have some special case, for example, float matrix with size 4x4, you may inherit form ArrayView2d:

```Scala
FloatMatrix4x4() extends ArrayView2d[Float]:
  override val data = new Array[Float](16)
  
  override def getIndex(i0: Int, i1: Int): Int = i0 * 4 + i1
    
  override inline val hasSimpleFlatLayout = true  
  override inline val shape0 = 4
  override inline val shape1 = 4
  override inline val offset = 0
  override inline val stride0 = 4
  override inline val stride1 = 1
```

Note that inline values are not stored as fields, so FloatMatrix4x4 has only `data` as field.
So it is a lightweight class, which has a full set of ArrayView2d methods.
