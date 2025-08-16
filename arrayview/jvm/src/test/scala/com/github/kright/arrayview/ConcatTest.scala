package com.github.kright.arrayview

import org.scalatest.funsuite.AnyFunSuite

class ConcatTest extends AnyFunSuite:
  test("ArrayView3d concatenation along axis 0") {
    // Create 3D arrays to concatenate along axis 0
    val view1 = ArrayView3d[Int](2, 2, 2)
    view1.fill((i, j, k) => i * 4 + j * 2 + k) // [[[0, 1], [2, 3]], [[4, 5], [6, 7]]]
    
    val view2 = ArrayView3d[Int](1, 2, 2)
    view2.fill((_, j, k) => 8 + j * 2 + k) // [[[8, 9], [10, 11]]]
    
    val view3 = ArrayView3d[Int](2, 2, 2)
    view3.fill((i, j, k) => 12 + i * 4 + j * 2 + k) // [[[12, 13], [14, 15]], [[16, 17], [18, 19]]]
    
    // Concatenate along axis 0
    val concatenated = ArrayView3d.concat(Seq(view1, view2, view3), axis = 0)
    
    // Check shape
    assert(concatenated.shape0 == 5)
    assert(concatenated.shape1 == 2)
    assert(concatenated.shape2 == 2)
    
    // Check values
    val expected = Array(
      // view1
      Array(Array(0, 1), Array(2, 3)),
      Array(Array(4, 5), Array(6, 7)),
      // view2
      Array(Array(8, 9), Array(10, 11)),
      // view3
      Array(Array(12, 13), Array(14, 15)),
      Array(Array(16, 17), Array(18, 19))
    )
    
    for (i <- 0 until 5; j <- 0 until 2; k <- 0 until 2) {
      assert(concatenated(i, j, k) == expected(i)(j)(k), 
             s"Value at ($i, $j, $k) should be ${expected(i)(j)(k)} but was ${concatenated(i, j, k)}")
    }
  }
  
  test("ArrayView3d concatenation along axis 1") {
    // Create 3D arrays to concatenate along axis 1
    val view1 = ArrayView3d[Int](2, 1, 2)
    view1.fill((i, _, k) => i * 2 + k) // [[[0, 1]], [[2, 3]]]
    
    val view2 = ArrayView3d[Int](2, 2, 2)
    view2.fill((i, j, k) => 4 + i * 4 + j * 2 + k) // [[[4, 5], [6, 7]], [[8, 9], [10, 11]]]
    
    val view3 = ArrayView3d[Int](2, 1, 2)
    view3.fill((i, _, k) => 12 + i * 2 + k) // [[[12, 13]], [[14, 15]]]
    
    // Concatenate along axis 1
    val concatenated = ArrayView3d.concat(Seq(view1, view2, view3), axis = 1)
    
    // Check shape
    assert(concatenated.shape0 == 2)
    assert(concatenated.shape1 == 4)
    assert(concatenated.shape2 == 2)
    
    // Check values
    val expected = Array(
      Array(Array(0, 1), Array(4, 5), Array(6, 7), Array(12, 13)),
      Array(Array(2, 3), Array(8, 9), Array(10, 11), Array(14, 15))
    )

    for (i <- 0 until 2; j <- 0 until 4; k <- 0 until 2) {
      assert(concatenated(i, j, k) == expected(i)(j)(k),
             s"Value at ($i, $j, $k) should be ${expected(i)(j)(k)} but was ${concatenated(i, j, k)}")
    }
  }
  
  test("ArrayView3d concatenation along axis 2") {
    // Create 3D arrays to concatenate along axis 2
    val view1 = ArrayView3d[Int](2, 2, 1)
    view1.fill((i, j, _) => i * 2 + j) // [[[0], [1]], [[2], [3]]]
    
    val view2 = ArrayView3d[Int](2, 2, 2)
    view2.fill((i, j, k) => 4 + i * 4 + j * 2 + k) // [[[4, 5], [6, 7]], [[8, 9], [10, 11]]]
    
    val view3 = ArrayView3d[Int](2, 2, 1)
    view3.fill((i, j, _) => 12 + i * 2 + j) // [[[12], [13]], [[14], [15]]]
    
    // Concatenate along axis 2
    val concatenated = ArrayView3d.concat(Seq(view1, view2, view3), axis = 2)
    
    // Check shape
    assert(concatenated.shape0 == 2)
    assert(concatenated.shape1 == 2)
    assert(concatenated.shape2 == 4)
    
    // Check values
    val expected = Array(
      Array(Array(0, 4, 5, 12), Array(1, 6, 7, 13)),
      Array(Array(2, 8, 9, 14), Array(3, 10, 11, 15))
    )
    
    for (i <- 0 until 2; j <- 0 until 2; k <- 0 until 4) {
      assert(concatenated(i, j, k) == expected(i)(j)(k), 
             s"Value at ($i, $j, $k) should be ${expected(i)(j)(k)} but was ${concatenated(i, j, k)}")
    }
  }
  
  test("ArrayView3d concatenation with invalid shapes") {
    val view1 = ArrayView3d[Int](2, 2, 2)
    val view2 = ArrayView3d[Int](2, 3, 2) // Different shape1
    
    // Should throw exception when concatenating along axis 0 with different shape1 or shape2
    assertThrows[IllegalArgumentException] {
      ArrayView3d.concat(Seq(view1, view2), axis = 0)
    }
    
    val view3 = ArrayView3d[Int](2, 2, 2)
    val view4 = ArrayView3d[Int](3, 2, 2) // Different shape0
    
    // Should throw exception when concatenating along axis 1 with different shape0 or shape2
    assertThrows[IllegalArgumentException] {
      ArrayView3d.concat(Seq(view3, view4), axis = 1)
    }
    
    val view5 = ArrayView3d[Int](2, 2, 2)
    val view6 = ArrayView3d[Int](2, 2, 3) // Different shape2
    
    // Should throw exception when concatenating along axis 2 with different shape0 or shape1
    assertThrows[IllegalArgumentException] {
      ArrayView3d.concat(Seq(view5, view6), axis = 0)
    }
    
    // Should throw exception for invalid axis
    assertThrows[IllegalArgumentException] {
      ArrayView3d.concat(Seq(view1, view1), axis = 3)
    }
    
    // Should throw exception for empty sequence
    assertThrows[IllegalArgumentException] {
      ArrayView3d.concat(Seq.empty[ArrayView3d[Int]], axis = 0)
    }
  }
  
  test("ArrayView1d concatenation") {
    val view1 = ArrayView1d[Double](Array(0.0, 1.0))
    val view2 = ArrayView1d[Double](Array(2.0, 3.0, 4.0))
    val view3 = ArrayView1d[Double](Array[Double]())
    val view4 = ArrayView1d[Double](Array(5.0))

    val concatenated = ArrayView1d.concat(Array(view1, view2, view3, view4))

    assert(concatenated.shape0 == 6)
    assert(concatenated.data sameElements (0 to 5).map(_.toDouble).toArray, s"concatenated data = ${concatenated.data.mkString(", ")}")
  }

  test("ArrayView1d concatenation of empty") {
    val concatEmpty0 = ArrayView1d.concat(Array[ArrayView1d[Double]]())
    assert(concatEmpty0.shape0 == 0)

    val concatEmpty1 = ArrayView1d.concat(
      Array(
        ArrayView1d[Double](shape0 = 0),
        ArrayView1d[Double](shape0 = 0)
      )
    )
    assert(concatEmpty1.shape0 == 0)
  }

  test("ArrayView2d broadcasting during concatenation") {
    val view0 = ArrayView2d[Int](1, 3)
    val view1 = ArrayView2d[Int](1, 1)

    view0.fill(1)
    view1.fill(2)

    val result = ArrayView2d.concat(Seq(view0, view1), axis = 0)

    assert(result.shape0 == 2)
    assert(result.shape1 == 3)

    assert(result.data sameElements Array(1, 1, 1, 2, 2, 2))
  }

  test("ArrayView2d concatenation along axis 0") {
    // Create 2D arrays to concatenate along axis 0 (rows)
    val view1 = ArrayView2d[Int](2, 3)
    view1.fill((i, j) => i * 3 + j) // [[0, 1, 2], [3, 4, 5]]

    val view2 = ArrayView2d[Int](1, 3)
    view2.fill((_, j) => 6 + j) // [[6, 7, 8]]

    val view3 = ArrayView2d[Int](3, 3)
    view3.fill((i, j) => 9 + i * 3 + j) // [[9, 10, 11], [12, 13, 14], [15, 16, 17]]

    // Concatenate along axis 0 (default)
    val concatenated = ArrayView2d.concat(Seq(view1, view2, view3), axis = 0)

    // Check shape
    assert(concatenated.shape0 == 6)
    assert(concatenated.shape1 == 3)

    // Check values
    for (i <- 0 until 6; j <- 0 until 3) {
      val expected = if (i < 2) i * 3 + j
      else if (i == 2) 6 + j
      else 9 + (i - 3) * 3 + j
      assert(concatenated(i, j) == expected, s"Value at ($i, $j) should be $expected but was ${concatenated(i, j)}")
    }
  }

  test("ArrayView2d concatenation along axis 1") {
    // Create 2D arrays to concatenate along axis 1 (columns)
    val view1 = ArrayView2d[Int](2, 2)
    view1.fill((i, j) => i * 2 + j) // [[0, 1], [2, 3]]

    val view2 = ArrayView2d[Int](2, 3)
    view2.fill((i, j) => 4 + i * 3 + j) // [[4, 5, 6], [7, 8, 9]]

    val view3 = ArrayView2d[Int](2, 1)
    view3.fill((i, _) => 10 + i) // [[10], [11]]

    // Concatenate along axis 1
    val concatenated = ArrayView2d.concat(Seq(view1, view2, view3), axis = 1)

    // Check shape
    assert(concatenated.shape0 == 2)
    assert(concatenated.shape1 == 6)

    // Check values
    val expected = Array(
      Array(0, 1, 4, 5, 6, 10),
      Array(2, 3, 7, 8, 9, 11)
    )

    for (i <- 0 until 2; j <- 0 until 6) {
      assert(concatenated(i, j) == expected(i)(j), s"Value at ($i, $j) should be ${expected(i)(j)} but was ${concatenated(i, j)}")
    }
  }

  test("ArrayView2d concatenation with invalid shapes") {
    val view1 = ArrayView2d[Int](2, 3)
    val view2 = ArrayView2d[Int](2, 4) // Different shape1

    // Should throw exception when concatenating along axis 0 with different shape1
    assertThrows[IllegalArgumentException] {
      ArrayView2d.concat(Seq(view1, view2), axis = 0)
    }

    val view3 = ArrayView2d[Int](3, 3)
    val view4 = ArrayView2d[Int](4, 3) // Different shape0

    // Should throw exception when concatenating along axis 1 with different shape0
    assertThrows[IllegalArgumentException] {
      ArrayView2d.concat(Seq(view3, view4), axis = 1)
    }

    // Should throw exception for invalid axis
    assertThrows[IllegalArgumentException] {
      ArrayView2d.concat(Seq(view1, view1), axis = 2)
    }

    // Should throw exception for empty sequence
    assertThrows[IllegalArgumentException] {
      ArrayView2d.concat(Seq.empty[ArrayView2d[Int]], axis = 0)
    }
  }
