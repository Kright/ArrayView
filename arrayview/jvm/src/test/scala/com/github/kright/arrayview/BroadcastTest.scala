package com.github.kright.arrayview

import org.scalatest.funsuite.AnyFunSuite

class BroadcastTest extends AnyFunSuite:
  
  test("ArrayView1d := with broadcasting") {
    // Test broadcasting a scalar (1-element array) to a larger array
    val source = ArrayView1d[Int](Array(5))
    val target = ArrayView1d[Int](4)
    
    target := source
    
    // All elements should be set to 5
    assert(target.data sameElements Array(5, 5, 5, 5))
    
    // Test broadcasting with stride 0
    val sourceWithStride0 = ArrayView1dImpl[Int](Array(7), shape0 = 1, offset = 0, stride0 = 0)
    target := sourceWithStride0
    
    // All elements should be set to 7
    assert(target.data sameElements Array(7, 7, 7, 7))
    
    // Test that broadcasting fails when source shape > 1 and stride != 0
    val invalidSource = ArrayView1d[Int](Array(1, 2))
    assertThrows[IllegalArgumentException] {
      target := invalidSource
    }
  }
  
  test("ArrayView2d := with broadcasting") {
    // Test broadcasting a scalar (1x1 array) to a larger array
    val source = ArrayView2d[Int](1, 1)
    source(0, 0) = 5
    val target = ArrayView2d[Int](3, 2)
    
    target := source
    
    // All elements should be set to 5
    for (i <- 0 until 3; j <- 0 until 2) {
      assert(target(i, j) == 5, s"Element at ($i, $j) should be 5")
    }
    
    // Test broadcasting along first dimension (1 x n) -> (m x n)
    val source1xN = ArrayView2d[Int](1, 2)
    source1xN(0, 0) = 1
    source1xN(0, 1) = 2
    
    target := source1xN
    
    // Each row should be [1, 2]
    for (i <- 0 until 3) {
      assert(target(i, 0) == 1, s"Element at ($i, 0) should be 1")
      assert(target(i, 1) == 2, s"Element at ($i, 1) should be 2")
    }
    
    // Test broadcasting along second dimension (m x 1) -> (m x n)
    val sourceMx1 = ArrayView2d[Int](3, 1)
    sourceMx1(0, 0) = 3
    sourceMx1(1, 0) = 4
    sourceMx1(2, 0) = 5
    
    target := sourceMx1
    
    // Each column should be [3, 4, 5]
    for (j <- 0 until 2) {
      assert(target(0, j) == 3, s"Element at (0, $j) should be 3")
      assert(target(1, j) == 4, s"Element at (1, $j) should be 4")
      assert(target(2, j) == 5, s"Element at (2, $j) should be 5")
    }
    
    // Test that broadcasting fails when dimensions don't match and can't be broadcast
    val invalidSource = ArrayView2d[Int](2, 3)
    assertThrows[IllegalArgumentException] {
      target := invalidSource
    }
  }
  
  test("ArrayView3d := with broadcasting") {
    // Test broadcasting a scalar (1x1x1 array) to a larger array
    val source = ArrayView3d[Int](1, 1, 1)
    source(0, 0, 0) = 5
    val target = ArrayView3d[Int](2, 3, 2)
    
    target := source
    
    // All elements should be set to 5
    for (i <- 0 until 2; j <- 0 until 3; k <- 0 until 2) {
      assert(target(i, j, k) == 5, s"Element at ($i, $j, $k) should be 5")
    }
    
    // Test broadcasting along multiple dimensions (1 x n x 1) -> (m x n x p)
    val source1xNx1 = ArrayView3d[Int](1, 3, 1)
    source1xNx1(0, 0, 0) = 1
    source1xNx1(0, 1, 0) = 2
    source1xNx1(0, 2, 0) = 3
    
    target := source1xNx1
    
    // Each "slice" along the middle dimension should be constant
    for (i <- 0 until 2; j <- 0 until 3; k <- 0 until 2) {
      val expected = j + 1
      assert(target(i, j, k) == expected, s"Element at ($i, $j, $k) should be $expected")
    }
    
    // Test that broadcasting fails when dimensions don't match and can't be broadcast
    val invalidSource = ArrayView3d[Int](2, 2, 2)
    assertThrows[IllegalArgumentException] {
      target := invalidSource
    }
  }
  
  test("ArrayView4d := with broadcasting") {
    // Test broadcasting a scalar (1x1x1x1 array) to a larger array
    val source = ArrayView4d[Int](1, 1, 1, 1)
    source(0, 0, 0, 0) = 5
    val target = ArrayView4d[Int](2, 2, 2, 2)
    
    target := source
    
    // All elements should be set to 5
    for (i <- 0 until 2; j <- 0 until 2; k <- 0 until 2; l <- 0 until 2) {
      assert(target(i, j, k, l) == 5, s"Element at ($i, $j, $k, $l) should be 5")
    }
    
    // Test broadcasting along multiple dimensions (1 x n x 1 x m) -> (a x n x b x m)
    val source1xNx1xM = ArrayView4d[Int](1, 2, 1, 2)
    source1xNx1xM(0, 0, 0, 0) = 1
    source1xNx1xM(0, 0, 0, 1) = 2
    source1xNx1xM(0, 1, 0, 0) = 3
    source1xNx1xM(0, 1, 0, 1) = 4
    
    target := source1xNx1xM
    
    // Check the broadcasted values
    for (i <- 0 until 2; j <- 0 until 2; k <- 0 until 2; l <- 0 until 2) {
      val expected = j * 2 + l + 1
      assert(target(i, j, k, l) == expected, s"Element at ($i, $j, $k, $l) should be $expected")
    }
    
    // Test that broadcasting fails when dimensions don't match and can't be broadcast
    val invalidSource = ArrayView4d[Int](2, 1, 2, 1)
    assertThrows[IllegalArgumentException] {
      target := invalidSource
    }
  }