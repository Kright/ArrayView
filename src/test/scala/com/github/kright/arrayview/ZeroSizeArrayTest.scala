package com.github.kright.arrayview

import org.scalatest.funsuite.AnyFunSuite

class ZeroSizeArrayTest extends AnyFunSuite:

  test("1D array with zero size") {
    val arr = ArrayView1d[Double](0)
    assert(arr.shape0 == 0)
    assert(arr.data.length == 0)
    
    // Test that operations on empty arrays don't throw exceptions
    arr.foreach(_ => fail("foreach should not be called for empty array"))
    arr.foreachIndex(_ => fail("foreachIndex should not be called for empty array"))
    arr.foreachWithIndex((_, _) => fail("foreachWithIndex should not be called for empty array"))
    
    // Test map operations
    val mapped = arr.map(_ + 1.0)
    assert(mapped.shape0 == 0)
    assert(mapped.data.length == 0)
    
    // Test copy operation
    val copied = arr.copy
    assert(copied.shape0 == 0)
    assert(copied.data.length == 0)
  }
  
  test("2D array with zero size in first dimension") {
    val arr = ArrayView2d[Double](0, 5)
    assert(arr.shape0 == 0)
    assert(arr.shape1 == 5)
    assert(arr.data.length == 0)
    
    // Test that operations on empty arrays don't throw exceptions
    arr.foreach(_ => fail("foreach should not be called for empty array"))
    arr.foreachIndex((_, _) => fail("foreachIndex should not be called for empty array"))
    arr.foreachWithIndex((_, _, _) => fail("foreachWithIndex should not be called for empty array"))
    
    // Test map operations
    val mapped = arr.map(_ + 1.0)
    assert(mapped.shape0 == 0)
    assert(mapped.shape1 == 5)
    assert(mapped.data.length == 0)
    
    // Test copy operation
    val copied = arr.copy
    assert(copied.shape0 == 0)
    assert(copied.shape1 == 5)
    assert(copied.data.length == 0)
  }
  
  test("2D array with zero size in second dimension") {
    val arr = ArrayView2d[Double](5, 0)
    assert(arr.shape0 == 5)
    assert(arr.shape1 == 0)
    assert(arr.data.length == 0)
    
    // Test that operations on empty arrays don't throw exceptions
    arr.foreach(_ => fail("foreach should not be called for empty array"))
    arr.foreachIndex((_, _) => fail("foreachIndex should not be called for empty array"))
    arr.foreachWithIndex((_, _, _) => fail("foreachWithIndex should not be called for empty array"))
    
    // Test map operations
    val mapped = arr.map(_ + 1.0)
    assert(mapped.shape0 == 5)
    assert(mapped.shape1 == 0)
    assert(mapped.data.length == 0)
    
    // Test copy operation
    val copied = arr.copy
    assert(copied.shape0 == 5)
    assert(copied.shape1 == 0)
    assert(copied.data.length == 0)
  }
  
  test("3D array with zero size in one dimension") {
    val arr = ArrayView3d[Double](3, 0, 4)
    assert(arr.shape0 == 3)
    assert(arr.shape1 == 0)
    assert(arr.shape2 == 4)
    assert(arr.data.length == 0)
    
    // Test that operations on empty arrays don't throw exceptions
    arr.foreach(_ => fail("foreach should not be called for empty array"))
    arr.foreachIndex((_, _, _) => fail("foreachIndex should not be called for empty array"))
    arr.foreachWithIndex((_, _, _, _) => fail("foreachWithIndex should not be called for empty array"))
    
    // Test map operations
    val mapped = arr.map(_ + 1.0)
    assert(mapped.shape0 == 3)
    assert(mapped.shape1 == 0)
    assert(mapped.shape2 == 4)
    assert(mapped.data.length == 0)
    
    // Test copy operation
    val copied = arr.copy
    assert(copied.shape0 == 3)
    assert(copied.shape1 == 0)
    assert(copied.shape2 == 4)
    assert(copied.data.length == 0)
  }
  
  test("4D array with zero size in one dimension") {
    val arr = ArrayView4d[Double](2, 3, 0, 4)
    assert(arr.shape0 == 2)
    assert(arr.shape1 == 3)
    assert(arr.shape2 == 0)
    assert(arr.shape3 == 4)
    assert(arr.data.length == 0)
    
    // Test that operations on empty arrays don't throw exceptions
    arr.foreach(_ => fail("foreach should not be called for empty array"))
    arr.foreachIndex((_, _, _, _) => fail("foreachIndex should not be called for empty array"))
    arr.foreachWithIndex((_, _, _, _, _) => fail("foreachWithIndex should not be called for empty array"))
    
    // Test map operations
    val mapped = arr.map(_ + 1.0)
    assert(mapped.shape0 == 2)
    assert(mapped.shape1 == 3)
    assert(mapped.shape2 == 0)
    assert(mapped.shape3 == 4)
    assert(mapped.data.length == 0)
    
    // Test copy operation
    val copied = arr.copy
    assert(copied.shape0 == 2)
    assert(copied.shape1 == 3)
    assert(copied.shape2 == 0)
    assert(copied.shape3 == 4)
    assert(copied.data.length == 0)
  }