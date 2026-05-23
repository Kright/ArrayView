# ArrayView Benchmarks

This module contains JMH benchmarks for the ArrayView library.

## Running Benchmarks

To run all benchmarks:

```
sbt "benchmark/Jmh/run"
```

To run specific benchmarks:

```
sbt "benchmark/Jmh/run -i 10 -wi 5 -f1 -t1 .*Matrix4x4MultiplicationBenchmark.*"
```

Where:
- `-i` specifies the number of measurement iterations
- `-wi` specifies the number of warmup iterations
- `-f` specifies the number of forks
- `-t` specifies the number of threads
- `.*ArrayViewBenchmark.*` is a regular expression to select which benchmarks to run

## Available Benchmarks

The `ArrayViewBenchmark` class contains benchmarks for:

1. Basic operations on 1D, 2D, and 3D arrays
2. View operations on 1D and 2D arrays
3. Performance comparison with different array sizes (10, 100, 1000)

## Adding New Benchmarks

To add new benchmarks:

1. Create a new class in the `me.kright.arrayview.benchmarks` package
2. Annotate the class with JMH annotations (`@State`, `@BenchmarkMode`, etc.)
3. Add methods annotated with `@Benchmark`
4. Run the benchmarks using the commands above


## Results:

AMD Ryzen 5950X
```
[info] Benchmark                                                            Mode  Cnt   Score   Error  Units
[info] Matrix4x4MultiplicationBenchmark.multiplicationOfArray               avgt    5  21.059 ± 5.337  ns/op
[info] Matrix4x4MultiplicationBenchmark.multiplicationOfArrayView           avgt    5  29.793 ± 4.261  ns/op
[info] Matrix4x4MultiplicationBenchmark.multiplicationOfArrayView4d         avgt    5  31.048 ± 5.886  ns/op
[info] Matrix4x4MultiplicationBenchmark.multiplicationOfArrayView4x4        avgt    5  26.263 ± 8.377  ns/op
[info] Matrix4x4MultiplicationBenchmark.multiplicationOfArrayView4x4double  avgt    5  22.382 ± 0.447  ns/op
[info] Matrix4x4MultiplicationBenchmark.multiplicationOfArrayViewV2         avgt    5  40.931 ± 1.146  ns/op
[info] Matrix4x4MultiplicationBenchmark.multiplicationOfArrays              avgt    5  41.198 ± 0.409  ns/op
[info] Matrix4x4MultiplicationBenchmark.multiplicationOfMatrix4x4           avgt    5  20.595 ± 0.289  ns/op
```