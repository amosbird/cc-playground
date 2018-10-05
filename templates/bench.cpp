#include <benchmark/benchmark.h>
#define NOMAIN
#include "snippet.cpp"
#undef NOMAIN

class MyFixture : public benchmark::Fixture {};

BENCHMARK_F(MyFixture, FooTest)(benchmark::State& state) {
   for (auto _ : state) {
  }
}

static void BM_StringCreation(benchmark::State& state) {
    for (auto _ : state) std::string empty_string;
}
// Register the function as a benchmark
BENCHMARK(BM_StringCreation);

// Define another benchmark
static void BM_StringCopy(benchmark::State& state) {
    std::string x = "hello";
    for (auto _ : state) std::string copy(x);
}
BENCHMARK(BM_StringCopy);

BENCHMARK_MAIN();
