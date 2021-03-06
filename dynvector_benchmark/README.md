### 1. Dynvector Benchmark

This benchmark is also a beginner's tutorial on how to use *dynvectors*. It will go through some significant features of the package, but make sure to complement it by perusing the documentation accompanying the source code files.

Creating a *dynvector* is straight forward:

~~~
dynvector_create(Id),
~~~

where `Id` is an atom identifying the *dynvector*. It will fail if a *dynvector* with the same id already exists. If necessary, use

~~~
(\+ is_dynvector(Id) ; dynvector_destroy(Id)),
dynvector_create(Id),
~~~

or simply

~~~
dynvector_destroy(Id),
dynvector_create(Id),
~~~

since `dynvector_destroy/1` will not fail or complain when there is nothing to destroy. Note that the size of the *dynvector* (or the maximum number of cells it may contain) is not specified, as it grows and shrinks dynamically as needed.

A *dynvector* may also be created indirectly with `dynvector_list(+Id, +List)`, which causes the content of `List` to be loaded onto the *dynvector*, creating it anew or, if it already exists, replacing its contents.

One possible use of *dynvectors*, and one we will pursue throughout this benchmark, is to replace standard Prolog lists when they are large and there is a need to search their contents. We will create two large lists with randomly generated integers, from an even larger interval. One of them will be our base set, the other our search set. We will iterate over the elements of the search set and try to determine if they are in the base set, counting the hits and misses.

Clearly, if we use 100000 out of 1000000, the expected rate of hits to misses will be around 1:9, varying slightly every time the benchmark runs with these numbers as parameters.

There are many ways to iterate over *dynvectors*, one of them being to use its built-in iterator implementation:

~~~
. . .
% create an iterator over the contents of DynvectorId
dynvector_iterator_create(DynvectorId),
repeat,
    % retrieve Value at iterator's current position
    dynvector_iterator_current(DynvectorId, Value),
    . . . (do something with Value)
    % increment iterator up to its upper bound
    \+ dynvector_iterator_next(DynvectorId, _Value),
!,
% destroy the iterator
dynvector_iterator_destroy(DynvectorId),
. . .
~~~

`dynvector_iterator_create/1` creates an iterator with a range equals to the entire contents of the *dynvector*, `dynvector_iterator_current/2` retrieves the value at the current position, and `dynvector_iterator_next/2` increments the iterator pointer, failing when it reaches the end of the iterator.

Alternatively, a counter may be used:

~~~
% create counter 'loop', and initialize it to 0
counter_create(loop),
repeat,
    % unify Index with value from counter
    counter_value(loop, Index)
    % retrieve Value from DynvectorId at Index
    dynvector_value(DynvectorId, Index, Value),
    . . . (do something with Value)
    % increment counter up to Count
    counter_inc(loop, Count),
!,
% destroy counter`
counter_destroy(loop),
~~~

`counter_create/1` creates a counter with 0 as its initial value, counter_value/2, retrieves the value of the `counter, dynvector_value/3` retrieves/set `Value` at `Index`, and `counter_inc/2` increments the counter and fails if its resulting value is not `Count`.

Another possibility is to use `maplist/2`:

~~~
. . .
numlist(0, Last, Indices),
maplist(my_pred(DynvectorId), Indices),
. . .
my_pred(DynectorId, Index) :-
    % retrieve Value from DynvectorId at Index
    dynvector_value(DynvectorId, Index, Value),
    . . . (do something with Value)
~~~

Finally, recursivity may be used:

~~~
my_pred(VectorId, 0, Count),
. . .
my_pred(_DynvectorId, Count, Count).
my_pred(DynvectorId, Index, Count) :-
    % retrieve Value from DynvectorId at Index
    dynvector_value(DynvectorId, Index, Value),
    . . . (do something with Value)
    IndexNext is Index + 1,
    my_pred(DynvectorId, IndexNext, Count).
~~~

To search a *dynvector*, that is, to locate a given value within the *dynvector*, use

~~~
dynvector_find(Id, Index, Value)
~~~

*Index* will be unified with the index of *Value*, or the invocation will fail if *Value* does not exist. This is the main purpose of this benchmark: to compare searching a *dynvector* with searching a list in the traditional way, using `memberchk/2`.

To invoke the benchmark, use one of

~~~
benchmark.
benchmark(Benchmarks).
benchmark(Benchmarks, SearchCount, RangeCount).
~~~

where *Benchmarks* is a list with the benchmark type(s) desired (default: [1,2,3,4]), *SearchCount* is the number of random integers in the search and base sets(default: 100000), and *RangeCount* is the range to use (default: 1000000) when generating the two sets of random integers. Note that in SICStus, the number of random integers generated is not exact, but a close approximation, due to the behavior of the predicate used. This, however, in no way affects the benchmark results.

Alternatively, from this folder use one of the command files *benchmarkN.cmd* (where `N` varies from `1` to `4`)

~~~
sicstus < benchmarkN.cmd
~~~

or

~~~
swipl < benchmarkN.cmd
~~~

on either Windows or Ubuntu.

These are the types of benchmark to choose from:

  1. iterate over the *dynvector* with a counter, search with `dynvector_find/3`
  2. iterate over the *dynvector* with maplist/2, search with `dynvector_find/3`
  3. iterate over the *dynvector* with its built-in iterator, search with `dynvector_find/3`
  4. iterate over a list, search with `dynvector_find/3`
  5. iterate over the *dynvector* with a counter, search with `memberchk/2` over a list
  6. iterate over the *dynvector* with `maplist/2`, search with `memberchk/2` over a list
  7. iterate over the *dynvector* with its built-in iterator, search with `memberchk/2` over a list
  8. iterate over a list, search with `memberchk/2` over an unsorted list
  9. iterate over a list, search with `memberchk/2` over a sorted list

Invoking the benchmark with `benchmark` is the same as invoking it with `benchmark([1,2,3,4], 100000, 1000000)`. You may try various benchmark options with different sizes and ranges. SWI-Prolog will respond well over sizes and ranges in the millions. The same is not true for SICStus, as you may verify.

The tables below illustrate the results obtained with all nine options, running SICStus e SWI-Prolog, in three size/range situations. Under each Prolog/OS platform, we show the time it took to complete each run, in milliseconds.

Equipment used:
* Microsoft Windows 10 Enterprise, 64-bit<br/>
  Desktop Intel Core i7-4770K CPU 3.50 GHz x 4, 16.0 GB RAM
* Ubuntu 20.04.01 LTS, 64-bit<br/>
  Notebook Intel Core i7-3537U CPU 2.00 GHz x 4, 7.7 GB RAM
  
#### 1.1. Base/Search size: 100000 - Values range: 1..1000000  

|N|    Iteration     |     Search       |SWI-W(a)|SWI-U(b)|SIC-W(c)|SIC-U(d)|
|-|:-----------------|:-----------------|-------:|-------:|-------:|-------:|
|1|dynvector-counter |dynvector_find    |     390|     572|  250284|  235160|
|2|dynvector-maplist |dynvector_find    |     141|     262|  249693|  337610|
|3|dynvector-iterator|dynvector_find    |     453|     954|  245763|  339518|
|4|list-recursion    |dynvector_find    |     109|     176|  244775|  233869|
|5|dynvector-counter |memberchk         |  217712|  314827|   68863|   99267|
|6|dynvector-maplist |memberchk         |  219466|  314970|   78862|   99203|
|7|dynvector-iterator|memberchk         |  219502|  315775|   67017|   99661|
|8|list-recursion    |memberchk-unsorted|  212149|  316104|   70065|  102154|
|9|list-recursion    |memberchk-sorted  |  223043|  316777|  134461|  132836|
  
  
#### 1.2. Base/Search size: 500000 - Values range: 1..5000000  

|N|    Iteration     |     Search       |SWI-W(a)|SWI-U(b)|SIC-W(c)|SIC-U(d)|
|-|:-----------------|:-----------------|-------:|-------:|-------:|-------:|
|1|dynvector-counter |dynvector_find    |    2062|    2913| 5979917| 6893043|
|2|dynvector-maplist |dynvector_find    |     813|    1279| 5973836| 6423391|
|3|dynvector-iterator|dynvector_find    |    2343|    4759| 5979535| 5959181|
|4|list-recursion    |dynvector_find    |     516|     841| 5973514| 5870632|
|5|dynvector-counter |memberchk         | 5162234| 7903036| 1670254| 2504737|
|6|dynvector-maplist |memberchk         | 5135975| 7906523| 1656815| 2503408|
|7|dynvector-iterator|memberchk         | 5162279| 7927544| 1642470| 2508727|
|8|list-recursion    |memberchk-unsorted| 5167659| 8142615| 1704884| 2580523|
|9|list-recursion    |memberchk-sorted  |12765616|21331819| 5309165|14673131|
  
  
#### 1.3. Base/Search size: 2000000 - Values range: 1..10000000  

|N|    Iteration     |     Search       |SWI-W(a)|SWI-U(b)|SIC-W(c)|SIC-U(d)|
|-|:-----------------|:-----------------|:------:|:------:|:------:|:------:|
|1|dynvector-counter |dynvector_find    |    8626|   12275|   **   |   **   |
|2|dynvector-maplist |dynvector_find    |    3219|    5322|   **   |   **   |
|3|dynvector-iterator|dynvector_find    |    9484|   19525|   **   |   **   |
|4|list-recursion    |dynvector_find    |    1969|    3777|   **   |   **   |
|5|dynvector-counter |memberchk         |   **   |   **   |   **   |   **   |
|6|dynvector-maplist |memberchk         |   **   |   **   |   **   |   **   |
|7|dynvector-iterator|memberchk         |   **   |   **   |   **   |   **   |
|8|list-recursion    |memberchk-unsorted|   **   |   **   |   **   |   **   |
|9|list-recursion    |memberchk-sorted  |   **   |   **   |   **   |   **   |
  
Legends:<br/>
(a) SWI-Prolog on MS Windows 10<br/>
(b) SWI-Prolog on Ubuntu 20.04<br/>
(d) SICStus on Ubuntu 20.04<br/>
(c) SICStus on MS Windows 10<br/>
** It is not feasible to compute the time to complete the benchmark.

### 2. Result Analysis

2.1. The differences found running the benchmark with the same parameters on the same Prolog environment, but different operating systems (Windows and Ubuntu), may be fully explained by the differences in the hardware capabilities (CPU and RAM) of the two host machines.

2.2. Among the iteration strategies tested, SWI-Prolog shows a much better performance when the iteration (the iteration, not the search) is carried out recursively, with standard Prolog lists. SICStus, on the other hand, shows no significative performance difference related to the choice of the iteration mechanism.

2.3. On the subject of searching very large sets, SWI-Prolog shines when its native database hash mechanism is used. As a matter of fact, it shows consistent performance gains of thousands of times (i.e., around three orders of magnitude) over SICStus + its  native hash mechanism, and a little less over SICStus + lists + `memberchk/2`.

2.4. On moderately-sized datasets (less than a few hundred thousand items), SICStus performs around two times better than SWI-Prolog when the search is done with `memberchk/2` over lists, and curiously, even substantially better when the list is unsorted. Taken individually, SICStus performs better when searching with `memberchk/2` than when it uses its native database hash mechanism, whereas the opposite is true for SWI-Prolog.

2.5. If one must deal with very large data sets (i.e., data sets holding from hundreds of thousands to many millions of items), the only viable alternative among the ones attempted here is searching on *dynvectors*, using `dynvector_find/3` and SWI-Prolog. As shown on table 1.3, performance can be as high as 1 million searches per second on a 2 million item data set, using run-of-the-mill PCs. We took the challenge up to searches on data sets containing 10 million items, and SWI-Prolog kept its high marks throughout. This is absolutely impressive in the Prolog world.
