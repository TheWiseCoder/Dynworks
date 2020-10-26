:- module(benchmark,
    [
        benchmark/0,
        benchmark/1,            % benchmark(+Benchmarks)
        benchmark/3             %  benchmark(+Benchmarks, +SearchCount, +RangeCount)
    ]).

/** <module> Dynvector Benchmark

This benchmark is also a beginner's tutorial on how to use dynvectors.
It will go through some significant features of the package. Make sure
to complement it by reviewing the documentation.

@author GT Nunes
@version 1.1.1
@copyright (c) 2020 GT Nunes
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- if(current_prolog_flag(dialect, sicstus)).   % SICStus ----------------------------

:- use_module(library(between),
    [
        numlist/3
    ]).

:- use_module(library(lists),
    [
        maplist/2
    ]).

:- use_module(library(random),
    [
        random_numlist/4,
        random_permutation/2
    ]).

:- use_module('../src/sicstus/simple_counter',
    [
        counter_create/1,
        counter_inc/1,
        counter_inc/2,
        counter_value/2
    ]).

get_flag(Key, Value) :-
    ( bb_get(Key, Value)
    ; (Value = 0 , bb_put(Key, Value)) ).

set_flag(Key, Value) :- bb_put(Key, Value).

randseq(K, N, Set) :-
    K =< N,
    P is float(K) / float(N),
    random_numlist(P, 1, N, Ints),
    random_permutation(Ints, Set).

:- elif(current_prolog_flag(dialect, swi)).     % SWI-Prolog -------------------------

:- use_module(library(apply),
    [
        maplist/3
    ]).

:- use_module(library(lists),
    [
        numlist/3
    ]).

:- use_module('../src/swi/simple_counter',
    [
        counter_create/1,
        counter_inc/1,
        counter_inc/2,
        counter_value/2
    ]).

:- endif.                                       % ------------------------------------

:- use_module('../src/dynvector_core',
    [
        dynvector_create/1,
        dynvector_destroy/1,
        dynvector_find/3,
        dynvector_iterator_create/1,
        dynvector_iterator_current/2,
        dynvector_iterator_destroy/1,
        dynvector_iterator_next/2,
        dynvector_list/2,
        dynvector_value/3
    ]).

%! benchmark is det.
%
%  Invoke benchmark with default values for `Benchmarks`/`SearchCount`/`RangeCount`.

benchmark :-
    benchmark([1,2,3,4], 100000, 1000000).

%! benchmark(+Benchmarks) is det.
%
%  Invoke benchmark with default values for `SearchCount`/`RangeCount`.
%
%  @param Benchmarks List of operations to perform

benchmark(Benchmarks) :-
    benchmark(Benchmarks, 100000, 1000000).

%! benchmark(+Benchmarks, +SearchCount, +RangeCount) is det.
%
%  Invoke benchmark with given parameters.
%
%  @param Benchmarks  List of operations to perform
%  @param SearchCount Number of randoms integers in base and search lists
%  @param RangeCount  Upper bound for random integers generation

benchmark(Benchmarks, SearchCount, RangeCount) :-

    % fail point
    RangeCount >= SearchCount,

    % identify the Prolog platform
    current_prolog_flag(dialect, Dialect),

    % identify the OS (SICStus)
    ( current_prolog_flag(platform_data,  platform(Family, _Name, _Extra))
    % is it Windows ? (SWI-Prolog)
    ; (current_prolog_flag(windows, true) , Family = windows)
    % is it Unix ? (SWI-Prolog)
    ; (current_prolog_flag(unix, true) , Family = unix)
    ),

    % display the platform
    format('Platform ~a on ~a~n', [Dialect,Family]),

    % create the counters
    counter_create(loop),
    counter_create(pass),
    counter_create(fail),

    % create the base data set
    randseq(SearchCount, RangeCount, BaseData),
    length(BaseData, LenData),
    format('~n~d random base numbers generated in range 1..~d~n',
           [LenData,RangeCount]),
    dynvector_destroy(dyn_base),
    dynvector_list(dyn_base, BaseData),

    % create the search data set
    randseq(SearchCount, RangeCount, SearchData),
    length(SearchData, LenSearch),
    format('~d random search numbers generated in range 1..~d~n',
           [LenSearch,RangeCount]),
    dynvector_destroy(dyn_search),
    dynvector_list(dyn_search, SearchData),

    % run the benchmarks
    maplist(benchmarks(SearchData, BaseData, dyn_search, dyn_base, LenSearch),
            Benchmarks).

%-------------------------------------------------------------------------------

benchmarks(_SearchData, _BaseData, SearchDyn, BaseDyn, LenSearch, 1) :-
    benchmark_dynvector_counter(SearchDyn, BaseDyn, LenSearch).

benchmarks(_SearchData, _BaseData, SearchDyn, BaseDyn, LenSearch, 2) :-
    benchmark_dynvector_maplist(SearchDyn, BaseDyn, LenSearch).

benchmarks(_SearchData, _BaseData, SearchDyn, BaseDyn, _LenSearch, 3) :-
    benchmark_dynvector_iterator(SearchDyn, BaseDyn).

benchmarks(SearchData, _BaseData, _SearchDyn, BaseDyn, _LenSearch, 4) :-
    benchmark_list_dynvector(SearchData, BaseDyn).

benchmarks(_SearchData, BaseData, SearchDyn, _BaseDyn, LenSearch, 5) :-
    benchmark_dynvector_counter_memberchk(SearchDyn, BaseData, LenSearch).

benchmarks(_SearchData, BaseData, SearchDyn, _BaseDyn, LenSearch, 6) :-
    benchmark_dynvector_maplist_memberchk(SearchDyn, BaseData, LenSearch).

benchmarks(_SearchData, BaseData, SearchDyn, _BaseDyn, _LenSearch, 7) :-
    benchmark_dynvector_iterator_memberchk(SearchDyn, BaseData).

benchmarks(SearchData, BaseData, _SearchDyn, _BaseDyn, _LenSearch, 8) :-
    benchmark_list_unsorted(SearchData, BaseData).

benchmarks(SearchData, BaseData, _SearchDyn, _BaseDyn, _LenSearch, 9) :-
    benchmark_list_sorted(SearchData, BaseData).

%-------------------------------------------------------------------------------------
% dynvector counter iteration, dynvector search

benchmark_dynvector_counter(DynlSD, DynlBD, Count) :-

    write('\n1) Dynvector counter iteration, unsorted dynvector search\n'),
    counter_value(loop, 0),
    benchmark_begin,

    repeat,
        counter_value(loop, Index),
        dynvector_value(DynlSD, Index, Value),
        (dynvector_find(DynlBD, _, Value) ->
            counter_inc(pass)
        ;
            counter_inc(fail)
        ),
        % fail point
        counter_inc(loop, Count),

    !,
    benchmark_end.

%-------------------------------------------------------------------------------------
% dynvector maplist iteration, dynvector search

benchmark_dynvector_maplist(DynlSD, DynlBD, Count) :-

    write('\n2) Dynvector maplist iteration, unsorted dynvector search\n'),
    Last is Count - 1,
    numlist(0, Last, Counts),
    benchmark_begin,
    maplist(benchmark_dynvector_maplist_(DynlSD, DynlBD), Counts),
    benchmark_end.

benchmark_dynvector_maplist_(DynlSD, DynlBD, Count) :-

    dynvector_value(DynlSD, Count, Value),
    (dynvector_find(DynlBD, _Index, Value) ->
        counter_inc(pass)
    ;
        counter_inc(fail)
    ).

%-------------------------------------------------------------------------------------
% dynvector iterator iteration, dynvector search

benchmark_dynvector_iterator(DynlSD, DynlBD) :-

    write('\n3) Dynvector iterator iteration, unsorted dynvector search\n'),
    dynvector_iterator_destroy(DynlSD),
    dynvector_iterator_create(DynlSD),
    benchmark_begin,

    repeat,
        dynvector_iterator_current(DynlSD, Value),
        (dynvector_find(DynlBD, _Index, Value) ->
            counter_inc(pass)
        ;
            counter_inc(fail)
        ),
        % fail point
        \+ dynvector_iterator_next(DynlSD, _Value),

    !,
    benchmark_end.

%-------------------------------------------------------------------------------------
% List iteration, dynvector search

benchmark_list_dynvector(SearchData, DynlBD) :-

    write('\n4) List iteration, unsorted dynvector search\n'),
    benchmark_begin,
    benchmark_list_dynvector_(SearchData, DynlBD).

% (done)
benchmark_list_dynvector_([], _) :-
    benchmark_end.

% (iterate)
benchmark_list_dynvector_([H|SearchData], DynlBD) :-

    (dynvector_find(DynlBD, _Index, H) ->
        counter_inc(pass)
    ;
        counter_inc(fail)
    ),
    benchmark_list_dynvector_(SearchData, DynlBD).

%-------------------------------------------------------------------------------------
% dynvector counter iteration, memberchk search

benchmark_dynvector_counter_memberchk(DynlSD, BaseData, Count) :-

    write('\n5) Dynvector counter iteration, unsorted memberchk search\n'),
    counter_value(loop, 0),
    benchmark_begin,

    repeat,
        counter_value(loop, Curr),
        dynvector_value(DynlSD, Curr, Value),
        (memberchk(Value, BaseData) ->
            counter_inc(pass)
        ;
            counter_inc(fail)
        ),
        % fail point
        counter_inc(loop, Count),

    !,
    benchmark_end.

%-------------------------------------------------------------------------------------
% dynvector maplist iteration, memberchk search

benchmark_dynvector_maplist_memberchk(DynlSD, BaseData, Count) :-

    write('\n6) Dynvector maplist iteration, unsorted memberchk search\n'),
    Last is Count - 1,
    numlist(0, Last, Counts),
    benchmark_begin,
    maplist(benchmark_dynvector_maplist_memberchk_(DynlSD, BaseData), Counts),
    benchmark_end.

benchmark_dynvector_maplist_memberchk_(DynlSD, BaseData, Count) :-

    dynvector_value(DynlSD, Count, Value),
    (memberchk(Value, BaseData) ->
        counter_inc(pass)
    ;
        counter_inc(fail)
    ).

%-------------------------------------------------------------------------------------
% dynvector iterator iteration, memberchk search

benchmark_dynvector_iterator_memberchk(DynlSD, List) :-

    write('\n7) Dynvector iterator iteration, unsorted memberchk search\n'),
    benchmark_begin,
    dynvector_iterator_destroy(DynlSD),
    dynvector_iterator_create(DynlSD),

    repeat,
        dynvector_iterator_current(DynlSD, Value),
        (memberchk(Value, List) ->
            counter_inc(pass)
        ;
            counter_inc(fail)
        ),
        % fail point
        \+ dynvector_iterator_next(DynlSD, _Value),

    !,
    benchmark_end.

%-------------------------------------------------------------------------------------
% List iteration, unsorted/sorted memberchk search

benchmark_list_unsorted(SearchData, BaseData) :-

    write('\n8) List iteration, unsorted memberchk search\n'),
    benchmark_begin,
    benchmark_list_(SearchData, BaseData).

benchmark_list_sorted(SearchData, UnsortedData) :-

    write('\n9) List iteration, sorted memberchk search\n'),
    sort(UnsortedData, SortedData),
    benchmark_begin,
    benchmark_list_(SearchData, SortedData).

% (done)
benchmark_list_([], _) :-
    benchmark_end.

% (iterate)
benchmark_list_([H|SearchData], BaseData) :-

    (memberchk(H, BaseData) ->
        counter_inc(pass)
    ;
        counter_inc(fail)
    ),
    benchmark_list_(SearchData, BaseData).

%-------------------------------------------------------------------------------------

% counters and timer initialization
benchmark_begin :-

    counter_value(pass, 0),
    counter_value(fail, 0),

    statistics(walltime, [Start,_]),
    set_flag(timer, Start).

% duration and success/failure tallies
benchmark_end :-

    statistics(walltime, [Lap,_]),
    get_flag(timer, Start),
    Duration is Lap - Start,

    counter_value(pass, Hits),
    counter_value(fail, Misses),
    format('~d hits, ~d misses, ~d millis~n',
           [Hits,Misses,Duration]).
