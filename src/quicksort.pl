:- module(quicksort,
    [
        quicksort/3,
        quicksort_number_asc/3,
        quicksort_number_desc/3
    ]).

/** <module> Binary search on sorted lists

An implementation of the classic `Quicksort` sorting algorithm on generic lists.
`Quicksort` works by selecting a 'pivot' element from the list to be sorted and
partitioning the other elements into two sublists, according to whether they are
less than or greater than the pivot.The sublists are then sorted recursively.
For a description of the quicksort algorithm, see
https://en.wikipedia.org/wiki/Quicksort .<br/>
This implementation takes as input a `comparator`. This is a predicate able to
perform a comparison between any two elements of the input list as parameters,
and return a negative number, zero, or a positive number, to indicate whether the
first parameter is smaller than, equal to, or greater than the second parameter,
respectively.  

@author GT Nunes
@version 1.3.1
@copyright (c) TheWiseCoder 2020-2021
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- meta_predicate quicksort(+, 3, -).
:- meta_predicate quicksort_partition(3, +, +, +, +).

%! quicksort(+List:list, :Comparator:pred, -SortedList:list) is det.
%
%  Sort the contents of List according to the given comparison predicate,
% and unify the result with SortedList. <br/>
%  The comparison predicate must accept two parameters, `ValueX` and `ValueY`,
%  which might be any two elements in List, and have the following behavior:
%  ~~~
%  <Comparator>(+ValueX, +ValueY, -Result:number) is det
%  where Result is unified with
%    a) 0 (zero)          - ValueX is equal to ValueY
%    b) a negative number - ValueX is less than ValueY
%    c) a positive number - ValueX is greater than ValueY
%  ~~~
%  The criteria that will determine the results of the comparisons are entirely up
%  to Comparator, and as such it must be able to handle the values it will receive.
%  Nothing is done if List has less than 2 elements.
%
%  @param List       The list to be sorted
%  @param Comparator Predicate to perform comparisons between any two elements in List
%  @param SortedList The resulting sorted list

quicksort(List, Comparator, SortedList) :-

    % does the input contain more than one element ?
    length(List, Len),
    (Len > 1 ->
        % yes, so sort them using the given comparator
        quicksort_list(Comparator, List, SortedList)
    ;
        % no, so just unify SortedList with List and exit
        SortedList = List
    ).

quicksort_list(_Comparator, [], []).
    
quicksort_list(Comparator, [ValueX|ValuesX], ValuesY) :-

    quicksort_partition(Comparator, ValuesX, ValueX, ValuesLeft, ValuesRight),
    quicksort_list(Comparator, ValuesLeft, ListLeft),
    quicksort_list(Comparator, ValuesRight, ListRight),
    quicksort_append(ListLeft, [ValueX|ListRight], ValuesY).

quicksort_partition(_Comparator, [], _ValueY, [], []).

quicksort_partition(Comparator, [ValueX|ValuesX],
                        ValueY, [ValueX|ListLeft], ListRight) :-

    call(Comparator, ValueX, ValueY, Cmp),
    Cmp =< 0,
    quicksort_partition(Comparator, ValuesX, ValueY, ListLeft, ListRight).

quicksort_partition(Comparator, [ValueX|ValuesX],
                        ValueY, ListLeft, [ValueX|ListRight]) :-

    call(Comparator, ValueX, ValueY, Cmp),
    Cmp > 0,
    quicksort_partition(Comparator, ValuesX, ValueY, ListLeft, ListRight).

quicksort_append([], ValuesY, ValuesY).

quicksort_append([ValueX|ValuesX], ValuesY, [ValueX|ValuesZ]) :-
    quicksort_append(ValuesX, ValuesY, ValuesZ).

%-------------------------------------------------------------------------------------

%! quicksort_number_asc(+ValueX:number, +ValueY:number, -Cmp:int) is det.
%
%  Compare the numbers ValueX and ValueY ascendingly. <br/>
%  Unify Cmp with -1, 0, or 1, depending on whether ValueX is smaller than,
%  equal to, or greater than, ValueY.
%
%  @param ValueX The first value to compare
%  @param ValueY The second value to compare
%  @param Cmp    The result of the comparison
%

quicksort_number_asc(ValueX, ValueY, Cmp) :-

    (ValueX < ValueY ->
        Cmp = -1
    ; ValueX > ValueY ->
        Cmp = 1
    ; otherwise ->
        Cmp = 0
    ).

%! quicksort_number_desc(+ValueX:number, +ValueY:number, -Cmp:int) is det.
%
%  Compare the numbers ValueX and ValueY descendingly. <br/>
%  Unify Cmp with -1, 0, or 1, depending on whether ValueX is greater than,
%  equal to, or smaller than, ValueY.
%
%  @param ValueX The first value to compare
%  @param ValueY The second value to compare
%  @param Cmp    The result of the comparison
%

quicksort_number_desc(ValueX, ValueY, Cmp) :-

    (ValueX > ValueY ->
        Cmp = -1
    ; ValueX < ValueY ->
        Cmp = 1
    ; otherwise ->
        Cmp = 0
    ).
