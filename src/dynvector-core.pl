/*******************************************************************************
* FILENAME / MODULE : dynvector-core.pl / dynvector_core
*
* DESCRIPTION :
*       This module provides an implementation of dynvectors. These are their
*       noteworthy characteristics:
*         1. dynvectors are powerful, flexible, extendable, high-performance,
*            hash-based vectors
*         2. dynvectors have O(1) read/insert/update/delete times, and this
*            holds true up to sizes in the order of millions of cells
*         3. dynvectors are not immutable objects, or more specifically,
*            they are not recreated upon modification
*         4. dynvectors have no limitation on the number of cells, apart from
*            the running platform's resource limitations
*         5. dynvectors do not have a maximum number of cells specified at
*            creation time - a dynvector dynamically adjusts its upper bound
*            as needed
*         6. dynvectors demand no storage space reservation previous to the
*            actual cell-by-cell space allocation requests
*         7. dynvectors are resource-minded; their cells are not required to
*            have values assigned to, in any particular sequence or fashion.
*         8. in order to avoid resource wastage, dynvectors should be
*            explicitly destroyed, upon ceasing to be of any further use.
*
* PUBLIC PREDICATES :
*       dynvector_append(+Id, +Value, -Index)
*       dynvector_create(+Id)
*       dynvector_delete(+Id, +Index)
*       dynvector_destroy(+Id)
*       dynvector_fill(+Id, +Value)
*       dynvector_find(+Id, ?Value, ?Index)
*       dynvector_insert(+Id, +Index, +Value)
*       dynvector_label(+Id, +Label, ?Value)
*       dynvector_list(+Id, ?List)
*       dynvector_top(+Id, -Top)
*       dynvector_value(+Id, +Index, ?Value)
*       is_dynvector(+Id)
*
*       dynvector_iterator_create(+Id)
*       dynvector_iterator_create(+Id, +From)
*       dynvector_iterator_create(+Id, +From, +To)
*       dynvector_iterator_destroy(+Id)
*       dynvector_iterator_delete(+Id)
*       dynvector_iterator_append(+Id, -Value)
*       dynvector_iterator_current(+Id, ?Value)
*       dynvector_iterator_first(+Id, ?Value)
*       dynvector_iterator_last(+Id, ?Value)
*       dynvector_iterator_index(+Id, -Index)
*       dynvector_iterator_insert(+Id, -Value)
*       dynvector_iterator_next(+Id, ?Value)
*       dynvector_iterator_prev(+Id, ?Value)
*
* NOTES :
*       None yet.
*
*       Copyright TheWiseCoder 2020.  All rights reserved.
*
* REVISION HISTORY :
*
* DATE        AUTHOR            REVISION
* ----------  ----------------  ------------------------------------------------
* 2020-09-22  GT Nunes          Module creation
*
*******************************************************************************/

:- module(dynvector_core,
    [
        dynvector_append/3,
        dynvector_create/1,
        dynvector_delete/2,
        dynvector_destroy/1,
        dynvector_fill/2,
        dynvector_find/3,
        dynvector_insert/3,
        dynvector_label/3,
        dynvector_list/2,
        dynvector_top/2,
        dynvector_value/3,
        is_dynvector/1,

        dynvector_iterator_append/2,
        dynvector_iterator_create/1,
        dynvector_iterator_create/2,
        dynvector_iterator_create/3,
        dynvector_iterator_current/2,
        dynvector_iterator_delete/1,
        dynvector_iterator_destroy/1,
        dynvector_iterator_first/2,
        dynvector_iterator_index/2,
        dynvector_iterator_insert/2,
        dynvector_iterator_last/2,
        dynvector_iterator_next/2,
        dynvector_iterator_prev/2
    ]).

:- if(current_prolog_flag(dialect, sicstus)).   % SICStus ----------------------

:- use_module(library(lists),
    [
        is_list/1
    ]).

:- elif(current_prolog_flag(dialect, swi)).     % SWI-Prolog -------------------

:- use_module(library(apply),
    [
        maplist/2
    ]).

:- endif.                                       % ------------------------------

:- dynamic  dynvect_labels/3,
            dynvect_values/3.

:- volatile dynvect_labels/3,
            dynvect_values/3.

%-------------------------------------------------------------------------------
% Flexible, iterable, extendable dynvectors. Notes:
%
%   1. the dynvector may be sparsed, i.e., none of its cells must have been
%      unified with a value, but dynvector_value/3 will fail on an attempt
%      to retrieve the value of a non-grounded cell
%
%   2. atoms standing for index may be specified with dynvector_label/3,
%      for subsequent use with dynvector_value/3 and other predicates
%      requiring an index as parameter. For example:
%      a. create the label 'path' to stand for index 0
%         dynvector_label(Id, path, 0),
%      b. now, value at index 0 may be accessed as
%         dynvector_value(Id, path, Value)
%
%   3. elements may be freely inserted, updated, or deleted, and the
%      dynvector's upper bound will automatically adjust to the new index
%
%   4. the elements in the dynvector are stored as facts
%      dynvect_values(Position, Id, Value)
%
%   5. the label in the dynvector are stored as facts
%      dynvect_labels(Id, Label, Value)
%
%   6. the following are the read-only private labels in use, whose values are
%      retrievable with dynvector_label/3:
%        dv_top   - maximum index value in the dynvector
%        dv_first - begin index for iterator
%        dv_last  - end index for iterator
%        dv_curr  - current index for iterator

% create a dynvector
% dynvector_create(+Id)
% Id    atom identifying the dynvector
dynvector_create(Id) :-

    % fail point (make sure id is an atom)
    atom(Id),

    % fail point (make sure id is not taken)
    \+ is_dynvector(Id),

    % register the dynvector's size, max index, and iterator bounds
    assertz(dynvect_labels(Id, dv_top, -1)),
    assertz(dynvect_labels(Id, dv_first, -1)),
    assertz(dynvect_labels(Id, dv_last, -1)),
    assertz(dynvect_labels(Id, dv_curr, -1)).

% destroy a dynvector, and release all of its resources (no fail)
% dynvector_destroy(+Id)
% Id    atom identifying the dynvector
dynvector_destroy(Id) :-

  retractall(dynvect_labels(Id, _, _)),
  retractall(dynvect_values(_, Id, _)).

% Unify Top with the highest inserted index value in the dynvector, 
% even if it has subsequently been deleted. Upon dynvector's creation,
% this highest value is set to -1.
% dynvector_top(+Id, +Dim, -Top)
% Id    atom identifying the dynvector
% Top   value of the highest index
dynvector_top(Id, Top) :-
    dynvect_labels(Id, dv_top, Top).

% fail if Id does not identify a dynvector
% is_dynvector(+Id)
% Id    atom identifying the dynvector
is_dynvector(Id) :-
    dynvect_labels(Id, dv_top, _).

% Unify Index or Value with an occurrence of Index or Value in the
% dynvector, respectively. Fail if no such value or index exists.
% dynvector_find(+Id, +Value, -Index)
% Id        atom identifying the dynvector
% Value     the reference value
% Index     the reference index
dynvector_find(Id, Value, Index) :-
    dynvect_values(Index, Id, Value).

%-------------------------------------------------------------------------------
% Unify the specified dynvector element with its value. Fail on attempt
% to retrieve the value of an empty cell.

% dynvector_value(+Id, +Index, ?Value)
% Id        atom identifying the dynvector
% Index     the reference index, or a label standing for it
% Value     the dynvector element value
dynvector_value(Id, Index, Value) :-

    % determine the element's index from label, if necessary
    ( (integer(Index) , Inx = Index)
    ; dynvect_labels(Id, Index, Inx) ),

    % has Value been grounded ?
    (ground(Value) ->
       % yes, so register value and top index
       dynvector_value_(Id, Inx, Value)
    ;
       % no, so retrieve value
       !,
       % fail point (cell might be empty)
       dynvect_values(Inx, Id, Value)
    ).

dynvector_value_(Id, Index, Value) :-

    % register value
    (retract(dynvect_values(Index, Id, _)) ; true),
    assertz(dynvect_values(Index, Id, Value)),

    % register top index, if appropriate
    dynvect_labels(Id, dv_top, Top),
    ( Index =< Top
    ; ( retract(dynvect_labels(Id, dv_top, _))
      , assertz(dynvect_labels(Id, dv_top, Index)) ) ).

%-------------------------------------------------------------------------------
% append the given value or list of values to the dynvector

% dynvector_append(+Id, +Value, -Index)
% Id        atom identifying the dynvector
% Value     value or list of of values to append
% Index     index identifying the element holding Value or its first element

dynvector_append(Id, Value, Index) :-

    % compute the index for the Value
    dynvect_labels(Id, dv_top, Top),
    Index is Top + 1,

    % is Value a list of Values ?
    (is_list(Value) ->
        % yes, so append the list of values to the dynvector
        Values = Value

    % is Value a dynvector ?
    ; is_dynvector(Value) ->
        % yes, so append the source dynvector values into the target dynvector
        findall(Value, dynvect_values(_Index, Id, Value), Values)

    % Value is a single entity
    ; otherwise ->
        % so, append it to the dynvector
        Values = [Value]
    ),

    % append Values to the dynvector
    dynvector_append_(Values, Id, Index),

    % register the new Top
    length(Values, Count),
    TopNew is Top + Count,
    retract(dynvect_labels(Id, dv_top, _)),
    assertz(dynvect_labels(Id, dv_top, TopNew)).

dynvector_append_([], _Id, _Index).

dynvector_append_([Value|Values], Id, Index) :-

    assertz(dynvect_values(Index, Id, Value)),
    IndexNext is Index + 1,
    dynvector_append_(Values, IndexNext, Id).

%-------------------------------------------------------------------------------
% insert the given value or list of values at the given index inro the dynvector

% dynvector_insert(+Id, +Index, +Value)
% Id        atom identifying the dynvector
% Index     the insertion point
% Value     value or list of of values to insert
dynvector_insert(Id, Index, Value) :-

    % is Value a list of Values ?
    (is_list(Value) ->
        % yes, so make it explicit
        Values = Value

    % is Value a dynvector ?
    ; is_dynvector(Value) ->
        % yes, so obtain its list of values
        findall(Value, dynvect_values(_Index, Id, Value), Values)

    % Value is a single entity
    ; otherwise ->
        % so, obtain a list with a Value as its single element
        Values = [Value]
    ),

    % insert the values
    dynvect_labels(Id, dv_top, Top),
    dynvector_insert_(Values, Id, Top, Index, Top),

    % register the new top
    length(Values, Count),
    TopNew is Top + Count,
    retract(dynvect_labels(Id, dv_top, _)),
    assertz(dynvect_labels(Id, dv_top, TopNew)).

% (done)
dynvector_insert_([], _Id, _Top, _From, _To).

% (iterate)
dynvector_insert_([Value|Values], Id, Top, From, To) :-

    ToNext is To + 1,

    % is there a value at From ?
    ((From =< Top , dynvect_values(From, Id, Value)) ->
        % yes, so move it to ToNext
        retract(dynvect_values(From, Id, _Value)),
        assertz(dynvect_values(ToNext, Id, Value))
    ;
        % no, so proceed
        true
    ),

    % register Value at From
    assertz(dynvect_values(From, Id, Value)),

    % go for the next value
    FromNext is From + 1,
    dynvector_insert_(Values, Id, Top, FromNext, ToNext).

%-------------------------------------------------------------------------------

% erase the dynvector cell at the given index
% (will fail if no such element exists.)
% dynvector_delete(+Id, +Index)
% Id        atom identifying the dynvector
% Index     the reference index, or a label standing for it
dynvector_delete(Id, Index) :-

    % determine the element's Index from Label, if necessary
    ( (integer(Index) , Inx = Index)
    ; dynvect_labels(Id, Index, Inx) ),

    % erase the cell
    !,
    % fail point (cell might already be empty)
    retract(dynvect_values(Inx, Id, _)).

% unify Value with the value associated with the named attribute
% Id        atom identifying the dynvector
% Label     atom standing for the named attribute
% Value     associated with the named attribute
% dynvector_label(+Id, +Label, ?Value)
dynvector_label(Id, Label, Value) :-

    (ground(Value) ->
        % fail point (must be an atom, and must not start with dv_)
        \+ sub_atom(Label, 0, 3, _, dv_),
        (retract(dynvect_labels(Id, Label, _)) ; true),
        assertz(dynvect_labels(Id, Label, Value))
    ;
        dynvect_labels(Id, Label, Value)
    ).

%-------------------------------------------------------------------------------
% Unify all of the cells of the dynvector with the given list. A dynvector
% capable of to hold all the list elements may be created. Note that this
% is not a serialization mechanism, and as such it should not be used for
% backup/restore purposes.

% dynvector_list(+Id, ?List)
% Id        atom identifying the dynvector
% List      list to dump the dynvector cells to
dynvector_list(Id, List) :-

    % HAZARD: ground(List) might be very expensive
    (var(List) ->
        % load all values in dynvector into List
        findall(Value, dynvect_values(_Index, Id, Value), List)
    ;
        % clear dynvector or create it anew
        ( (is_dynvector(Id) , retractall(dynvect_values(_, Id, _)))
        ; dynvector_create(Id) ),

        % List might be a list or a dynvector
        % (in the latter case, List is an atom holding the dynvector id)
        ( (is_list(List) , Values = List)
        ; (findall(Value, dynvect_values(_, List, Value), Values)) ),

        % load all values in Values into dynvector
        list_to_dynvector_(Values, Id, 0)
    ).

% list_to_dynvector_(+[Value|List], +Id, +Index)
% [Value|List]  the value to unify the dynvector cell with
% Id        atom identifying the dynvector
% Index     index identifying the dynvector cell

% (done)
list_to_dynvector_([], Id, Index) :-

    % register the top index for the dynvector
    Top is Index - 1,
    retract(dynvect_labels(Id, dv_top, _)),
    assertz(dynvect_labels(Id, dv_top, Top)).

% (iterate)
list_to_dynvector_([Value|List], Id, Index) :-

    assertz(dynvect_values(Index, Id, Value)),
    IndexNext is Index + 1,
    list_to_dynvector_(List, Id, IndexNext).

%-------------------------------------------------------------------------------
% unify all of the cells of the dynvector with the given value

% dynvector_fill(+Id, +Value)
% Id        atom identifying the dynvector
% Value     value to unify the dynvector cells with
dynvector_fill(Id, Value) :-

    retractall(dynvect_values(_, Id, _)),
    dynvect_labels(Id, dv_top, Count),
    dynvector_fill_(Id, Value, 0, Count).

% dynvector_fill_(+Id, +Value, +Index, +Count)
% Id        atom identifying the dynvector
% Value     value to unify the dynvector cell with
% Index     0-based index identifying the dynvector cell
% Count     number of cells in dynvector

% (done)
dynvector_fill_(_Id, _Value, Count, Count).

% (iterate)
dynvector_fill_(Id, Value, Index, Count) :-

    % load Value into the cell at Index
    assertz(dynvect_values(Index, Id, Value)),

    % go for thew next index
    IndexNext is Index + 1,
    dynvector_fill_(Id, Value, IndexNext, Count).

%-------------------------------------------------------------------------------
% Iterator creation / destruction

% create iterator from 0 to Top
% dynvector_iterator_create(+Id)
% Id    atom identifying the dynvector
dynvector_iterator_create(Id) :-

    dynvect_labels(Id, dv_top, Top),
    dynvector_iterator_create(Id, 0, Top).

% create iterator from From to Top
% dynvector_iterator_create(+Id, +From)
% Id    atom identifying the dynvector
% From  the iterator's first index
dynvector_iterator_create(Id, From) :-

    dynvect_labels(Id, dv_top, Top),
    dynvector_iterator_create(Id, From, Top).

% create iterator from From to To
% dynvector_iterator_create(+Id, +To)
% Id    atom identifying the dynvector
% From  the iterator's first index
% To    the iterator's last index
dynvector_iterator_create(Id, From, To) :-

    % fail points (From and To must be consistent)
    To >= From,
    From >= 0,

    dynvect_labels(Id, dv_top, Top),
    %fail point (iterator's upper bound must be within dynvector's bounds)
    To >= Top,

    % fail point (dynvector cannot already have an active iterator)
    dynvect_labels(Id, dv_first, -1),

    % register the iterator
    retract(dynvect_labels(Id, dv_first, _)),
    assertz(dynvect_labels(Id, dv_first, From)),
    retract(dynvect_labels(Id, dv_last, _)),
    assertz(dynvect_labels(Id, dv_last, To)),
    retract(dynvect_labels(Id, dv_curr, _)),
    assertz(dynvect_labels(Id, dv_curr, From)).

% Destroy the dynvector's iterator. Fail if dynvector does not exist.
% dynvector_iterator_destroy(+Id)
% Id    atom identifying the dynvector
dynvector_iterator_destroy(Id) :-

    retract(dynvect_labels(Id, dv_first, _)),
    assertz(dynvect_labels(Id, dv_first, -1)),
    retract(dynvect_labels(Id, dv_last, _)),
    assertz(dynvect_labels(Id, dv_last, -1)),
    retract(dynvect_labels(Id, dv_curr, _)),
    assertz(dynvect_labels(Id, dv_curr, -1)).

%-------------------------------------------------------------------------------

% unify Value with the value at iterator's next position
% dynvector_iterator_next(+Id, ?Value)
% Id        atom identifying the dynvector
% Value     value to unify the iterator's position with
dynvector_iterator_next(Id, Value) :-

    % retrieve iterator's current position
    dynvect_labels(Id, dv_curr, Current),
    % fail point (iterator must be active)
    Current > -1,

    % retrieve iterator's last position
    dynvect_labels(Id, dv_last, Last),
    !,
    % fail point
    dynvector_iterator_next_(Id, Current, Last, Value).

dynvector_iterator_next_(_Id, Last, Last, _Value) :- !, fail.

dynvector_iterator_next_(Id, Current, Last, Value) :-

    Next is Current + 1,
    % attempt to unify Value with the value at Next, OR
    ( dynvector_iterator_nav_(Id, Next, Value)
    % go for the next position
    ; (!, dynvector_iterator_next_(Id, Next, Last, Value)) ).

%-------------------------------------------------------------------------------

% unify Value with the value at iterator's previous position
% dynvector_iterator_prev(+Id, ?Value)
% Id        atom identifying the dynvector
% Value     value to unify the iterator's position with
dynvector_iterator_prev(Id, Value) :-

    % retrieve iterator's current position
    dynvect_labels(Id, dv_curr, Current),
    % fail point (iterator must be active)
    Current > -1,

    % retrieve iterator's first position
    dynvect_labels(Id, dv_first, First),
    !,
    % fail point
    dynvector_iterator_prev_(Id, Current, First, Value).

dynvector_iterator_prev_(_Id, First, First, _Value) :- !, fail.

dynvector_iterator_prev_(Id, Current, First, Value) :-

    Prev is Current - 1,
    % attempt to unify Value with the value at Prev, OR
    ( dynvector_iterator_nav_(Id, Prev, Value)
    % go for the previous position
    ; (!, dynvector_iterator_prev_(Id, Prev, First, Value)) ).

%-------------------------------------------------------------------------------

% unify Value with the value at iterator's first position
% dynvector_iterator_first(+Id, ?Value)
% Id        atom identifying the dynvector
% Value     value to unify the iterator's position with
dynvector_iterator_first(Id, Value) :-

    % obtain iterator's first index
    dynvect_labels(Id, dv_first, First),
    % fail point (iterator must be active)
    First > -1,

    % unify Value with the value at First
    !,
    % fail point
    dynvector_iterator_nav_(Id, First, Value).

% unify Value with the value at iterator's last position
% dynvector_iterator_last(+Id, ?Value)
% Id        atom identifying the dynvector
% Value     value to unify the iterator's position with
dynvector_iterator_last(Id, Value) :-

    % obtain iterator's last index
    dynvect_labels(Id, dv_last, Last),
    % fail point (iterator must be active)
    Last > -1,

    % unify Value with the value at Last
    !,
    % fail point
    dynvector_iterator_nav_(Id, Last, Value).

% unify Value with the value at Index
% dynvector_iterator_nav_(+Id, ?Value)
% Id        atom identifying the dynvector
% Value     value to unify the iterator's position with
dynvector_iterator_nav_(Id, Index, Value) :-

    % has Value been grounded ?
    (ground(Value) ->
       % yes, so register value
       (retract(dynvect_values(Index, Id, Value)) ; true),
       assertz(dynvect_values(Index, Id, Value))
    ;
       % no, so retrieve value
       !,
       % fail point (cell might be empty)
       dynvect_values(Index, Id, Value)
    ),

    % adjust iterator's current index
    retract(dynvect_labels(Id, dv_curr, _)),
    assertz(dynvect_labels(Id, dv_curr, Index)).

%-------------------------------------------------------------------------------
% Iterator operations

% unify Value with the value at iterator's current position
% dynvector_iterator_current(+Id, ?Value)
% Id        atom identifying the dynvector
% Value     value to unify the iterator's position with
dynvector_iterator_current(Id, Value) :-

    % obtain iterator's current index
    dynvect_labels(Id, dv_curr, Current),
    % fail point (iterator must be active)
    Current > -1,

    % unify Value with the value at Current
    !,
    % fail point
    dynvector_iterator_nav_(Id, Current, Value).

% remove value at iterator's current position
% dynvector_iterator_delete(+Id)
% Id        atom identifying the dynvector
dynvector_iterator_delete(Id) :-

    % obtain iterator's current index
    dynvect_labels(Id, dv_curr, Current),
    % fail point (iterator must be active)
    Current > -1,

    % remove value
    (retract(dynvect_values(Current, Id, _)) ; true).

% unify Index with iterator's current index
% dynvector_iterator_index(+Id, -Index)
% Id        atom identifying the dynvector
% Index     the iterator's current index
dynvector_iterator_index(Id, Index) :-

    % fail point (Index must be a var)
    var(Index),

    % obtain iterator's current index
    dynvect_labels(Id, dv_curr, Index).

% insert Value at iterator's current position
% dynvector_iterator_insert(+Id, ?Value)
% Id        atom identifying the dynvector
% Value     value to be inserted
dynvector_iterator_insert(Id, Value) :-

    % obtain iterator's current position
    dynvect_labels(Id, dv_curr, Index),
    % fail point (iterator mus be active)
    Index > -1,

    % save dynvector's current Top
    dynvect_labels(Id, dv_top, Top),

    % insert Value (Value might be a singleton, a list, or another dynvector)
    dynvector_insert(Id, Index, Value),

    % adjust iterator's last index
    dynvect_labels(Id, dv_last, Last),
    dynvect_labels(Id, dv_top, TopNew),
    LastNew is Last + TopNew - Top,
    retract(dynvect_labels(Id, dv_last, _)),
    assertz(dynvect_labels(Id, dv_last, LastNew)).

% insert Value following iterator's last position
% dynvector_iterator_append(+Id, ?Value)
% Id        atom identifying the dynvector
% Value     value to be inserted
dynvector_iterator_append(Id, Value) :-

    % obtain iterator's current position
    dynvect_labels(Id, dv_last, Last),
    % fail point (iterator mus be active)
    Last > -1,

    % save dynvector's current Top
    dynvect_labels(Id, dv_top, Top),

    % insert Value (Value might be a singleton, a list, or another dynvector)
    Index is Last + 1,
    dynvector_insert(Id, Index, Value),

    % adjust iterator's last index
    dynvect_labels(Id, dv_last, Last),
    dynvect_labels(Id, dv_top, TopNew),
    LastNew is Last + TopNew - Top,
    retract(dynvect_labels(Id, dv_last, _)),
    assertz(dynvect_labels(Id, dv_last, LastNew)).
