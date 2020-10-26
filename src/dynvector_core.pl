:- module(dynvector_core,
    [
        dynvector_append/3,             % dynvector_append(+Id, +Value, -Index)
        dynvector_create/1,             % dynvector_create(+Id)
        dynvector_delete/2,             % dynvector_delete(+Id, +Index)
        dynvector_destroy/1,            % dynvector_destroy(+Id)
        dynvector_fill/2,               % dynvector_fill(+Id, +Value)
        dynvector_find/3,               % dynvector_find(+Id, ?Index, ?Value)
        dynvector_insert/3,             % dynvector_insert(+Id, +Index, +Value)
        dynvector_label/3,              % dynvector_label(+Id, +Label, ?Value)
        dynvector_list/2,               % dynvector_list(+Id, ?List)
        dynvector_top/2,                % dynvector_top(+Id, -Top)
        dynvector_value/3,              % dynvector_value(+Id, +Index, ?Value)
        is_dynvector/1,                 % is_dynvector(+Id)

        dynvector_iterator_append/2,    % dynvector_iterator_create(+Id)
        dynvector_iterator_create/1,    % dynvector_iterator_create(+Id, +From)
        dynvector_iterator_create/2,    % dynvector_iterator_create(+Id, +From, +To)
        dynvector_iterator_create/3,    % dynvector_iterator_destroy(+Id)
        dynvector_iterator_current/2,   % dynvector_iterator_delete(+Id)
        dynvector_iterator_delete/1,    % dynvector_iterator_append(+Id, -Value)
        dynvector_iterator_destroy/1,   % dynvector_iterator_current(+Id, ?Value)
        dynvector_iterator_first/2,     % dynvector_iterator_first(+Id, ?Value)
        dynvector_iterator_index/2,     % dynvector_iterator_last(+Id, ?Value)
        dynvector_iterator_insert/2,    % dynvector_iterator_index(+Id, -Index)
        dynvector_iterator_last/2,      % dynvector_iterator_insert(+Id, -Value)
        dynvector_iterator_next/2,      % dynvector_iterator_next(+Id, ?Value)
        dynvector_iterator_prev/2       % dynvector_iterator_prev(+Id, ?Value)
    ]).

/** <module> Dynamic vectors

This module provides an implementation of dynvectors. These are their
noteworthy characteristics:

a. dynvectors are powerful, flexible, extendable, high-performance,
hash-based vectors;

b. dynvectors have O(1) read/insert/update/delete times, and this holds
true up to sizes in the order of millions of cells;

c. dynvectors are not immutable objects, or more specifically, they are not
recreated upon modification;

d. dynvectors have no limitation on the number of cells, apart from the
running platform's resource limitations;

e. dynvectors do not have a maximum number of cells specified at creation time -
elements may be freely inserted, updated, or deleted, as the dynvector
dynamically adjusts its upper bound as needed;

f. dynvectors demand no storage space reservation previous to the actual
cell-by-cell space allocation requests;

g. dynvectors are resource-minded; their cells are not required to have
values assigned to, in any particular sequence or fashion;

h. in order to avoid resource wastage, dynvectors should be explicitly
destroyed, upon ceasing to be of any further use.

@author GT Nunes
@version 1.0
@copyright (c) 2020 GT Nunes
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

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

%-------------------------------------------------------------------------------------

%! dynvector_create(+Id:atom) is semidet.
%
%  Create a dynvector.
%
%  @param Id Atom identifying the dynvector

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

%-------------------------------------------------------------------------------------

%! dynvector_destroy(+Id:atom)
%
%  Destroy dynvector Id, and release all of its resources. No action if
%  it does not exist.
%
%  @param Id Atom identifying the dynvector

dynvector_destroy(Id) :-

  retractall(dynvect_labels(Id, _, _)),
  retractall(dynvect_values(_, Id, _)).

%-------------------------------------------------------------------------------------

%! is_dynvector(+Id:atom) is semidet.
%
%  Fail if Id does not identify a dynvector.
%
%  @param Id Atom identifying the dynvector

is_dynvector(Id) :-
    dynvect_labels(Id, dv_top, _).

%-------------------------------------------------------------------------------------

%! dynvector_top(+Id:atom, -Top:int) is semidet.
%
%  Unify Top with the highest inserted index value in the dynvector, even if it has
%  subsequently been deleted. Upon dynvector's creation, this value is set to -1.
%
%  @param Id  Atom identifying the dynvector
%  @param Top Value of the highest index

dynvector_top(Id, Top) :-
    dynvect_labels(Id, dv_top, Top).

%-------------------------------------------------------------------------------------

%! dynvector_value(+Id:atom, +Index:int, ?Value:data) is semidet.
%
%  Unify Value with the value of the dynvector cell at Index. Dynvectors
%  may be sparsed, i.e., they may have cells not holding values, but attempts to
%  retrieve the value of an empty cell will fail. Dynvector values are stored in
%  the dynamic predicate dynvect_vaLues(Position, Id, Value).
%
%  @param Id    Atom identifying the dynvector
%  @param Index The reference index, or a label standing for it
%  @param Value The dynvector cell value

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

%-------------------------------------------------------------------------------------

%!  dynvector_label(+Id:atom, +Label:atom, ?Value:Data) is semidet.
%
%  Unify Value with the value associated with Label. This allows atoms
%  to stand for indices. Label values are stored in the dynamic predicate
%  dynvect_labels(Id, Label, Value). The following are the read-only private
%  labels in use:<br>
%    dv_top   - maximum index value in the dynvector<br>
%    dv_first - begin index for iterator<br>
%    dv_last  - end index for iterator<br>
%    dv_curr  - current index for iterator
%
%  @param Id        atom identifying the dynvector
%  @param Label     atom standing for the named attribute
%  @param Value     associated with the named attribute

dynvector_label(Id, Label, Value) :-

    (ground(Value) ->
        % fail point (must be an atom, and must not start with dv_)
        \+ sub_atom(Label, 0, 3, _, dv_),
        (retract(dynvect_labels(Id, Label, _)) ; true),
        assertz(dynvect_labels(Id, Label, Value))
    ;
        dynvect_labels(Id, Label, Value)
    ).

%-------------------------------------------------------------------------------------

%! dynvector_find(+Id:atom, ?Index:int, ?Value:data) is semidet.
%
%  Unify Index or Value with an occurrence of Index or Value in the
%  dynvector, respectively. Fail if no such value or index exist.
%
%  @param Id    Atom identifying the dynvector
%  @param Index The reference index
%  @param Value The reference value

dynvector_find(Id, Index, Value) :-
    dynvect_values(Index, Id, Value).

%-------------------------------------------------------------------------------------

%! dynvector_append(+Id:atom, +Value:data, -Index:int) is det.
%
%  Append the given Value to the dynvector, and unify Index with the
%  appension position. Value may be scalar, a list, or another dynvector.
%
%  @param Id    Atom identifying the dynvector
%  @param Value Value or list of of values to append
%  @param Index Index identifying the element holding Value or its first element

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

%-------------------------------------------------------------------------------------

%! dynvector_insert(+Id:atom, +Index:int, +Value:data)
%
%  Insert Value into the dynvector at Index. Value may be scalar, a list,
%  or another dynvector.
%
%  @param Id    Atom identifying the dynvector
%  @param Index The insertion point
%  @param Value Value or list of of values to insert

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

%-------------------------------------------------------------------------------------

%! dynvector_delete(+Id:atom, +Index) is semidet.
%
%  Erase the dynvector cell at Index, releasing the storage space taken.
%  Fail if no such cell exists.
%
%  @param Id    Atom identifying the dynvector
%  @param Index The reference index, or a label standing for it

dynvector_delete(Id, Index) :-

    % determine the element's Index from Label, if necessary
    ( (integer(Index) , Inx = Index)
    ; dynvect_labels(Id, Index, Inx) ),

    % erase the cell
    !,
    % fail point (cell might already be empty)
    retract(dynvect_values(Inx, Id, _)).

%-------------------------------------------------------------------------------------

%! dynvector_list(+Id:atom, ?List:list) is det.
%
%  Unify the cells of the dynvector with the values in List. A dynvector to hold
%  all the list elements may be created. Note that this is not a serialization a
%  mechanism, and as such it should not be used for backup/restore purposes.
%
%  @param Id   Atom identifying the dynvector
%  @param List List of values to unify the dynvector cells with

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

% list_to_dynvector_(+List:list, +Id:atom, +Index:int) is det.
%
%  @param List  The value to unify the dynvector cell with
%  @param Id    Atom identifying the dynvector
%  @param Index Index identifying the dynvector cell

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

%-------------------------------------------------------------------------------------

%! dynvector_fill(+Id:atom, +Value:data) is det.
%
%  Unify all of the cells of the dynvector with Value.
%
%  @param Id    Atom identifying the dynvector
%  @param Value Value to unify the dynvector cells with

dynvector_fill(Id, Value) :-

    retractall(dynvect_values(_, Id, _)),
    dynvect_labels(Id, dv_top, Count),
    dynvector_fill_(Id, Value, 0, Count).

% dynvector_fill_(+Id:atom, +Value:data, +Index:int, +Count:int) is det.
%
% @param Id    Atom identifying the dynvector
% @param Value Value to unify the dynvector cell with
% @param Index 0-based index identifying the dynvector cell
% @param Count Nnumber of cells in dynvector

% (done)
dynvector_fill_(_Id, _Value, Count, Count).

% (iterate)
dynvector_fill_(Id, Value, Index, Count) :-

    % load Value into the cell at Index
    assertz(dynvect_values(Index, Id, Value)),

    % go for thew next index
    IndexNext is Index + 1,
    dynvector_fill_(Id, Value, IndexNext, Count).

%-------------------------------------------------------------------------------------

%! dynvector_iterator_create(+Id:atom) is semidet.
%
%  Create iterator with range from `0` to Top.
%
%  @param Id Atom identifying the dynvector

dynvector_iterator_create(Id) :-

    dynvect_labels(Id, dv_top, Top),
    dynvector_iterator_create(Id, 0, Top).

%! dynvector_iterator_create(+Id:atom, +From:int) is semidet.
%
%  Create iterator with range from From to Top.
%
%  @param Id   Atom identifying the dynvector
%  @param From The iterator's first index

dynvector_iterator_create(Id, From) :-

    dynvect_labels(Id, dv_top, Top),
    dynvector_iterator_create(Id, From, Top).

%! dynvector_iterator_create(+Id:atom, +From:int, +To:int) is semidet.
%
%  Create iterator with range from From to To. Initial and final range
%  positions must be consistent with the dynvector state. Fail if the dynvector
%  already has an active iterator.

%  @param Id   Atom identifying the dynvector
%  @param From The iterator's first index
%  @param To   The iterator's last index

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

%-------------------------------------------------------------------------------------

%! dynvector_iterator_destroy(+Id:atom) is semidet.
%
%  Destroy the dynvector's iterator. Fail if dynvector Id does not exist.
%  No action if dynvector does not have an active iterator.
%
%  @param Id Atom identifying the dynvector

dynvector_iterator_destroy(Id) :-

    retract(dynvect_labels(Id, dv_first, _)),
    assertz(dynvect_labels(Id, dv_first, -1)),
    retract(dynvect_labels(Id, dv_last, _)),
    assertz(dynvect_labels(Id, dv_last, -1)),
    retract(dynvect_labels(Id, dv_curr, _)),
    assertz(dynvect_labels(Id, dv_curr, -1)).

%-------------------------------------------------------------------------------------

%! dynvector_iterator_next(+Id:atom, ?Value:data) is semidet.
%
%  Move the itrator to the next position, and unify Value with the value therein.
%  Fail if dynvector does not have an active iterator, or if a next position is not
%  possible.
%
%  @param Id    Atom identifying the dynvector
%  @param Value Value to unify the iterator's next position with

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

%-------------------------------------------------------------------------------------

%! dynvector_iterator_prev(+Id:atom, ?Value:data) is semidet.
%
%  Move the iterator to the previous position, and unify Value with the value
%  therein. Fail if dynvector does not have an active iterator, or if a previous
%  position is not possible.
%
%  @param Id    Atom identifying the dynvector
%  @param Value Value to unify the iterator's previous position with

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

%-------------------------------------------------------------------------------------

%! dynvector_iterator_first(+Id:atom, ?Value:data) is semidet.
%
%  Move the iterator to the first position, and unify Value with the value
%  therein. Fail if dynvector does not have an active iterator.

%  @param Id    Atom identifying the dynvector
%  @param Value Value to unify the iterator's first position with

dynvector_iterator_first(Id, Value) :-

    % obtain iterator's first index
    dynvect_labels(Id, dv_first, First),
    % fail point (iterator must be active)
    First > -1,

    % unify Value with the value at First
    !,
    % fail point
    dynvector_iterator_nav_(Id, First, Value).

%-------------------------------------------------------------------------------------

%! dynvector_iterator_last(+Id:atom, ?Value:data) is semidet.
%
%  Move the iterator to the last position, and unify Value with the value
%  therein. Fail if dynvector does not have an active iterator.

%  @param Id    Atom identifying the dynvector
%  @param Value Value to unify the iterator's last position with

dynvector_iterator_last(Id, Value) :-

    % obtain iterator's last index
    dynvect_labels(Id, dv_last, Last),
    % fail point (iterator must be active)
    Last > -1,

    % unify Value with the value at Last
    !,
    % fail point
    dynvector_iterator_nav_(Id, Last, Value).

% dynvector_iterator_nav_(+Id:atom, ?Value:data) is det.
%
% Unify Value with the value at Index.
%
% @param Id    Atom identifying the dynvector
% @param Value Value to unify the iterator's given position with

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

%-------------------------------------------------------------------------------------

%! dynvector_iterator_current(+Id:atom, ?Value) is semidet.
%
%  Unify Value with the value at iterator's current position. Fail if
%  dynvector does not have an active iterator.
%
%  @param Id    Atom identifying the dynvector
%  @param Value Value to unify the iterator's current position with

dynvector_iterator_current(Id, Value) :-

    % obtain iterator's current index
    dynvect_labels(Id, dv_curr, Current),
    % fail point (iterator must be active)
    Current > -1,

    % unify Value with the value at Current
    !,
    % fail point
    dynvector_iterator_nav_(Id, Current, Value).

%-------------------------------------------------------------------------------------

%! dynvector_iterator_delete(+Id:atom) is semidet.
%
%  Erase value at iterator's current position. Fail if dynvector does not have
%  an active iterator.
%
%  @param Id Atom identifying the dynvector

dynvector_iterator_delete(Id) :-

    % obtain iterator's current index
    dynvect_labels(Id, dv_curr, Current),
    % fail point (iterator must be active)
    Current > -1,

    % remove value
    (retract(dynvect_values(Current, Id, _)) ; true).

%-------------------------------------------------------------------------------------

%! dynvector_iterator_index(+Id:atom, -Index:int) is semidet.
%
%  Unify Index with iterator's current index.
%
%  @param Id    Atom identifying the dynvector
%  @param Index The iterator's current index

dynvector_iterator_index(Id, Index) :-

    % fail point (Index must be a var)
    var(Index),

    % obtain iterator's current index
    dynvect_labels(Id, dv_curr, Index).

%-------------------------------------------------------------------------------------

%! dynvector_iterator_insert(+Id:atom, ?Value:data) is semidet.
%
%  Insert Value at iterator's current position, and adjust the iterator's
%  range accordingly. Fail if dynvector does not have an active iterator.
%
%  @param Id    Atom identifying the dynvector
%  @param Value Value to be inserted

dynvector_iterator_insert(Id, Value) :-

    % obtain iterator's current position
    dynvect_labels(Id, dv_curr, Index),
    % fail point (iterator must be active)
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

%-------------------------------------------------------------------------------------

%! dynvector_iterator_append(+Id:atom, ?Value)
%
%  Insert Value after iterator's last position, and adjust the iterator's
%  range accordingly. Fail if dynvector does not have an active iterator.
%
%  @param Id    Atom identifying the dynvector
%  @param Value Value to be appended

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
