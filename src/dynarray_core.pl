:- module(dynarray_core,
    [
        dynarray_cells/2,
        dynarray_cells/3,
        dynarray_create/2,
        dynarray_delete/2,
        dynarray_destroy/1,
        dynarray_dims/2,
        dynarray_elements/2,
        dynarray_fill/2,
        dynarray_find/3,
        dynarray_label/3,
        dynarray_list/2,
        dynarray_top/3,
        dynarray_value/3,
        dynarray_position_delete/2,
        dynarray_position_find/3,
        dynarray_position_indices/3,
        dynarray_position_top/2,
        dynarray_position_value/3,
        is_dynarray/1
    ]).

/** <module> Dynamic, multi-dimensional arrays

This module provides an implementation of dynamic multi-dimensional arrays.
These are some of their noteworthy characteristics:
~~~
1. dynarrays are powerful, flexible, high-performance, hash-based
   multi-dimensional arrays;
2. dynarrays have O(1) read/insert/update/delete times, and this holds true
   up to sizes in the order of millions of cells;
3. dynarrays are not immutable objects, or more specifically, they are not
   recreated upon modification;
4. dynarrays have no limitation on the number of dimensions, nor any restriction
   on dimension sizes, apart from the running platform's resource limitations;
5. dynarrays have a maximum number of cells, defined at creation time and
   kept constant thereafter;
6. dynarrays demand no storage space reservation previous to the actual
   cell-by-cell space allocation requests;
7. dynarrays are resource-minded; their cells are not required to have values
   assigned to, in any particular sequence or fashion;
8. in order to avoid resource wastage, dynarrays should be explicitly destroyed,
   upon ceasing to be of any further use;
9. elements may be freely inserted, updated, or deleted, as long as their
   indices are within the dynarray's dimension bounds.
~~~

@author GT Nunes
@version 1.2
@copyright (c) TheWiseCoder 2020-2021
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- use_module(library(lists),
    [
        nth1/3,
        reverse/2
    ]).

:- dynamic  dynarr_dims/3,
            dynarr_factors/3,
            dynarr_labels/3,
            dynarr_offsets/3,
            dynarr_tops/3,
            dynarr_values/3.

:- volatile dynarr_dims/3,
            dynarr_factors/3,
            dynarr_labels/3,
            dynarr_offsets/3,
            dynarr_tops/3,
            dynarr_values/3.

%-------------------------------------------------------------------------------------

%! dynarray_create(+Id:atom, +DimRanges:list) is semidet.
%
%  Create a dynarray. Multi-dimensional dynarrays must be constructed with
%  dimension sizes as integers > 0, and in this case their indices are
%  0-based positive integers, smaller then the corresponding dimension size
%  (0 <= IndexI < DimSizeI).
%
%  Alternatively, a range of indices may be specified for any of its
%  dimensions, in the form of an integer pair `Ii:If`. These pairs may
%  contain negative values, and, within a single pair, the interval
%  markers may be expressed in any order. Internally, offsets compensate
%  for the fact that linear positions of cells start at 0 (zero).
%
%  These are examples of valid dynarray creation requests:
%  ~~~
%    dynarray_create(a, [9,5,8])      - indices ranges: [0 : 8,0 : 4,0 : 7]<br/>
%    dynarray_create(a, [3,5,3 : -8]) - indices ranges: [0 : 2,-8 : 3]<br/>
%    dynarray_create(a, [3,19 : 4])   - indices ranges: [0 : 2,4 : 19]<br/>
%    dynarray_create(a, [-4 : -3,7])  - indices ranges: [-4 : -3,0 : 6]
%  ~~~
%
%  @param Id        Atom identifying the dynarray
%  @param DimRanges List of dimension ranges in ascending dimension order

dynarray_create(Id, DimRanges) :-

    % fail point (make sure id is an atom)
    atom(Id),
    % fail point (make sure id is not taken)
    \+ is_dynarray(Id),

    % compute the dynarray's structure
    dynarray_dimensions(Id, DimRanges),
    dynarray_factors(Id, CellCount),

    % register the dynarray's sizes and creation ranges
    length(DimRanges, DimCount),
    assertz(dynarr_labels(Id, da_cells, CellCount)),
    assertz(dynarr_labels(Id, da_dims, DimCount)),
    assertz(dynarr_labels(Id, da_ranges, DimRanges)).

%-------------------------------------------------------------------------------------

%! dynarray_destroy(+Id:atom) is det.
%
%  Destroy dynarray Id, and release all of its resources. No action if
%  it does not exist.
%
%  @param Id atom identifying the dynarray

dynarray_destroy(Id) :-

  retractall(dynarr_dims(Id, _, _)),
  retractall(dynarr_factors(Id, _, _)),
  retractall(dynarr_labels(Id, _, _)),
  retractall(dynarr_offsets(Id, _, _)),
  retractall(dynarr_tops(Id, _, _)),
  retractall(dynarr_values(_, Id, _)).

%-------------------------------------------------------------------------------------

%! is_dynarray(+Id:atom) is semidet.
%
%  Fail if Id does not identify a dynarray.
%
%  @param Id Atom identifying the dynarray

is_dynarray(Id) :-
    dynarr_dims(Id, 0, _).

%-------------------------------------------------------------------------------------

%! dynarray_dims(+Id:atom, -DimCount:int) is det.
%
%  Unify DimCount with the number of dimensions in the dynarray.
%
%  @param Id       Atom identifying the dynarray
%  @param DimCount The number of dimensions in the dynarray

dynarray_dims(Id, DimCount) :-
    dynarr_labels(Id, da_dims, DimCount).

%-------------------------------------------------------------------------------------

%! dynarray_top(+Id:atom, +Dim:int, -Top:int) is semidet.
%
%  Unify Top with the highest inserted index on the dimension Dim. 
%  This holds true even if this highest index has subsequently been deleted.
%  Dimensions are 1-based integers, thus if Dim is specified as 0 (zero), unify
%  Top with the list of the highest indices for all dimensions, instead.
%  Upon dynarray's creation, this value is set to -1 for all dimensions.
%
%  @param Id  Atom identifying the dynarray
%  @param Dim 1-based dimension ordinal, or 0 for all top indices
%  @param Top Value of the highest index

dynarray_top(Id, Dim, Top) :-

    % fail points
    Dim >= 0,
    dynarr_tops(Id, Dim, Top).

%! dynarray_position_top(+Id:atom, -Top:int) is det.
%
%  Unify Top with the highest inserted 0-based linear position. This holds true
%  even if the element at this highest linear position has subsequently been deleted.
%  Unify Top with -1 If no element has been inserted.
%
%  @param Id  Atom identifying the dynarray
%  @param Top Value of the highest linear position

dynarray_position_top(Id, Top) :-

    % obtain the offset-adjusted list of the highest 0-based indices used
    dynarr_tops(Id, 0, Tops),
    dynarray_offset(Id, Tops, TopsOffset),

    % obtain the corresponding linear position
    dynarray_position_indices(Id, Top, TopsOffset).

%-------------------------------------------------------------------------------------

%! dynarray_cells(+Id:atom, -CellCount:int) is det.
%
%  Unify CellCount with the number of cells in the dynarray.
%
%  @param Id        Atom identifying the dynarray
%  @param CellCount The number of cells in the dynarray

dynarray_cells(Id, CellCount) :-
    dynarr_labels(Id, da_cells, CellCount).

%! dynarray_cells(+Id:atom, +Dim:int, -CellCount:int) is semidet.
%
%  Unify CellCount with the number of cells in the dimension Dim.
%  The cell values are stored in the dynamic predicate
%  `dynarr_dims(Id, DimI, DimSizeI)`. For the special instance `Dim0`,
%  this list of lists is stored:<br/>
%  [[DimSizeI,I],...,[DimSizeK,K]] (in ascending order by `DimSizeI`).
%
%  @param Id        Atom identifying the dynarray
%  @param Dim       The 1-based dynarray dimension
%  @param CellCount The number of cells in the given dimension

dynarray_cells(Id, Dim, CellCount) :-
    dynarr_dims(Id, Dim, CellCount).

%-------------------------------------------------------------------------------------

%! dynarray_elements(+Id:atom, -ElementsCount:int) is det.
%
%  Unify ElementsCount with the number of elements in the dynarray. This might be
%  a very costly operation, as the elements are counted by fully traversing the
%  dynarray space. 
%
%  @param Id            Atom identifying the dynarray
%  @param ElementsCount The number of elements in the dynarray

dynarray_elements(Id, ElementsCount) :-

    % obtain the highest 0-based linear position in use
    dynarray_position_top(Id, LastPosition),

    % count the elements, by traversing the dynarray space in reverse position order
    dynarray_elements_(Id, LastPosition, 0, ElementsCount).

% (done)
dynarray_elements_(_Id, -1, CountFinal, CountFinal).

% iterate
dynarray_elements_(Id, Position, CountProgress, CountFinal) :-

    % is there an element at this position ?
    (dynarray_position_value(Id, Position, _) ->
        % yes, so increment the count
        CountRevised is CountProgress + 1
    ;
        % no, so proceed
        CountRevised = CountProgress
    ),

    % go for the next position
    PositionNext is Position - 1,
    dynarray_elements_(Id, PositionNext, CountRevised, CountFinal).

%-------------------------------------------------------------------------------------

%! dynarray_value(+Id:atom, +Indices:list, ?Value:data) is semidet.
%
%  Unify Value with the value of the dynarray cell at Indices.
%
%  Dynarrays may be sparsed, i.e., they may have cells not holding values,
%  but attempts to  retrieve the value of an empty cell will fail.
%  Dynarray values are stored in the dynamic predicate
%  `dynarr_vaLues(Position, Id, Value)`.
%
%  @param Id      Atom identifying the dynarray
%  @param Indices Indices identifying the element
%  @param Value   The dynarray cell value

dynarray_value(Id, Indices, Value) :-

    % obtain corresponding linear position
    labels_indices(Id, Indices, Indexes),
    dynarray_position_indices(Id, Position, Indexes),

    % has Value been grounded ?
    (ground(Value) ->
       % yes, so register value and top indices
       dynarray_value_(Id, Indexes, Position, Value)
    ;
       % no, so retrieve value
       !,
       % fail point (cell might be empty)
       dynarr_values(Position, Id, Value)
   ).

%! dynarray_position_value(+Id:atom, +Position:int, ?Value:data) is semidet.
%
%  Unify Value with the value of the cell at Position.
%
%  The dynarray may be sparsed, i.e., it may have cell not holding values,
%  but attempts to retrieve  value of an empty cell will fail.
%
%  @param Id       Atom identifying the dynarray
%  @param Position Linear position identifying the cell
%  @param Value    The dynarray cell value

dynarray_position_value(Id, Position, Value) :-

    % has Value been grounded ?
    (ground(Value) ->
       % yes, so register value and top indices
       dynarray_position_indices(Id, Position, Indices),
       dynarray_value_(Id, Indices, Position, Value)
    ;
       % no, so retrieve value
       !,
       % fail point (cell might been empty)
       dynarr_values(Position, Id, Value)
   ).

%! dynarray_value_(+Id:atom, +Indices:list, +Position:int, +Value:data) is semidet.
%
%  @param Id       Atom identifying the dynarray
%  @param Indices  Indices identifying the element
%  @param Position Linear position identifying the element
%  @param Value    The dynarray element value

dynarray_value_(Id, Indices, Position, Value) :-

    % register value at position
    (retract(dynarr_values(Position, Id, _)) ; true),
    assertz(dynarr_values(Position, Id, Value)),

    % register top indices
    dynarray_tops_register(Id, Indices).

%-------------------------------------------------------------------------------------

%! dynarray_label(+Id:atom, +Label:atom, ?Value:data) is semidet.
%
%  Unify Value with the value associated with Label.
%  This allows atoms to stand for indices. Label values are stored in the
%  dynamic predicate `dynarr_labels(Id, Label, Value)`.
%
%  The following are the read-only private labels in use:
%  ~~~
%    da_cells  - number of cells in the dynarray
%    da_dims   - number of dimensions in the dynarray
%    da_ranges - dimension ranges data used at the dynarray's creation
%  ~~~
%
%  @param Id    Atom identifying the dynarray
%  @param Label Atom standing for the named attribute
%  @param Value Value associated with the named attribute

dynarray_label(Id, Label, Value) :-

    ((ground(Label) , ground(Value)) ->
        % fail point (must be an atom, and must not start with 'da_')
        \+ sub_atom(Label, 0, 3, _, da_),
        (retract(dynarr_labels(Id, Label, _)) ; true),
        assertz(dynarr_labels(Id, Label, Value))
    ;
        % fail point
        dynarr_labels(Id, Label, Value)
    ).

%-------------------------------------------------------------------------------------

%! dynarray_find(+Id:atom, +Indices:list, -Value:data) is semidet.
%! dynarray_find(+Id:atom, -Indices:list, +Value:data) is semidet.
%
%  Unify Indices or Value with an occurrence of Indices or Value in the
%  dynarray, respectively. Fail if no such value or indices exist.
%
%  @param Id      Atom identifying the dynarray
%  @param Indices The reference indices
%  @param Value   The reference value

dynarray_find(Id, Indices, Value) :-

    % are Indices fully specified ?
    (ground(Indices) ->
        % yes, so obtain linear position and retrieve value
        labels_indices(Id, Indices, Indexes),
        dynarray_position_indices(Id, Position, Indexes),
        dynarr_values(Position, Id, Value)
    ;
        % no, so obtain the Indices of the cell holding Value
        dynarr_values(Position, Id, Value),
        dynarray_position_indices(Id, Position, Indices)
    ).

%! dynarray_position_find(+Id:atom, +Position:int, -Value:data) is semidet.
%! dynarray_position_find(+Id:atom, -Position:int, +Value:data) is semidet.
%
%  Unify Position or Value with an occurrence of Position or Value in the
%  dynarray, respectively. Fail if no such value or position exists.
%
%  @param Id       atom identifying the dynarray
%  @param Position the reference linear position
%  @param Value    the reference value

dynarray_position_find(Id, Position, Value) :-
    dynarr_values(Position, Id, Value).

%-------------------------------------------------------------------------------------

%! dynarray_tops_register(+Id:atom, +Indices:list) is det.
%
%  Register maximum indices associated with non-empty cells.
%
%  The dynamic predicate `dynarr_tops(Id, DimI, DimTopI)` holds the
%  corresponding values for the 1-based dimensions. The special instance
%  `Dim0` holds the list `[DimTop1,...,DimTopN]` (top values for all dimensions).
%
%  @param Id       Atom identifying the dynarray
%  @param Indices  Indices identifying the element

dynarray_tops_register(Id, Indices) :-
    dynarray_tops_register_(Id, 1, Indices, []).

% (done)
dynarray_tops_register_(Id, _Dim, [], AllTops) :-

    % register top indices for all dimensions
    reverse(AllTops, DimsTops),
    retract(dynarr_tops(Id, 0, _)),
    assertz(dynarr_tops(Id, 0, DimsTops)).
    

% (iterate)
dynarray_tops_register_(Id, Dim, [Index|Indices], AllTops) :-

    % obtain current top index for dimension
    dynarr_tops(Id, Dim, Top),

    % update current top index for dimension, if applicable
    ( Top >= Index
    ; ( retract(dynarr_tops(Id, Dim, _))
      , assertz(dynarr_tops(Id, Dim, Index)) ) ),

    % go for the next index
    DimNext is Dim + 1,
    TopIndex is max(Top, Index),
    dynarray_tops_register_(Id, DimNext, Indices, [TopIndex|AllTops]).

%-------------------------------------------------------------------------------------

%! dynarray_delete(+Id:atom, +Indices:list) is semidet.
%
%  Erase the dynarray cell at Indices, releasing the storage space taken.
%  Fail if no such cell exists.
%
%  @param Id      Atom identifying the dynarray
%  @param Indices Indices identifying the cell

dynarray_delete(Id, Indices) :-

    % determine the element's linear position
    labels_indices(Id, Indices, Indexes),
    dynarray_position_indices(Id, Position, Indexes),

    % erase the cell
    !,
    % fail point (cell might already be empty)
    retract(dynarr_values(Position, Id, _)).

%! dynarray_position_delete(+Id:atom, +Position:int) is semidet.
%
%  Erase the dynarray cell at the given Position, releasing the storage
%  space taken. Fail if no such cell exists.
%
%  @param Id       Atom identifying the dynarray
%  @param Position Linear position identifying the cell

dynarray_position_delete(Id, Position) :-
    % fail point (cell might not exist)
    retract(dynarr_values(Position, Id, _)).

%-------------------------------------------------------------------------------------

%! dynarray_list(+Id:atom, ?List:list) is det.
%
%  Unify the cells of the dynarray with the values in List.
%  A 1-dimension dynarray sized to hold all the list elements may be created.
%  Note that this is not a serialization mechanism, and as such it should not
%  be used for backup/restore purposes.
%
%  @param Id   Atom identifying the dynarray
%  @param List List of values to unify the dynarray cells with

dynarray_list(Id, List) :-

    % HAZARD: ground(List) might be very expensive
    (var(List) ->
        % load all values in dynarray into List
        findall(Value, dynarr_values(_Position, Id, Value), List)
    ;
        ( (is_dynarray(Id) , retractall(dynarr_values(_, Id, _)))
        ; (length(List, Length) , dynarray_create(Id, [Length])) ),
        list_to_dynarray_(List, Id, 0)
    ).

%! list_to_dynarray_(+List:list, +Id:atom, +Position:int) is det.

%  @param Value    The value to unify the dynarray cell with
%  @param Id       Atom identifying the dynarray
%  @param Position Linear position identifying the dynarray cell

% (done)
list_to_dynarray_([], Id, Position) :-

    % register the top index for each dimension
    (Position = 0 ->
       dynarr_values(Id, da_dims, Dims),
       list_repeat(Dims, [-1], Indices)
    ;
       Pos is Position - 1,
       dynarray_position_indices(Id, Pos, Indices)
    ),
    dynarray_tops_register(Id, Indices).

% (iterate)
list_to_dynarray_([Value|List], Id, Position) :-

    assertz(dynarr_values(Position, Id, Value)),
    PosNext is Position + 1,
    list_to_dynarray_(List, Id, PosNext).

% (done)
list_repeat(1, ListFinal, ListFinal).

% (iterate)
list_repeat(Count, [Elem|ListProgress], ListFinal) :-

    CountNext is Count - 1,
    list_repeat(CountNext, [Elem|[Elem|[ListProgress]]], ListFinal).

%-------------------------------------------------------------------------------------

%! dynarray_fill(+Id:atom, +Value:data) is det.
%
%  Unify all the cells of the dynarray with Value.
%
%  @param Id    Atom identifying the dynarray
%  @param Value Value to unify the dynarray cells with

dynarray_fill(Id, Value) :-

    retractall(dynarr_values(_, Id, _)),
    dynarr_labels(Id, da_cells, CellCount),
    dynarray_fill_(Id, Value, 0, CellCount).

%! dynarray_fill_(+Id:atom, +Value, +Position:int) is det.
%
%  @param Id        Atom identifying the dynarray
%  @param Value     Value to unify the dynarray cell with
%  @param Position  0-based linear position identifying the dynarray cell
%  @param CellCount Number of cells in dynarray

% (done)
dynarray_fill_(_Id, _Value, CellCount, CellCount).

% (iterate)
dynarray_fill_(Id, Value, Position, CellCount) :-

    assertz(dynarr_values(Position, Id, Value)),
    PosNext is Position + 1,
    dynarray_fill_(Id, Value, PosNext, CellCount).

%-------------------------------------------------------------------------------------

%! dynarray_dimensions(+Id:atom, +DimRanges:list) is det.
%
%  Initialize the dynarray by ackowledging its dimensions.
%  The dynamic predicate `dynarr_offsets(Id, DimI, DimOffsetI)` holds the
%  offsets for the 1-based dynarray dimensions. The special instance `Dim0`
%  holds the list `[DimOffset1,...,DimOffsetN]` (offset values for all dimensions).
%
%  @param Id        Atom identifying the dynarray
%  @param DimRanges List holding the dynarray dimension ranges

dynarray_dimensions(Id, DimRanges) :-

    % register the dynarray dimension offsets, sizes, and top indices
    dynarray_dimensions_(Id, 1, DimRanges, [], [], []).

%! dynarray_dimensions_(+Id:atom, +Dim:int, +DimRanges:list, +DimOffsets:list, +DimTops:list, -DimsSizes:list) is det.
%
%  @param Id         Atom identifying the dynarray
%  @param Dim        The 1-based dynarray dimension
%  @param DimRanges  Range of indices of dimension Dim
%  @param DimTops    List of dimensions' top indices ([Top1,...,TopN])
%  @param DimOffsets Dimensions' indices offsets ([Offset1,...,OffsetN])
%  @param DimsSizes  List of dimensions and its sizes ([[DimSz1,1],...,[DimSzN,N]])

% (done)
dynarray_dimensions_(Id, _Dim, [], DimOffsets, DimTops, DimsSizes) :-
    
    % register the initial values for the dimensions' top indices
    assertz(dynarr_tops(Id, 0, DimTops)),

    % the dynarr_offsets 0 position will hold the dimension indices offsets
    reverse(DimOffsets, Offsets),
    assertz(dynarr_offsets(Id, 0, Offsets)),

    % the dynarr_dims 0 position will hold the dimension sizes list of lists:
    %   [[DimSizeI,I],...,[DimSizeK,K]] - ordered by dim_size
    sort(DimsSizes, DimsSorted),
    assertz(dynarr_dims(Id, 0, DimsSorted)).

% (iterate)
dynarray_dimensions_(Id, Dim, [DimRange|DimRanges],
                     DimOffsets, DimTops, DimsSizes) :-

    (Ii:If = DimRange ->
        % fail points
        integer(Ii),
        integer(If)
    ;
       % fail points
       integer(DimRange),
       DimRange > 0,

       Ii = 0,
       If is DimRange - 1
    ),

    % register the dimension's size information
    Size is abs(If - Ii) + 1,
    assertz(dynarr_dims(Id, Dim, Size)),

    % register the dimension's initial top index value
    assertz(dynarr_tops(Id, Dim, -1)),

    % register the dimension's index offset information
    Offset is min(Ii, If),
    assertz(dynarr_offsets(Id, Dim, Offset)),

    % go for the next dimension
    DimNext is Dim + 1,
    dynarray_dimensions_(Id, DimNext, DimRanges, [Offset|DimOffsets],
                         [-1|DimTops], [[Size,Dim]|DimsSizes]).

%-------------------------------------------------------------------------------------

%! dynarray_factors(+Id:atom, -CellCount) is det.
%
%  Obtain the number of cells in the dynarray, and its compound factors.
%  Compound factors are used for mapping between indices and linear positions.
%  The dynamic predicate `dynarr_factors(Id, DimI, DimFactorI)` holds the
%  factors for the 1-based dimensions. The special instance `Dim0` holds the
%  list `[DimFactor1,...,DimFactorN]` (the factor values for all dimensions).
%
%  These facts hold for a 4-dimension dynarray:
%  ~~~
%  (a)
%  DimSizeW <= DimSizeX <= DimSizeY <= DimSizeZ
%
%  (b)
%  FactorW = DimSizeX * DimSizeY * DimSizeZ
%  FactorX = DimSizeY * DimSizeZ
%  FactorY = DimSizeZ
%  FactorZ = 1
%
%  (c)
%  Indices (W,X,Y,Z) -> Linear position:
%  Pos = FactorW * W + FactorX * X + FactorY * Y + FactorZ * Z
%
%  (d)
%  Linear position -> Indices (W,X,Y,Z):
%  W    = div(Pos, FactorW)
%  RemW = mod(Pos, FactorW)
%  X    = div(RemW, FactorX)
%  RemX = mod(RemW, FactorX)
%  Y    = div(RemX, FactorY)
%  RemY = mod(RemX, FactorY)
%  Z    = div(RemY, FactorZ) -> FactorZ = 1, Z = RemY
%  ~~~
%
%  @param Id        Atom identifying the dynarray
%  @param CellCount Number of cells in dynarray

dynarray_factors(Id, CellCount) :-

    dynarr_dims(Id, 0, DimsSizes),
    length(DimsSizes, DimCount),
    dynarray_factors_(Id, DimCount, DimCount, 1,
                      DimsSizes, 1, CellCount, [], DimFactors),

    % the dynarr_factors 0 position holds the dimension factors:
    % [DimFactor1,...,DimFactorN]
    assertz(dynarr_factors(Id, 0, DimFactors)).

%! dynarray_factors_(+Id:atom, +DimOrdinal:int, +DimCount:int, +CompoundFactor:int, +DimsSizes:list, +CountProgress:list, -CountFinal:list, +FactorsProgress:list, -FactorsFinal:list) is det.
%
%  @param Id              Atom identifying the dynarray
%  @param DimOrdinal      Size-based position for dynarray dimension
%  @param DimCount        Number of dynarray dimensions
%  @param CompoundFactor  Current compound factor
%  @param DimsSizes       List of dimensions and its sizes
%  param CountProgress    Working number of dynarray cells
%  @param CountFinal      Final number of dynarray cells
%  @param FactorsProgress The working dimension factors
%  @param FactorsFinal    The final dimension factors

% (done)
dynarray_factors_(_Id, 0, _DimCount, _CompoundFactor, _DimsSizes,
                  CountFinal, CountFinal, FactorsProgress, FactorsFinal) :-
    sort(FactorsProgress, FactorsFinal).

% (iterate)
dynarray_factors_(Id, DimOrdinal, DimCount, CompoundFactor, DimsSizes,
                  CountProgress, CountFinal, FactorsProgress, FactorsFinal) :-

    nth1(DimOrdinal, DimsSizes, [DimSize,Dim]),
    (DimOrdinal = DimCount ->
       % factor for dimension with the largest size is 1
       DimFactor = 1
    ;
       % factor for current dimension is size of next larger dimension
       DimAdjusted is DimOrdinal + 1,
       nth1(DimAdjusted, DimsSizes, [DimFactor,_])
    ),

    CountRevised is DimSize * CountProgress,
    Factor is DimFactor * CompoundFactor,

    % register the dimension's index factor
    assertz(dynarr_factors(Id, Dim, Factor)),

    % go for the next dimension
    OrdinalNext is DimOrdinal - 1,
    dynarray_factors_(Id, OrdinalNext, DimCount, Factor,
                      DimsSizes, CountRevised, CountFinal,
                      [Factor|FactorsProgress], FactorsFinal).

%-------------------------------------------------------------------------------------

%! dynarray_position_indices(+Id:atom, +Position:int, -Indices:list) is semidet.
%! dynarray_position_indices(+Id:atom, -Position:int, +Indices:list) is semidet.
%
%  Unify Position or Indices with the corresponding Position or Indices,
%  respectively.
%
%  @param Id       Atom identifying the dynarray
%  @param Position The final 0-based linear position of the element
%  @param Indices  The element's indices (offset-corrected, if applicable)

dynarray_position_indices(Id, Position, Indices) :-

    (ground(Position) ->
        % fail point
        Position >= 0,
        dynarr_dims(Id, 0, DimsSizes),
        position_indices_1(Id, Position, DimsSizes, [], IndicesOffset),
        dynarray_offset(Id, Indices, IndicesOffset)
    ;
        labels_indices(Id, Indices, Indexes),
        dynarray_offset(Id, Indexes, IndicesOffset),
        dynarr_labels(Id, da_dims, DimCount),
        indices_position_(Id, IndicesOffset, DimCount, 0, Position)
    ).

%! indices_position_(+Id:atom, +Indices:list, +Dim:int, +PosProgress:int, -PosFinal:int) is semidet.
%
%  Obtain the element's linear position from its indices.</br>
%  Pos = Factor1 * I1 + Factor2 * I2 + ... + FactorN * In (FactorN = 1)
%
%  @param Id          Atom identifying the dynarray
%  @param Indices     The element's indices
%  @param Dim         The 1-based dynarray dimension
%  @param PosProgress The working linear position of the cell
%  @param PosFinal    The final linear position of the cell

% (done)
indices_position_(_Id, _Indices, 0, PosFinal, PosFinal).

% (iterate)
indices_position_(Id, Indices, Dim, PosProgress, PosFinal) :-

    % fail points
    nth1(Dim, Indices, Index),
    Index >= 0,
    dynarr_dims(Id, Dim, DimSize),
    Index < DimSize,

    dynarr_factors(Id, Dim, DimFactor),
    PosRevised is PosProgress + DimFactor * Index,
    DimNext is Dim - 1,
    indices_position_(Id, Indices, DimNext, PosRevised, PosFinal).

%! position_indices_1(+Id:atom, +Factor:int, +Dim:int, +IndicesProgress:list, -IndicesFinal:list) is semidet.
%
%  Obtain the element's indices from its linear position.
%  ~~~
%  Size1 <= Size2 <= ... <= SizeN
%
%  I1     = div(Pos, Factor1)
%  Rem1   = mod(Pos, Factor1)
%  I2     = div(Rem1, Factor2)
%  Rem2   = mod(Rem1, Factor2)
%  :                :
%  :                :
%  In-1   = div(RemN-2, FactorN-1)
%  RemN-1 = mod(RemN-2, FactorN-1)
%  In     = div(RemN-1, FactorN) -> In = RemN-1
%  ~~~
%
% @param Id              Atom identifying the dynarray
% @param Position        The element's 0-based linear position
% @param DimsSizes       The dimensions and their corresponding sizes
% @param IndicesProgress The working indices of the element
% @param IndicesFinal    The final indices of the element

% (done)
position_indices_1(_Id, _Pos, [], IndicesProgress, IndicesFinal) :-

    % IndicesProgress is [[DimI,IndexI],...[DimK,IndexK]], unsorted
    % IndicesSorted is [[Dim1,Index1],...[DimN,IndexN]], ascending order by Dim
    sort(IndicesProgress, IndicesSorted),
 
    % IndicesFinal has the indices in proper order: [Index1,...,IndexN]
    position_indices_2(IndicesSorted, [], IndicesFinal).

% (iterate)
position_indices_1(Id, Position, [[_,Dim]|DimsSizes],
                   IndicesProgress, IndicesFinal) :-

    dynarr_factors(Id, Dim, DimFactor),
    Index is div(Position, DimFactor),
    Remainder is mod(Position, DimFactor),
    position_indices_1(Id, Remainder, DimsSizes,
                       [[Dim,Index]|IndicesProgress], IndicesFinal).

% morph list of lists with dimensions and indices in the format
%   [[Dim1,Index1],...,[DimN,IndexN]]
% into a simple list of indices in the format
%   [Index1,...,IndexN]

% (done)
position_indices_2([], IndicesProgress, IndicesFinal) :-
    reverse(IndicesProgress, IndicesFinal).

% (iterate)
position_indices_2([[_,Index]|DimsIndices], IndicesProgress, IndicesFinal) :-
    position_indices_2(DimsIndices, [Index|IndicesProgress], IndicesFinal).

%-------------------------------------------------------------------------------------

%! dynarray_offset(+Id:atom, +Indices:list, -OffsetIndices:list) is det.
%! dynarray_offset(+Id:atom, -Indices:list, +OffsetIndices:list) is det.
%
%  Unify Indices and OffsetIndices with the corresponding real indices
%  and offset indices, respectively.
%
%  @param Id            Atom identifying the dynarray
%  @param Indices       The dynarray cell's indices
%  @param OffsetIndices The dynarray cell's offset indices

dynarray_offset(Id, Indices, OffsetIndices) :-

    (ground(Indices) ->
        indices_offsets_(Id, 1, Indices, [], OffsetIndices)
    ;
        offsets_indices_(Id, 1, OffsetIndices, [], Indices)
    ).

%! indices_offsets_(+Id:atom, +Dim:int, +Indices:list, +OffsetsProgress:list, -OffsetsFinal:list) is det.
%
%  Convert real indices into offset indices.
%
%  @param Id              Atom identifying the dynarray
%  @param Dim             The current dynarray dimension
%  @param Indices         The dynarray cell's real indices
%  @param OffsetsProgress The working dynarray cell's offset indices
%  @param OffsetsFinal    The final dynarray cell's offset indices

% (done)
indices_offsets_(_Id, _Dim, [], OffsetsProgress, OffsetsFinal) :-
    reverse(OffsetsProgress, OffsetsFinal).

% (iterate)
indices_offsets_(Id, Dim, [Index|Indices], OffsetsProgress, OffsetsFinal) :-

    % adjust index with the offset for the given dim
    dynarr_offsets(Id, Dim, Offset),
    OffsetIndex is Index - Offset,

    % go for next dim
    DimNext is Dim + 1,
    indices_offsets_(Id, DimNext, Indices,
                     [OffsetIndex|OffsetsProgress], OffsetsFinal).

%! offsets_indices_(+Id:atom, +Dim:int, +OffsetIndices:list, +IndicesProgress:list, -IndicesFinal:list) is det.
%
%  Convert offset indices into real indices.
%
%  @param Id              Atom identifying the dynarray
%  @param Dim             The current dynarray dimension
%  @param OffsetIndices   The dynarray cell's offset indices
%  @param IndicesProgress The working dynarray cell's real indices
%  @param IndicesFinal    The final dynarray cell's real indices

% (done)
offsets_indices_(_Id, _Dim, [], IndicesProgress, IndicesFinal) :-
    reverse(IndicesProgress, IndicesFinal).

% (iterate)
offsets_indices_(Id, Dim, [OffsetIndex|OffsetIndices],
                 IndicesProgress, IndicesFinal) :-

    % adjust index with the offset for the given dim
    dynarr_offsets(Id, Dim, Offset),
    Index is OffsetIndex + Offset,

    % go for the next dim
    DimNext is Dim + 1,
    offsets_indices_(Id, DimNext, OffsetIndices,
                     [Index|IndicesProgress], IndicesFinal).

%-------------------------------------------------------------------------------------

%! labels_indices(+Id:atom, +Labels:list, -Indices:list) is semidet.
%
%  Unify Indices with a list of integers obtained from Labels.
%
%  @param Id      Atom identifying the dynarray
%  @param Labels  List of indices possibly containing atoms
%  @param Indices List with corresponding integer values
 
labels_indices(Id, Labels, Indices) :-
    labels_indices_(Id, Labels, [], Indices).

% (done)
labels_indices_(_Id, [], IndicesProgress, IndicesFinal) :-
    reverse(IndicesProgress, IndicesFinal).

% (iterate)
labels_indices_(Id, [Label|Labels], IndicesProgress, IndicesFinal) :-

    (atom(Label) ->
        dynarr_labels(Id, Label, Index)
    ;
        Index = Label
    ),

    % go for the next label
    labels_indices_(Id, Labels, [Index|IndicesProgress], IndicesFinal).
