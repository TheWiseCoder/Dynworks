/*******************************************************************************
* FILENAME / MODULE : dynarray_core.pl / dynarray_core
*
* DESCRIPTION :
*       This module provides an implementation of dynarrays. These are their
*       noteworthy characteristics:
*         1. dynarrays are powerful, flexible, high-performance, hash-based
*            multi-dimensional arrays
*         2. dynarrays have O(1) read/insert/update/delete times, and this
*            holds true up to sizes in the order of millions of cells
*         3. dynarrays are not immutable objects, or more specifically,
*            they are not recreated upon modification
*         4. dynarrays have no limitation on the number of dimensions, nor
*            any restriction on dimension sizes, apart from the running
*            platform's resource limitations
*         5. dynarrays have a maximum number of cells, defined at creation
*            time and kept constant thereafter
*         6. dynarrays demand no storage space reservation previous to the
*            actual cell-by-cell space allocation requests
*         7. dynarrays are resource-minded; their cells are not required to
*            have values assigned to, in any particular sequence or fashion
*         8. in order to avoid resource wastage, dynarrays should be
*            explicitly destroyed, upon ceasing to be of any further use
*
* PUBLIC PREDICATES :
*       dynarray_cells(+Id, -CellCount)
*       dynarray_cells(+Id, +Dim, -CellCount)
*       dynarray_create(+Id, +DimRanges)
*       dynarray_delete(+Id, +Indices)
*       dynarray_destroy(+Id)
*       dynarray_dims(+Id, -DimCount)
*       dynarray_fill(+Id, +Value)
*       dynarray_find(+Id, ?Value, ?Indices)
*       dynarray_label(+Id, ?Label, ?Value)
*       dynarray_list(+Id, ?List)
*       dynarray_top(+Id, +Dim, -Top)
*       dynarray_value(+Id, +Indices, ?Value)
*
*       dynarray_position_delete(+Id, +Position)
*       dynarray_position_find(+Id, ?Value, ?Position)
*       dynarray_position_indices(+Id, ?Position, ?Indices)
*       dynarray_position_value(+Id, +Position, ?Value)
*
*       is_dynarray(+Id)
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
* 2020-07-25  GT Nunes          Module creation
*
*******************************************************************************/

:- module(dynarray_core,
    [
        dynarray_cells/2,
        dynarray_cells/3,
        dynarray_create/2,
        dynarray_delete/2,
        dynarray_destroy/1,
        dynarray_dims/2,
        dynarray_fill/2,
        dynarray_find/3,
        dynarray_label/3,
        dynarray_list/2,
        dynarray_top/3,
        dynarray_value/3,
        dynarray_position_delete/2,
        dynarray_position_find/3,
        dynarray_position_indices/3,
        dynarray_position_value/3,
        is_dynarray/1
    ]).

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

%-------------------------------------------------------------------------------
% Powerful, flexible, high-performance, hash-based, multi-dimensional dynarrays.
% Notes:
%
%   1. multi-dimensional dynarrays must be constructed with dimension sizes
%      as integers > 0, and in this case their indices are 0-based positive
%      integers, smaller then the corresponding dimension size
%      (0 <= IndexI < DimSizeI)
%
%   2. alternatively, a range of indices may be specified for any of its
%      dimensions, in the form of an integer pair Ii:If. These pairs may
%      contain negative values, and, within a single pair, the interval
%      markers may be expressed in any order. These are examples of valid
%      dynarray creation requests:
%      a. dynarray_create(a, [9,5,8])      - indices ranges: [0 : 8,0 : 4,0 : 7]
%      b. dynarray_create(a, [3,5,3 : -8]) - indices ranges: [0 : 2,-8 : 3]
%      c. dynarray_create(a, [3,19 : 4])   - indices ranges: [0 : 2,4 : 19]
%      d. dynarray_create(a, [-4 : -3,7])  - indices ranges: [-4 : -3,0 : 6]
%
%   3. the dynarray may be sparsed, i.e., none of its cells must have been
%      unified with a value, but dynarray_value/3 will fail on an attempt
%      to retrieve the value of a non-grounded cell
%
%   4. atoms standing for indices may be specified with dynarray_label/3,
%      for subsequent use with dynarray_value/3. For example:
%      a. create the label 'path' to stand for index 0
%         dynarray_label(Id, path, 0),
%      b. now, value at indices [3,0] may be accessed as
%         dynarray_value(Id, [3,path], Value)
%
%   5. elements may be freely inserted, updated, or deleted, as long as their
%      indices are within the dynarray's dimension bounds
%
%   6. the following are the read-only private labels in use, whose values
%      are retrievable with dynarray_label/3:
%        da_cells  - number of cells in the dynarray
%        da_dims   - number of dimensions in the dynarray
%        da_ranges - dimension ranges data used at the dynarray's creation
%
%   7. the following dynamic predicates are privately used for internal
%      operations, such as indices to linear position mapping:
%
%      a.1. dynarr_dims(Id, 0, DimsSizes) - DimsSizes structure:
%           [[DimSizeI,I],...,[DimSizeK,K]] (in ascending order by DimSize)
%        2. dynarr_dims(Id, i, DimSizeI) -
%           DimSizeI = size of dimension i (i > 0)
%
%      b.1. dynarr_factors(Id, 0, DimsFactors) - DimsFactors structure:
%           [DimFactor1,...,DimFactorN]
%        2. dynarr_factors(Id, i, DimFactorI) -
%           DimFactorI = factor for dimension i (i > 0)
%
%      c.1. dynarr_offsets(Id, 0, DimsOffsets) - DimsOffsets structure:
%           [DimOffset1,...,DimOffsetN]
%        2. dynarr_offsets(Id, i, DimOffsetI)
%           DimOffsetI = offset index value for dimension i (i > 0)
%
%      d.1. dynarr_tops(Id, 0, DimsTops) - DimsTops structure:
%           [DimTop1,...,DimTopN]
%        2. dynarr_tops(Id, i, DimTopI)
%           DimTopI = highest index value for dimension i (i > 0)
%
%      e.   dynarr_vaLues(Position, Id, Value)
%           stores the elements of the dynarray

% create a dynarray
% dynarray_create(+Id, +DimRanges)
% Id            atom identifying the dynarray
% DimRanges     list of dimension ranges in ascending dimension order
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

% destroy a dynarray, and release all of its resources (no fail)
% dynarray_destroy(+Id)
% Id    atom identifying the dynarray
dynarray_destroy(Id) :-

  retractall(dynarr_dims(Id, _, _)),
  retractall(dynarr_factors(Id, _, _)),
  retractall(dynarr_labels(Id, _, _)),
  retractall(dynarr_offsets(Id, _, _)),
  retractall(dynarr_tops(Id, _, _)),
  retractall(dynarr_values(_, Id, _)).

% Unify Top with the highest inserted index value on the given dimension, 
% even if it has subsequently been deleted. Upon dynarray's creation,
% this highest value is set to -1 for all dimensions.
% dynarray_top(+Id, +Dim, -Top)
% Id    atom identifying the dynarray
% Dim   1-based dimension ordinal, or 0 for all top indices
% Top   value of the highest index
dynarray_top(Id, Dim, Top) :-

    % Dim may be represented by a registered atom
    %   holding the dimension's 0-based value
    ( ( atom(Dim) ,
        ( dynarray_label(Id, Dim, Dim0)
        , Dim1 is Dim0 + 1) )
    ; Dim1 = Dim),
    dynarr_tops(Id, Dim1, Top).

% unify CellCount with the number of cell positions in the dynarray
% dynarray_cells(+Id, -CellCount)
% Id            atom identifying the dynarray
% CellCount     the number of cells in the dynarray
dynarray_cells(Id, CellCount) :-
    dynarr_labels(Id, da_cells, CellCount).

% unify CellCount with the number of cells in the given dimension
% Id            atom identifying the dynarray
% Dim           the 1-based dynarray dimension
% CellCount     the number of cells in the given dimension
% dynarray_cells(+Id, +Dim, -CellCount)
dynarray_cells(Id, Dim, CellCount) :-
    dynarr_dims(Id, Dim, CellCount).

% obtain the number of dimensions in the dynarray
% Id            atom identifying the dynarray
% DimCount      the number of dimensions in the dynarray
dynarray_dims(Id, DimCount) :-
    dynarr_labels(Id, da_dims, DimCount).

% fail if Id does not identify a dynarray
% is_dynarray(+Id)
% Id    atom identifying the dynarray
is_dynarray(Id) :-
    dynarr_dims(Id, 0, _).

% unify Value with the value associated with the named attribute
% Id        atom identifying the dynarray
% Label     atom standing for the named attribute
% Value     value associated with the named attribute
% dynarray_label(+Id, ?Label, ?Value)
dynarray_label(Id, Label, Value) :-

    ((ground(Label) , ground(Value)) ->
        % fail point (must be an atom, and must not start with 'da_')
        \+ sub_atom(Label, 0, 3, _, da_),
        (retract(dynarr_labels(Id, Label, _)) ; true),
        assertz(dynarr_labels(Id, Label, Value))
    ;
        % fail if Label and Value are both vars
        dynarr_labels(Id, Label, Value)
    ).

% Unify Value or Indices with an occurrence of Value or Indices in the
% dynarray, respectively. Fail if no such value or index position exists.
% dynarray_find(+Id, ?Value, ?Indices)
% Id        atom identifying the dynarray
% Value     the reference value
% Indices   the reference indices
dynarray_find(Id, Value, Indices) :-

    (ground(Indices) ->
        labels_indices(Id, Indices, Indexes),
        dynarray_position_indices(Id, Position, Indexes),
        dynarr_values(Position, Id, Value)
    ;
        dynarr_values(Position, Id, Value),
        dynarray_position_indices(Id, Position, Indices)
    ).

% Unify Value or Position with an occurrence of Position or Value in the
% dynarray, respectively. Fail if no such value exists or position exists.
% dynarray_find(+Id, ?Value, ?Position)
% Id        atom identifying the dynarray
% Value     the reference value
% Position  the reference linear position
dynarray_position_find(Id, Value, Position) :-
    dynarr_values(Position, Id, Value).

%-------------------------------------------------------------------------------
% Unify the specified dynarray element with its value. Fail on attempt
% to retrieve the value of an empty cell.

% dynarray_value(+Id, +Indices, ?Value)
% Id            atom identifying the dynarray
% Indices       indices identifying the element
% Value         the dynarray element value
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

% dynarray_position_value(+Id, +Position, ?Value)
% Id            atom identifying the dynarray
% Position      linear position identifying the element
% Value         the dynarray element value
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

% dynarray_value_(+Id, +Indices, +Position, +Value)
% Id        atom identifying the dynarray
% Indices   indices identifying the element
% Position  linear position identifying the element
% Value     the dynarray element value
dynarray_value_(Id, Indices, Position, Value) :-

    % register value at position
    (retract(dynarr_values(Position, Id, _)) ; true),
    assertz(dynarr_values(Position, Id, Value)),

    % register top indices
    dynarray_tops_register(Id, 1, Indices, []).

% register maximum indices associated with non-empty cells
% dynarray_tops_register(+Id, +Dim, +[Index|Indices], +AllTops)
% Id        atom identifying the dynarray
% Indices   indices identifying the element
% Position  linear position identifying the element
% Value     the dynarray element value

% (done)
dynarray_tops_register(Id, _Dim, [], AllTops) :-

    % register top indices for all dimensions
    reverse(AllTops, DimsTops),
    retract(dynarr_tops(Id, 0, _)),
    assertz(dynarr_tops(Id, 0, DimsTops)).
    

% (iterate)
dynarray_tops_register(Id, Dim, [Index|Indices], AllTops) :-

    % obtain current top index for dimension
    dynarr_tops(Id, Dim, Top),

    % update current top index for dimension, if applicable
    ( Top >= Index
    ; ( retract(dynarr_tops(Id, Dim, _))
      , assertz(dynarr_tops(Id, Dim, Index)) ) ),

    % go for the next index
    DimNext is Dim + 1,
    TopIndex is max(Top, Index),
    dynarray_tops_register(Id, DimNext, Indices, [TopIndex|AllTops]).

%-------------------------------------------------------------------------------
% Erase the dynarray cell at the given Indices or Position, releasing the
% storage space taken. Fail if no such element exists.

% dynarray_delete(+Id, +Indices)
% Id        atom identifying the dynarray
% Indices   Indices identifying the element
dynarray_delete(Id, Indices) :-

    % determine the element's linear position
    labels_indices(Id, Indices, Indexes),
    dynarray_position_indices(Id, Position, Indexes),

    % erase the cell
    !,
    % fail point (cell might already be empty)
    retract(dynarr_values(Position, Id, _)).

% dynarray_position_delete(+Id, +Position)
% Id        atom identifying the dynarray
% Position  linear position identifying the element
dynarray_position_delete(Id, Position) :-
    % fail point (cell might not exist)
    retract(dynarr_values(Position, Id, _)).

%-------------------------------------------------------------------------------
% Unify all the cells of the dynarray with the given list. A 1-dimension
% dynarray able to hold all the list elements may be created. Note that this
% is not a serialization mechanism, and as such it should not be used for
% backup/restore purposes.

% dynarray_list(+Id, ?List)
% Id            atom identifying the dynarray
% List          list to dump the dynarray cells to
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

% list_to_dynarray_(+[Value|List], +Id, +Position)
% [Value|List]  the value to unify the dynarray cell with
% Id            atom identifying the dynarray
% Position      linear position identifying the dynarray cell

% (done)
list_to_dynarray_([], Id, Position) :-

    % register the top index for each dimension
    Pos is Position - 1,
    ( ( Pos = -1
      , dynarr_values(Id, da_dims, Dims)
      , list_repeat(Dims, [-1], Indices) )
    ; dynarray_position_indices(Id, Pos, Indices) ),
    dynarray_tops_register(Id, 1, Indices, []).

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

%-------------------------------------------------------------------------------
% unify all of the cells of the dynarray with the given value

% dynarray_fill(+Id, +Value)
% Id            atom identifying the dynarray
% Value         value to unify the dynarray cells with
dynarray_fill(Id, Value) :-

    retractall(dynarr_values(_, Id, _)),
    dynarr_labels(Id, da_cells, CellCount),
    dynarray_fill_(Id, Value, 0, CellCount).

% dynarray_fill_(+Id, +Value, +Position)
% Id            atom identifying the dynarray
% Value         value to unify the dynarray cell with
% Position      0-based linear position identifying the dynarray cell
% CellCount     number of cells in dynarray

% (done)
dynarray_fill_(_Id, _Value, CellCount, CellCount).

% (iterate)
dynarray_fill_(Id, Value, Position, CellCount) :-

    assertz(dynarr_values(Position, Id, Value)),
    PosNext is Position + 1,
    dynarray_fill_(Id, Value, PosNext, CellCount).

%-------------------------------------------------------------------------------
% initialize the dynarray by ackowledging its dimensions
% and registering their relevant data

% dynarray_dimensions(+Id, +DimRanges)
% Id            atom identifying the dynarray
% DimRanges     list holding the dynarray dimension ranges
dynarray_dimensions(Id, DimRanges) :-

    % register the dynarray dimension offsets, sizes, and top indices
    dynarray_dimensions_(Id, 1, DimRanges, [], [], []).

% dynarray_dimensions_(+Id, +Dim, +[DimRange|DimRanges],
%                      +DimOffsets, +DimTops, -DimsSizes)
% Id                    atom identifying the dynarray
% Dim                   the 1-based dynarray dimension
% [DimRange|DimRanges]  range of indices of dimension Dim
% DimTops               list of dimensions' top indices ([Top1,...,TopN])
% DimOffsets            dimensions' indices offsets ([Offset1,...,OffsetN])
% DimsSizes             list of dimensions and its sizes
%                       ([[DimSize1,1],...,[DimSizeN,N]])

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

%-------------------------------------------------------------------------------
% Obtain the number of cells in the dynarray and its compound factors.
% Compound factors are used for mapping between indices and linear positions.
% These facts hold for a 4-dimension dynarray:
%
%   1. DimSizeW <= DimSizeX <= DimSizeY <= DimSizeZ
%
%   2. FactorW = DimSizeX * DimSizeY * DimSizeZ
%      FactorX = DimSizeY * DimSizeZ
%      FactorY = DimSizeZ
%      FactorZ = 1
%
%   3. Indices (W,X,Y,Z) -> Linear position:
%      Pos = FactorW * W + FactorX * X + FactorY * Y + FactorZ * Z
%
%   4. Linear position -> Indices (W,X,Y,Z):
%      W    = div(Pos, FactorW)
%      RemW = mod(Pos, FactorW)
%      X    = div(RemW, FactorX)
%      RemX = mod(RemW, FactorX)
%      Y    = div(RemX, FactorY)
%      RemY = mod(RemX, FactorY)
%      Z    = div(RemY, FactorZ) -> FactorZ = 1, Z = RemY

% dynarray_factor(+Id, -DynarrayCount)
% Id            atom identifying the dynarray
% DynarrayCount number of cells in dynarray
dynarray_factors(Id, DynarrayCount) :-

    dynarr_dims(Id, 0, DimsSizes),
    length(DimsSizes, DimCount),
    dynarray_factors_(Id, DimCount, DimCount, 1, DimsSizes,
                      1, DynarrayCount, [], DimFactors),

    % the dynarr_factors 0 position holds the dimension factors:
    %   [DimFactor1,...,DimFactorN]
    assertz(dynarr_factors(Id, 0, DimFactors)).

% dynarray_factors_(+Id, +DimOrdinal, +DimCount, +CompoundFactor, +DimsSizes,
%                   +CountProgress, -CountFinal, +FactorsProgress,-FactorsFinal)
% Id                    atom identifying the dynarray
% DimOrdinal            size-based position for dynarray dimension
% DimCount              number of dynarray dimensions
% CompoundFactor        current compound factor
% DimsSizes             list of dimensions and its sizes
% CountProgress         working number of dynarray elements
% CountFinal            final number of dynarray elements
% FactorsProgress       the working dimension factors
% FactorsFinal          the final dimension factors

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

%-------------------------------------------------------------------------------
% unify Position with the corresponding Indices
% (Position is the cell's 0-based linear position)
% dynarray_position_position(+Id, ?Position, ?Indices)
% Id            atom identifying the dynarray
% Position      the final 0-based linear position of the element
% Indices       the element's indices (offset-corrected, if applicable)
dynarray_position_indices(Id, Position, Indices) :-

    labels_indices(Id, Indices, Indexes),
    dynarr_labels(Id, da_dims, DimCount),
    (ground(Position) ->
        % fail point
        Position >= 0,
        dynarr_dims(Id, 0, DimsSizes),
        position_indices_1(Id, Position, DimsSizes, [], IndicesOffset),
        dynarray_offset(Id, Indexes, IndicesOffset)
    ;
        dynarray_offset(Id, Indexes, IndicesOffset),
        indices_position_(Id, IndicesOffset, DimCount, 0, Position)
    ).

% obtain the element's linear position from its indices:
%   Pos = Factor1 * I1 + Factor2 * I2 + ... + FactorN * In (FactorN = 1)
%
% indices_position_(+Id, +Indices, +Dim, +PosProgress, -PosFinal)
% Id                    atom identifying the dynarray
% Indices               the element's indices
% Dim                   the 1-based dynarray dimension
% PosProgress           the working linear position of the element
% PosFinal              the final linear position of the element

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

% obtain the element's indices from its linear position
%      Size1 <= Size2 <= ... <= SizeN
%
%      I1     = div(Pos, Factor1)
%      Rem1   = mod(Pos, Factor1)
%      I2     = div(Rem1, Factor2)
%      Rem2   = mod(Rem1, Factor2)
%      :                :
%      :                :
%      In-1   = div(RemN-2, FactorN-1)
%      RemN-1 = mod(RemN-2, FactorN-1)
%      In     = div(RemN-1, FactorN) -> In = RemN-1
%
% position_indices_1(+Id, +Factor, +Dim, +IndicesProgress, -IndicesFinal)
% Id                atom identifying the dynarray
% Position          the element's 0-based linear position
% DimsSizes         the dimensions and their corresponding sizes
% IndicesProgress   the working indices of the element
% IndicesFinal      the final indices of the element

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

%-------------------------------------------------------------------------------

% convert any label in the list of indices into its corresponding integer value
% labels_indices(+Id, +Labels, -Indices)
% Id        atom identifying the dynarray
% Labels    list of indices possibly containing atoms
% Indices   list with corresponding integer values 
labels_indices(Id, Labels, Indices) :-
    ( (var(Labels) , Indices = Labels)
    ; labels_indices_(Id, Labels, [], Indices) ).

labels_indices_(_Id, [], IndicesProgress, IndicesFinal) :-
    reverse(IndicesProgress, IndicesFinal).

labels_indices_(Id, [Label|Labels], IndicesProgress, IndicesFinal) :-

    ((atom(Label) , dynarr_labels(Id, Label, Index)) ; Index = Label),
    labels_indices_(Id, Labels, [Index|IndicesProgress], IndicesFinal).

%-------------------------------------------------------------------------------
% convert real indices into offset indices and back

% dynarray_offset(+Id, ?Indices, ?OffsetIndices)
% Id            atom identifying the dynarray
% Indices       the dynarray cell's indices
% OffsetIndices the dynarray cell's offset indices
dynarray_offset(Id, Indices, OffsetIndices) :-

    (ground(Indices) ->
        indices_offsets_(Id, 1, Indices, [], OffsetIndices)
    ;
        offsets_indices_(Id, 1, OffsetIndices, [], Indices)
    ).

% convert real indices into offset indices
% indices_offsets_(+Id, +Dim, +[Index|Indices], +OffsetsProgress, -OffsetsFinal)
% Id                    atom identifying the dynarray
% Dim                   the current dynarray dimension
% [Index|Indices]       the dynarray cell's real index at head of indices list
% OffsetsProgress       the working dynarray cell's offset indices
% OffsetsFinal          the final dynarray cell's offset indices

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

% convert offset indices into real indices
% offsets_indices_(+Id, +Dim, +[OffsetIndex|OffsetIndices],
%                  +IndicesProgress, -IndicesFinal)
% Id                    atom identifying the dynarray
% Dim                   the current dynarray dimension
% OffsetIndex           the dynarray cell's offset at head of indices list
% IndicesProgress       the working dynarray cell's real indices
% IndicesFinal          the final dynarray cell's real indices

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
