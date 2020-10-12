/*******************************************************************************
* FILENAME / MODULE : dynarray-persistence.pl / dynarray_persistence
*
* DESCRIPTION :
*       This module provides persistence for dynarray objects, using the
*       Berkeley DB utility package. Please, refer to the portability layer
*       files (sicstus-bdb.pl and swi-bdb.pl) for details on the Prolog
*       interface to Berkeley DB.
*
* PUBLIC PREDICATES :
*       dynarray_clone(+IdSource, +IdTarget)
*       dynarray_erase(+Id, +DataSet)
*       dynarray_persist(+Id, +DataSet)
*       dynarray_restore(+Id, +DataSet)
*       dynarray_serialize(+Id, ?Serialized)
*
* NOTES :
*       None yet.
*
*       Copyright Instituto Modal 2020.  All rights reserved.
*
* REVISION HISTORY :
*
* DATE        AUTHOR            REVISION
* ----------  ----------------  ------------------------------------------------
* 2020-08-28  GT Nunes          Module creation
*
*******************************************************************************/

:- module(dynarray_persistence,
    [
        dynarray_clone/2,
        dynarray_erase/2,
        dynarray_persist/2,
        dynarray_restore/2,
        dynarray_serialize/2
    ]).

:- if(current_prolog_flag(dialect, sicstus)).   % SICStus ----------------------

:- use_module('../../goldies/src/bdb/sicstus-bdb',
    [
        bdb_erase/2,
        bdb_retrieve/3,
        bdb_store/3
    ]).

:- elif(current_prolog_flag(dialect, swi)).     % SWI-Prolog -------------------

:- use_module('../../goldies/src/bdb/swi-bdb',
    [
        bdb_erase/2,
        bdb_retrieve/3,
        bdb_store/3
    ]).

:- endif.                                       % ------------------------------

:- use_module(library(lists),
    [
        nth0/3
    ]).

:- use_module('dynarray-core',
    [
        dynarray_create/2,
        dynarray_destroy/1,
        dynarray_label/3,
        dynarray_position_value/3,
        is_dynarray/1
    ]).

%-------------------------------------------------------------------------------
% clone a dynarray

% dynarray_clone(+IdSource, +IdTarget)
% IdSource      atom identifying the source dynarray
% IdTarget      atom identifying the target dynarray
dynarray_clone(IdSource, IdTarget) :-

    % fail points (source dynarray must exist, target dynarray must not exist)
    is_dynarray(IdSource),
    \+ is_dynarray(IdTarget),

    % serialize the source dynarray
    dynarray_serialize(IdSource, Data),

    % create target dynarray with data from source
    dynarray_serialize(IdTarget, Data).

%-------------------------------------------------------------------------------
% A serialization mechanism, for backup/restore purposes. For a given dynarray
% containing Nv values and Np labels, its serialization structure will be
%   [<dims-ranges>],<num-labels>,
%   [<key-label-1>,<value-label-1>],...,[<key-label-Np>,<value-label-Np>],
%   [<pos-value-1>,<value-1>],...,[<pos-value-Nv>,<value-Nv>].
%
% The serialized list will thus contain Np + Nv + 2 elements:
%   <dims-ranges>   - the dimensions ranges used for the dynarray creation
%   <num-labels>    - the total number of key-value label pairs
%   <key-label-j>   - the key in the key-value label pair j
%   <value-label-j> - the value in the key-value label pair
%   <pos-value-j>   - the linear position of value j within the dynarray
%   <value-j>       - the value j within the dynarray

% dynarray_serialize(+Id, ?Serialized)
% Id            atom identifying the dynarray
% Serialized    serialization list containing the dynarray data

dynarray_serialize(Id, Serialized) :-

    % HAZARD: ground(Serialized) might be very expensive
    (var(Serialized) ->
        is_dynarray(Id),
        dynarray_to_serialized(Id, Serialized)
    ;
        ( Serialized = []
        ; serialized_to_dynarray(Id, Serialized) )
    ).

%-------------------------------------------------------------------------------
% Serialize the contents (labels and values) of the dynarray.

dynarray_to_serialized(Id, Serialized) :-

    % retrieve all labels (key and value pairs) in dynarray
    findall([Label,Value],
            dynarray_core:dynarr_labels(Id, Label, Value), Labels),

    % retrieve all values (position-value pairs) in dynarray
    findall([Position,Value],
            dynarray_core:dynarr_values(Position, Id, Value), Values),

    % join them in a single list
    append(Labels, Values, DynData),

    % add dimensions ranges and number of labels
    memberchk([da_ranges,DimRanges], Labels),
    length(Labels, NumLabels),
    append([DimRanges,NumLabels], DynData, Serialized).

%-------------------------------------------------------------------------------
% Restore the contents (labels and values) of the dynarray

% serialized_to_dynarray_(+Id, +Serialized)
% Id            atom identifying the dynarray
% Serialized    the serialized dynarray

serialized_to_dynarray(Id, Serialized) :-

    % create dynarray
    [DimRanges|[NumLabels|_]] = Serialized,
    dynarray_destroy(Id),                       % SANITY POINT
    dynarray_create(Id, DimRanges),

    % restore the labels
    LabelsFinal is NumLabels + 2,
    serialized_to_labels_(Id, Serialized, 2, LabelsFinal),

    % retrieve the positions/values list
    length(Serialized, ValuesFinal),
    serialized_to_values_(Id, Serialized, LabelsFinal, ValuesFinal).

% serialized_to_labels_(+Id, +Labels, +PosCurr, +PosFinal)
% Id        atom identifying the dynarray
% Labels    the labels (key-value pairs) to load to the dynarray
% PosCurr   the current label position
% PosLast   the last label position

% (done)
serialized_to_labels_(_Id, _Labels, PosFinal, PosFinal).

% (iterate)
serialized_to_labels_(Id, Labels, PosCurr, PosFinal) :-

    % register the label (da_* labels are not accepted)
    nth0(PosCurr, Labels, [Key,Value]),
    (dynarray_label(Id, Key, Value) ; true),

    % go for the next label
    PosNext is PosCurr + 1,
    serialized_to_labels_(Id, Labels, PosNext, PosFinal).

% serialized_to_values_(+Id, +Values, +PosCurr, +PosFinal)
% Id                atom identifying the dynarray
% Position,Value    the position/value to load to the dynarray
% PosCurr           the current value position
% PosLast           the last value position

% (done)
serialized_to_values_(_Id, _Values, PosFinal, PosFinal).

% (iterate)
serialized_to_values_(Id, Values, PosCurr, PosFinal) :-

    % load the value onto the dynarray
    nth0(PosCurr, Values, [Position,Value]),
    dynarray_position_value(Id, Position, Value),

    % go for the next value
    PosNext is PosCurr + 1,
    serialized_to_values_(Id, Values, PosNext, PosFinal).

%-------------------------------------------------------------------------------
% persist the dynarray data to a Berkeley DB external storage

% dynarray_persist(+Id, +DataSet)
% Id        atom identifying the dynarray
% DataSet   atom identifyingt the data set
dynarray_persist(Id, DataSet) :-

    % fail point
    is_dynarray(Id),

    % fail point (erase the dynarray storage)
    bdb_erase(Id, DataSet),

    % obtain the dynarray data
    dynarray_serialize(Id, Data),

    !,
    % fail point (persist the dynarray data)
    bdb_store(Id, DataSet, Data).

%-------------------------------------------------------------------------------
% restore the dynarray data from a Berkeley DB external storage

% dynarray_restore(+Id, +DataSet)
% Id        atom identifying the dynarray
% DataSet   atom identifyingt the data set
dynarray_restore(Id, DataSet) :-

    % fail point (retrieve the dynarray data from external storage)
    bdb_retrieve(Id, DataSet, Data),

    % re-create the dynarray with its contents
    dynarray_serialize(Id, Data).

%-------------------------------------------------------------------------------
% erase the dynarray persisted data

% dynarray_erase(+Id, +DataSet)
% Id        atom identifying the dynarray
% DataSet   atom identifyingt the data set
dynarray_erase(Id, DataSet) :-

    % fail point (erase the dynarray storage)
    bdb_erase(Id, DataSet).
