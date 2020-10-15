/*******************************************************************************
* FILENAME / MODULE : dynvector-persistence.pl / dynvector_persistence
*
* DESCRIPTION :
*       This module provides persistence for dynvector objects, using the
*       Berkeley DB utility package. Please, refer to the portability layer
*       file (bdb-wrapper.pl) for details on the Prolog interface to Berkeley DB.
*
* PUBLIC PREDICATES :
*       dynvector_clone(+IdSource, +IdTarget)
*       dynvector_erase(+Id, +DataSet)
*       dynvector_persist(+Id, +DataSet)
*       dynvector_restore(+Id, +DataSet)
*       dynvector_serialize(+Id, ?Serialized)
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

:- module(dynvector_persistence,
    [
        dynvector_clone/2,
        dynvector_erase/2,
        dynvector_persist/2,
        dynvector_restore/2,
        dynvector_serialize/2
    ]).

:- if(current_prolog_flag(dialect, sicstus)).   % SICStus ----------------------

:- use_module('./sicstus/bdb_wrapper',
    [
        bdb_erase/2,
        bdb_retrieve/3,
        bdb_store/3
    ]).

:- elif(current_prolog_flag(dialect, swi)).     % SWI-Prolog -------------------

:- use_module('./swi/bdb_wrapper',
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

:- use_module('dynvector_core',
    [
        dynvector_create/1,
        dynvector_destroy/1,
        dynvector_label/3,
        dynvector_value/3,
        is_dynvector/1
    ]).

%-------------------------------------------------------------------------------
% clone a dynvector

% dynvector_clone(+IdSource, +IdTarget)
% IdSource      atom identifying the source dynvector
% IdTarget      atom identifying the target dynvector
dynvector_clone(IdSource, IdTarget) :-

    % fail points (source dynvector must exist, target dynvector must not exist)
    is_dynvector(IdSource),
    \+ is_dynvector(IdTarget),

    % serialize the source dynvector
    dynvector_serialize(IdSource, Data),

    % create target dynvector with serialized data from IdSource
    dynvector_serialize(IdTarget, Data).

%-------------------------------------------------------------------------------
% A serialization mechanism, for backup/restore purposes. For a given dynvector
% containing Nv values and Np labels, its serialization structure will be
%   <num-labels>,
%   [<key-label-1>,<value-label-1>],...,[<key-label-Np>,<value-label-Np>],
%   [<index-1>,<value-1>],...,[<pos-value-Nv>,<value-Nv>].
%
% The serialized list will thus contain Np + Nv + 1 elements:
%   <num-labels>    - the total number of key-value label pairs
%   <key-label-j>   - the key in the key-value label pair j
%   <value-label-j> - the value in the key-value label pair
%   <index-j>       - the index position of value j within the dynvector
%   <value-j>       - the value j within the dynvector

% dynvector_serialize(+Id, ?Serialized)
% Id            atom identifying the dynvector
% Serialized    serialization list containing the dynvector data

dynvector_serialize(Id, Serialized) :-

    % HAZARD: ground(Serialized) might be very expensive
    (var(Serialized) ->
        is_dynvector(Id),
        dynvector_to_serialized(Id, Serialized)
    ;
        ( Serialized = []
        ; serialized_to_dynvector(Id, Serialized) )
    ).

%-------------------------------------------------------------------------------
% Serialize the contents (labels and values) of the dynvector.

dynvector_to_serialized(Id, Serialized) :-

    % retrieve all labels (key and value pairs) in dynvector
    findall([Label,Value],
            dynvector_core:dynvect_labels(Id, Label, Value), Labels),

    % retrieve all values (position-value pairs) in dynvector
    findall([Index,Value],
            dynvector_core:dynvect_values(Index, Id, Value), Values),

    % join them in a single list
    append(Labels, Values, DynData),

    % add dimensions ranges and number of labels
    length(Labels, NumLabels),
    append([NumLabels], DynData, Serialized).

%-------------------------------------------------------------------------------
% Restore the contents (labels and values) of the dynvector

% serialized_to_dynvector_(+Id, +Serialized)
% Id            atom identifying the dynvector
% Serialized    the serialized dynvector

serialized_to_dynvector(Id, Serialized) :-

    % create dynvector
    [NumLabels|_] = Serialized,
    dynvector_destroy(Id),                        % SANITY POINT
    dynvector_create(Id),

    % restore the labels
    LabelsFinal is NumLabels + 2,
    serialized_to_labels_(Id, Serialized, 2, LabelsFinal),

    % retrieve the indices/values list
    length(Serialized, ValuesFinal),
    serialized_to_values_(Id, Serialized, LabelsFinal, ValuesFinal).

% serialized_to_labels_(+Id, +Labels, +PosCurr, +PosFinal)
% Id        atom identifying the dynvector
% Labels    the labels (key-value pairs) to load to the dynvector
% PosCurr   the current label position
% PosLast   the last label position

% (done)
serialized_to_labels_(_Id, _Labels, PosFinal, PosFinal).

% (iterate)
serialized_to_labels_(Id, Labels, PosCurr, PosFinal) :-

    % register the label (dv_* labels are not accepted)
    nth0(PosCurr, Labels, [Key,Value]),
    (dynvector_label(Id, Key, Value) ; true),

    % go for the next label
    PosNext is PosCurr + 1,
    serialized_to_labels_(Id, Labels, PosNext, PosFinal).

% serialized_to_values_(+Id, +Values)
% Id            atom identifying the dynvector
% Index,Value   the index/value to load to the dynvector

% (done)
serialized_to_values_(_Id, _Values, IndexFinal, IndexFinal).

% (iterate)
serialized_to_values_(Id, Values, IndexCurr, IndexFinal) :-

    % load the value onto the dynvector
    nth0(IndexCurr, Values, [Index,Value]),
    dynvector_value(Id, Index, Value),

    % go for the next value
    IndexNext is IndexCurr + 1,
    serialized_to_values_(Id, Values, IndexNext, IndexFinal).

%-------------------------------------------------------------------------------
% persist the dynvector data to a Berkeley DB external storage

% dynvector_persist(+Id, +DataSet)
% Id        atom identifying the dynvector
% DataSet   atom identifying the data set
dynvector_persist(Id, DataSet) :-

    % fail point
    is_dynvector(Id),

    % fail point (erase the dynvector storage)
    bdb_erase(Id, DataSet),

    % obtain the dynvector data
    dynvector_serialize(Id, Data),

    !,
    % fail point (persist the dynvector data)
    bdb_store(Id, DataSet, Data).

%-------------------------------------------------------------------------------
% restore the dynvector data from a Berkeley DB external storage

% dynvector_restore(+Id, +DataSet)
% Id        atom identifying the dynvector
% DataSet   atom identifying the data set
dynvector_restore(Id, DataSet) :-

    % fail point (retrieve the dynvector data from external storage)
    bdb_retrieve(Id, DataSet, Data),

    % re-create the dynvector with its contents
    dynvector_serialize(Id, Data).

%-------------------------------------------------------------------------------
% erase the dynvector persisted data

% dynvector_erase(+Id, +DataSet)
% Id        atom identifying the dynvector
% DataSet   atom identifying the data set
dynvector_erase(Id, DataSet) :-

    % fail point (erase the dynvector storage)
    bdb_erase(Id, DataSet).
