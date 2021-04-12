:- module(dynvector_persistence,
    [
        dynvector_clone/2,
        dynvector_erase/2,
        dynvector_persist/2,
        dynvector_restore/2,
        dynvector_serialize/2
    ]).

/** <module> Persistence for dynvector objects, using Berkeley DB

This module provides persistence for dynvector objects, using the Berkeley DB
utility package. For details on the SWI-Prolog interface to Berkeley DB,
please refer to the documentation for bdb_rapper.pl .

@author GT Nunes
@version 1.3.2
@copyright (c) TheWiseCoder 2020-2021
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- if(current_prolog_flag(dialect, sicstus)).

:- use_module('./sicstus/bdb_wrapper',
    [
        bdb_erase/2,
        bdb_retrieve/3,
        bdb_store/3
    ]).

:- elif(current_prolog_flag(dialect, swi)).

:- use_module('./swi/bdb_wrapper',
    [
        bdb_erase/2,
        bdb_retrieve/3,
        bdb_store/3
    ]).

:- endif.

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

%-------------------------------------------------------------------------------------

%! dynvector_clone(+IdSource, +IdTarget) is det.
%
%  Clone a dynvector.
%
%  @param IdSource Atom identifying the source dynvector
%  @param IdTarget Atom identifying the target dynvector

dynvector_clone(IdSource, IdTarget) :-

    % fail points (source dynvector must exist, target dynvector must not exist)
    is_dynvector(IdSource),
    \+ is_dynvector(IdTarget),

    % serialize the source dynvector
    dynvector_serialize(IdSource, Data),

    % create target dynvector with serialized data from IdSource
    dynvector_serialize(IdTarget, Data).

%-------------------------------------------------------------------------------------

%! dynvector_serialize(+Id, ?Serialized) is det.
%
%  A serialization mechanism, for backup/restore purposes. The description below
%  applies for a given dynvector containing `Nv` values and `Nb` labels.
%  ~~~
%  Its serialization structure will be:
%    <Nb>,<br/>
%    [<key-label-1>,<value-label-1>],...,[<key-label-Nb>,<value-label-Nb>],<br/>
%    [<index-1>,<value-1>],...,[<index-Nv>,<value-Nv>]
%
%  The serialized list will thus contain `Np + Nv + 1` elements:<br/>
%    <num-labels>    - the total number of key-value label pairs<br/>
%    <key-label-j>   - the key in the key-value label pair `j`<br/>
%    <value-label-j> - the value in the key-value label pair<br/>
%    <index-j>       - the index position of value `j` within the dynvector<br/>
%    <value-j>       - the value `j` within the dynvector
%  ~~~
%
%  @param Id         Atom identifying the dynvector
%  @param Serialized Serialization list containing the dynvector data

dynvector_serialize(Id, Serialized) :-

    % HAZARD: ground(Serialized) might be very expensive
    (var(Serialized) ->
        is_dynvector(Id),
        dynvector_to_serialized(Id, Serialized)
    ;
        (Serialized = [] ->
            dynvector_destroy(Id),
            dynvector_create(Id) 
        ;
            serialized_to_dynvector(Id, Serialized)
        )
    ).

%-------------------------------------------------------------------------------------

%! dynvector_to_serialized(+Id:atom, Serialized:data) is det.
%
%  Serialize the contents (labels and values) of the dynvector.
%
%  @param Id         Atom identifying the dynvector
%  @param Serialized Serialization list containing the dynvector data

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

%-------------------------------------------------------------------------------------

%! serialized_to_dynvector(+Id:atom, +Serialized:data) is det.
%
%  Restore the contents (labels and values) of the dynvector
%
%  @param Id         Atom identifying the dynvector
%  @param Serialized Serialization list containing the dynvector data

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

%! serialized_to_labels_(+Id:atom, +Labels:list, +PosCurr:int, +PosFinal:int) is det.
%
%  @param Id      Atom identifying the dynvector
%  @param Labels  The labels (key-value pairs) to load to the dynvector
%  @param PosCurr The current label position
%  @param PosLast The last label position

% (done)
serialized_to_labels_(_Id, _Labels, PosFinal, PosFinal) :- !.

% (iterate)
serialized_to_labels_(Id, Labels, PosCurr, PosFinal) :-

    % register the label (dv_* labels are not accepted)
    nth0(PosCurr, Labels, [Key,Value]),
    (dynvector_label(Id, Key, Value) ; true),
    !,

    % go for the next label
    PosNext is PosCurr + 1,
    serialized_to_labels_(Id, Labels, PosNext, PosFinal).

%! serialized_to_values_(+Id:atom, +Values:data) is det.
%
%  @param Id     Atom identifying the dynvector
%  @param Values The values to load to the dynvector

% (done)
serialized_to_values_(_Id, _Values, IndexFinal, IndexFinal) :- !.

% (iterate)
serialized_to_values_(Id, Values, IndexCurr, IndexFinal) :-

    % load the value onto the dynvector
    nth0(IndexCurr, Values, [Index,Value]),
    dynvector_value(Id, Index, Value),

    % go for the next value
    IndexNext is IndexCurr + 1,
    serialized_to_values_(Id, Values, IndexNext, IndexFinal).

%-------------------------------------------------------------------------------------

%! dynvector_persist(+Id:atom, +DataSet:atom) is det.
%
%  Persist the dynvector data to a Berkeley DB external storage.
%
%  @param Id      Atom identifying the dynvector
%  @param DataSet Atom identifying the data set

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

%-------------------------------------------------------------------------------------

%! dynvector_restore(+Id, +DataSet) is det.
%
%  Restore the dynvector data from a Berkeley DB external storage
%
%  @param Id      Atom identifying the dynvector
%  @param DataSet Atom identifying the data set

dynvector_restore(Id, DataSet) :-

    % fail point (retrieve the dynvector data from external storage)
    bdb_retrieve(Id, DataSet, Data),

    % re-create the dynvector with its contents
    dynvector_serialize(Id, Data).

%-------------------------------------------------------------------------------------

%! dynvector_erase(+Id:atom, +DataSet:atom) is det.
%
%  Erase the dynvector persisted data.
%
%  @param Id      Atom identifying the dynvector
%  @param DataSet Atom identifying the data set

dynvector_erase(Id, DataSet) :-

    % fail point (erase the dynvector storage)
    bdb_erase(Id, DataSet).
