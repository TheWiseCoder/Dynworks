/*******************************************************************************
* FILENAME / MODULE : dynarray-persistence.pl / dynarray_persistence
*
* DESCRIPTION :
*       This module provides persistence for dynarray objects, using the
*       Berkeley DB utility package. Please, refer to the portability layer
*       files (sicstus-bdb.pl and swi-bdb.pl) for details on the Prolog
*       interface to Berkeley DB.
*
*       Additionally, persisting and restoring from '.csv' files is also
*       implemented. Please, refer to the portability layer files
*       (sicstus-csv.pl and swi-csv.pl) for details. The following
*       considerations apply for csv operations:
*         1. the dynarray involved must be 2-dimensional, and will be handled
*            as having rows (dimension 1) and columns (dimension 2)
*         2. the stream involved must be of type 'text', and will be read
*            or written from its current position  
*         3. persisting to, or restoring from, the given stream will be
*            attempted, depending on whether or not the dynarray exists
*         4. input and output are performed through the Prolog platform's
*            built-in csv library
*         5. when persisting, the atoms associated with the dynarray's columns
*            through dynarray_label/3, if they exist, will be used as field
*            names in the csv file's header record
*         6. when persisting, the data registered as labels, apart from the
*            column names, will not be included
*         7. when persisting, missing cells will be recorded on the csv file
*            as empty fields (containing the null char '\000\')
*         8. when restoring, an attempt will be made to extract field names
*            from the csv file's first record and use them as labels through
*            dynarray_label/3; if not possible, the record will be treated
*            as regular data
*
* PUBLIC PREDICATES :
*       dynarray_clone(+IdSource, +IdTarget)
*       dynarray_csv(+Id, +Stream)
*       dynarray_erase(+Id, +DataSet)
*       dynarray_persist(+Id, +DataSet)
*       dynarray_restore(+Id, +DataSet)
*       dynarray_serialize(+Id, ?Serialized)
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
* 2020-08-28  GT Nunes          Module creation
*
*******************************************************************************/

:- module(dynarray_persistence,
    [
        dynarray_clone/2,
        dynarray_csv/2,
        dynarray_erase/2,
        dynarray_persist/2,
        dynarray_restore/2,
        dynarray_serialize/2
    ]).

:- if(current_prolog_flag(dialect, sicstus)).   % SICStus ----------------------

:- use_module(library(between),
    [
        numlist/3
    ]).

:- use_module(library(lists),
    [
        convlist/3,
        maplist/2,
        maplist/3,
        nth0/3
    ]).

:- use_module('../Goldies/src/bdb/sicstus-bdb',
    [
        bdb_erase/2,
        bdb_retrieve/3,
        bdb_store/3
    ]).

:- use_module('../Goldies/src/csv/sicstus-csv',
    [
        csv_input_records/2,
        csv_is_header/1,
        csv_output_record/2
    ]).

:- elif(current_prolog_flag(dialect, swi)).     % SWI-Prolog -------------------

:- use_module(library(apply),
    [
        convlist/3,
        maplist/2,
        maplist/3,
        nth0/3
    ]).

:- use_module(library(lists),
    [
        numlist/3
    ]).

:- use_module('../Goldies/src/bdb/swi-bdb',
    [
        bdb_erase/2,
        bdb_retrieve/3,
        bdb_store/3
    ]).

:- use_module('../Goldies/src/csv/swi-csv',
    [
        csv_input_records/2,
        csv_is_header/1,
        csv_output_record/2
    ]).

:- endif.                                       % ------------------------------

:- use_module('dynarray-core',
    [
        dynarray_cells/3,
        dynarray_create/2,
        dynarray_dims/2,
        dynarray_destroy/1,
        dynarray_label/3,
        dynarray_position_value/3,
        dynarray_value/3,
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

    % create target dynarray with serialized data from IdSource
    dynarray_serialize(IdTarget, Data).

%-------------------------------------------------------------------------------
% persist or restore a dynarray into/from a csv file

% dynarray_csv(+Id, +Stream)
% Id        atom identifying the dynarray
% Stream    stream to read from/write to
dynarray_csv(Id, Stream) :-

    % does Id identify a dynarray ?
    (is_dynarray(Id) ->
        % yes, so persist it (fail if it is not bi-dimensional)
        dynarray_dims(Id, 2),
        dynarray_to_csv(Id, Stream)
    ;
        % no, so restore it
        csv_to_dynarray(Id, Stream)
    ).

% persist the dynarray to Stream as a csv file
dynarray_to_csv(Id, Stream) :-

    % retrieve the number of columns (columns are 0-based)
    dynarray_cells(Id, 2, ColCount),
    ColLast is ColCount - 1,
    numlist(0, ColLast, ColOrdinals),

    % are column names registered as labels ?
    ( ( convlist(col_label(Id), ColOrdinals, ColNames)
      , length(ColNames, ColCount)
      , csv_is_header(ColNames) )->
        % yes, so write the csv file header
        csv_output_record(Stream, ColNames)
    ;
        % no, so proceed
        true
    ),

    % persist the dynarray data to a csv file (rows are 0-based)
    dynarray_cells(Id, 2, RowCount),
    RowLast is RowCount - 1,
    numlist(0, RowLast, RowOrdinals),
    maplist(output_record(Id, Stream, ColOrdinals), RowOrdinals).

% retrieve the label associated with ColOrdinal
col_label(Id, ColOrdinal, Label) :-
    % fail point
    dynarray_label(Id, Label, ColOrdinal).

% build and output the csv record
output_record(Id, Stream, ColOrdinals, RowOrdinal) :-

    maplist(output_field(Id, RowOrdinal), ColOrdinals, Record),
    csv_output_record(Stream, Record).

output_field(Id, RowOrdinal, ColOrdinal, Field) :-
    dynarray_value(Id, [RowOrdinal,ColOrdinal], Field).

% restore the dynarray from a csv file in Stream
csv_to_dynarray(Id, Stream) :-

    % input csv records
    csv_input_records(Stream, Records),
    length(Records, Len),

    % set aside head and compute number of columns (columns are 0-based)
    [Head|Tail] = Records,
    length(Head, ColCount),
    ColLast is ColCount - 1,
    numlist(0, ColLast, ColOrdinals),

    % is it a csv file header ?-
    (csv_is_header(Head) ->

        % yes
        RowCount is Len - 1,

        % create the dynarray
        dynarray_create(Id, [RowCount,ColCount]),

        % register the column names
        maplist(dynarray_label(Id), Head, ColOrdinals),

        % establish the data
        Data = Tail
    ;
        % no
        RowCount = Len,

        % create the dynarray
        dynarray_create(Id, [RowCount,ColCount]),

        % establish the data
        Data = Records
    ),

    % load the data onto the dynarray (rows are 0-based)
    RowLast is RowCount - 1,
    numlist(0, RowLast, RowOrdinals),
    maplist(load_record(Id, ColOrdinals), RowOrdinals, Data).

% restore the csv Record
load_record(Id, ColOrdinals, RowOrdinal, Record) :-
    maplist(load_field(Id, RowOrdinal), ColOrdinals, Record).

% restore the csv Field
load_field(Id, RowOrdinal, ColOrdinal, Field) :-
    dynarray_value(Id, [RowOrdinal,ColOrdinal], Field).

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
