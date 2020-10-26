:- module(dynarray_persistence,
    [
        dynarray_clone/2,       % dynarray_clone(+Id:atomSource, +Id:atomTarget)
        dynarray_csv/2,         % dynarray_csv(+Id:atom, +Stream)
        dynarray_erase/2,       % dynarray_erase(+Id:atom, +DataSet)
        dynarray_persist/2,     % dynarray_persist(+Id:atom, +DataSet)
        dynarray_restore/2,     % dynarray_restore(+Id:atom, +DataSet)
        dynarray_serialize/2    % dynarray_serialize(+Id:atom, ?Serialized)
    ]).

/** <module> Persistence for dynarray objects, using Berkeley DB

This module provides persistence for dynarray objects, using the Berkeley DB
utility package. Please, refer to bdb_wrapper.pl for details on the SWI-Prolog
interface to Berkeley DB.

Additionally, persisting and restoring from `.csv` files is also implemented.
Please, refer to the csv_wrapper.pl for details.

The following  considerations apply for CSV operations:<br/>
a. the dynarray involved must be 2-dimensional, and will be handled as having
rows (dimension 1) and columns (dimension 2);<br/>
b. the stream involved must be of type 'text', and will be read or written from
its current position;<br/>
c. persisting to, or restoring from, the given stream will be attempted,
depending on whether or not the dynarray exists;<br/>
d. input and output are performed through the Prolog platform's built-in CSV
library;<br/>
e. when persisting, the atoms associated with the dynarray's columns, if they
exist, will be used as field names in the CSV file's header record;<br/>
f. when persisting, the data registered as labels, apart from the column names,
will not be included;<br/>
g. when persisting, missing cells will be recorded on the CSV file as empty fields
(containing the null char '\000\');<br/>
h. when restoring, an attempt will be made to extract field names from the CSV
file's first record and use them as labels; if not possible, the record will be
treated as regular data.

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
        convlist/3,
        maplist/2,
        maplist/3,
        nth0/3
    ]).

:- use_module('./sicstus/bdb_wrapper',
    [
        bdb_erase/2,
        bdb_retrieve/3,
        bdb_store/3
    ]).

:- use_module('./sicstus/csv_wrapper',
    [
        csv_input_records/2,
        csv_is_header/1,
        csv_output_record/2
    ]).

:- elif(current_prolog_flag(dialect, swi)).     % SWI-Prolog -------------------------

:- use_module(library(apply),
    [
        convlist/3,
        maplist/2,
        maplist/3
    ]).

:- use_module(library(lists),
    [
        nth0/3,
        numlist/3
    ]).

:- use_module('./swi/bdb_wrapper',
    [
        bdb_erase/2,
        bdb_retrieve/3,
        bdb_store/3
    ]).

:- use_module('./swi/csv_wrapper',
    [
        csv_input_records/2,
        csv_is_header/1,
        csv_output_record/2
    ]).

:- endif.                                       % ------------------------------------

:- use_module('dynarray_core',
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

%-------------------------------------------------------------------------------------

%! dynarray_clone(+Id:atomSource:atom, +Id:atomTarget:atom) is semidet.
%
%  Clone a dynarray.
%
%  @param IdSource Atom identifying the source dynarray
%  @param IdTarget Atom identifying the target dynarray

dynarray_clone(IdSource, IdTarget) :-

    % fail points (source dynarray must exist, target dynarray must not exist)
    is_dynarray(IdSource),
    \+ is_dynarray(IdTarget),

    % serialize the source dynarray
    dynarray_serialize(IdSource, Data),

    % create target dynarray with serialized data from IdSource
    dynarray_serialize(IdTarget, Data).

%-------------------------------------------------------------------------------------

%! dynarray_csv(+Id:atom, +Stream:ref) is det.
%
%  Persist or restore a dynarray into/from a CSV file.
%
%  @param Id     Atom identifying the dynarray
%  @param Stream Stream to read from/write to

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

%-------------------------------------------------------------------------------------

%! dynarray_to_csv(+Id:atom, +Stream:ref)
%  Persist the dynarray to Stream as a CSV file.

%  @param Id     Atom identifying the dynarray
%  @param Stream Stream to write to

dynarray_to_csv(Id, Stream) :-

    % retrieve the number of columns (columns are 0-based)
    dynarray_cells(Id, 2, ColCount),
    ColLast is ColCount - 1,
    numlist(0, ColLast, ColOrdinals),

    % are column names registered as labels ?
    ( ( convlist(col_label(Id), ColOrdinals, ColNames)
      , length(ColNames, ColCount)
      , csv_is_header(ColNames) )->
        % yes, so write the CSV file header
        csv_output_record(Stream, ColNames)
    ;
        % no, so proceed
        true
    ),

    % persist the dynarray data to a CSV file (rows are 0-based)
    dynarray_cells(Id, 2, RowCount),
    RowLast is RowCount - 1,
    numlist(0, RowLast, RowOrdinals),
    maplist(output_record(Id, Stream, ColOrdinals), RowOrdinals).

% retrieve the label associated with ColOrdinal
col_label(Id, ColOrdinal, Label) :-
    % fail point
    dynarray_label(Id, Label, ColOrdinal).

% build and output the CSV record
output_record(Id, Stream, ColOrdinals, RowOrdinal) :-

    maplist(output_field(Id, RowOrdinal), ColOrdinals, Record),
    csv_output_record(Stream, Record).

output_field(Id, RowOrdinal, ColOrdinal, Field) :-
    dynarray_value(Id, [RowOrdinal,ColOrdinal], Field).

%-------------------------------------------------------------------------------------

%! csv_to_dynarray(+Id:atom, +Stream:ref) is det.
%
%  Restore the dynarray from a CSV file in Stream.
%
%  @param Id     Atom identifying the dynarray
%  @param Stream Stream to read from

csv_to_dynarray(Id, Stream) :-

    % input CSV records
    csv_input_records(Stream, Records),
    length(Records, Len),

    % set aside head and compute number of columns (columns are 0-based)
    [Head|Tail] = Records,
    length(Head, ColCount),
    ColLast is ColCount - 1,
    numlist(0, ColLast, ColOrdinals),

    % is it a CSV file header ?-
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

% restore the CSV Record
load_record(Id, ColOrdinals, RowOrdinal, Record) :-
    maplist(load_field(Id, RowOrdinal), ColOrdinals, Record).

% restore the CSV Field
load_field(Id, RowOrdinal, ColOrdinal, Field) :-
    dynarray_value(Id, [RowOrdinal,ColOrdinal], Field).

%-------------------------------------------------------------------------------------

%! dynarray_serialize(+Id:atom, ?Serialized:data) is det.
%
%  A serialization mechanism, for backup/restore purposes.
%
%  For a given dynarray containing `Nv` values and `Nb` labels, its serialization
%  structure will be<br/>
%    [<dims-ranges>],<Nb>,<br/>
%    [<key-label-1>,<value-label-1>],...,[<key-label-Nb>,<value-label-Nb>],<br/>
%    [<pos-value-1>,<value-1>],...,[<pos-value-Nv>,<value-Nv>]
%
% The serialized list will thus contain `Np + Nv + 2` elements:<br/>
%    <dims-ranges>   - the dimensions ranges used for the dynarray creation<br/>
%    <num-labels>    - the total number of key-value label pairs<br/>
%    <key-label-j>   - the key in the key-value label pair `j`<br/>
%    <value-label-j> - the value in the key-value label pair `j`<br/>
%    <pos-value-j>   - the linear position of value `j` within the dynarray<br/>
%    <value-j>       - the value `j` within the dynarray
%
%  @param Id         Atom identifying the dynarray
%  @param Serialized Serialization list containing the dynarray data

dynarray_serialize(Id, Serialized) :-

    % HAZARD: ground(Serialized) might be very expensive
    (var(Serialized) ->
        is_dynarray(Id),
        dynarray_to_serialized(Id, Serialized)
    ;
        ( Serialized = []
        ; serialized_to_dynarray(Id, Serialized) )
    ).

%-------------------------------------------------------------------------------------

%! dynarray_to_serialized(+Id:atom, +Serialized:data) is det.
%
%  Serialize the contents (labels and values) of the dynarray.
%
%  @param Id         Atom identifying the dynarray
%  @param Serialized Serialization list containing the dynarray data

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

%-------------------------------------------------------------------------------------

%! serialized_to_dynarray(+Id:atom, +Serialized:data) is det.
%
%  Restore the contents (labels and values) of the dynarray.
%
%  @param Id         Atom identifying the dynarray
%  @param Serialized Serialization list containing the dynarray data

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

% serialized_to_labels_(+Id:atom, +Labels, +PosCurr, +PosFinal) is det.
%
% @param Id      Atom identifying the dynarray
% @param Labels  The labels (key-value pairs) to load to the dynarray
% @param PosCurr The current label position
% @param PosLast The last label position

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

% serialized_to_values_(+Id:atom, +Values, +PosCurr, +PosFinal) is det.
%
% @param Id      Atom identifying the dynarray
% @param Value   The value to load to the dynarray
% @param PosCurr The current value position
% @param PosLast The last value position

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

%-------------------------------------------------------------------------------------

%! dynarray_persist(+Id:atom, +DataSet:atom) is det.
%
%  Persist the dynarray data to a Berkeley DB external storage.
%
%  @param Id      Atom identifying the dynarray
%  @param DataSet Atom identifyingt the data set

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

%-------------------------------------------------------------------------------------

%! dynarray_restore(+Id:atom, +DataSet:atom) is det.
%
%  Restore the dynarray data from a Berkeley DB external storage.
%
%  @param Id      Atom identifying the dynarray
%  @param DataSet Atom identifyingt the data set

dynarray_restore(Id, DataSet) :-

    % fail point (retrieve the dynarray data from external storage)
    bdb_retrieve(Id, DataSet, Data),

    % re-create the dynarray with its contents
    dynarray_serialize(Id, Data).

%-------------------------------------------------------------------------------------

%! dynarray_erase(+Id:atom, +DataSet:atom) is det.
%
%  Erase the dynarray's persisted data.
%
%  @param Id      Atom identifying the dynarray
%  @param DataSet Atom identifyingt the data set

dynarray_erase(Id, DataSet) :-

    % fail point (erase the dynarray storage)
    bdb_erase(Id, DataSet).
