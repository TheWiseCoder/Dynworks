/*******************************************************************************
* FILENAME / MODULE : csv_wrapper.pl / csv_wrapper
*
* DESCRIPTION :
*       This is an attempt to define a standard interface for csv file
*       operations, to be used in different Prolog environments. This module
*       implements this standard for the SICStus Prolog platform.
*
* PUBLIC PREDICATES :
*       csv_input_record(+Stream, +Record)
*       csv_input_records(+Stream, +Records)
*       csv_is_header(+Record)
*       csv_output_record(+Stream, +Record)
*       csv_output_records(+Stream, +Records)
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
* 2020-10-12  GT Nunes          Module creation
*
*******************************************************************************/

:- module(csv_wrapper,
    [
        csv_input_record/2,
        csv_input_records/2,
        csv_is_header/1,
        csv_output_record/2,
        csv_output_records/2
    ]).

:- use_module(library(csv),
    [
        read_record/2,
        read_records/2,
        write_record/2
    ]).

:- use_module(library(lists),
    [
        convlist/3,
        maplist/2
    ]).

%-------------------------------------------------------------------------------
% csv file input

% retrieve all csv records from Stream
% csv_input_records(+Stream, +Records)
% Stream    the input stream
% Records   list of lists of fields
csv_input_records(Stream, Records) :-

    read_records(Stream, Rows),
    convlist(unwrap_row, Rows, Records).

% retrieve the next csv record from Stream
% csv_input_records(+Stream, +Records)
% Stream    the input stream
% Record    list of fields
csv_input_record(Stream, Record) :-

    read_record(Stream, Row),
    convlist(unwrap_field, Row, Record).

% build a csv record from the fields in a row
unwrap_row(Row, Record) :-
    convlist(unwrap_field, Row, Record).

% unwrap a field from a row
unwrap_field(Field, Value) :-

    ( integer(Value, _) = Field
    ; float(Value, _) = Field
    ; (string(Codes) = Field , atom_codes(Value, Codes)) ).

%-------------------------------------------------------------------------------
% csv file output

% write the given csv records to Stream
% csv_output_records(+Stream, +Records)
% Stream    the output stream
% Records   list of lists of fields
csv_output_records(Stream, Records) :-
    maplist(csv_output_record(Stream), Records).

% write the given csv record to Stream
% csv_output_record(+Stream, +Records)
% Stream    the output stream
% Record    list of fields
csv_output_record(Stream, Record) :-

    convlist(wrap_field, Record, Row),
    write_record(Stream, Row).

% wrap a field for inclusion in a row
wrap_field(Field, Value) :-

    ( atom(Field) ->
        atom_codes(Field, Codes),
        Value = string(Codes)
    ; integer(Field) ->
        number_codes(Field, Codes),
        Value = integer(Field, Codes)
    ; float(Field) ->
        number_codes(Field, Codes),
        Value = float(Field, Codes)
    ; otherwise ->
        % null char value
        Value = '\000\'
    ).

%-------------------------------------------------------------------------------

% assert whether all fields in Record may be column names
% csv_is_header(+Record)
% Record    list of fields
csv_is_header(Record) :-
    maplist(is_col_name, Record). 

% assert whether Field may be a column name
% is_col_name(+Field)
% Field     the column name candidate
is_col_name(Field) :-

    % fail points
    atom(Field),
    atom_length(Field, Len),
    Len =< 32.
