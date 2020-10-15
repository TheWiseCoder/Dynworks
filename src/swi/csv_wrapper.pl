/*******************************************************************************
* FILENAME / MODULE : csv_wrapper.pl / csv_wrapper
*
* DESCRIPTION :
*       This is an attempt to define a standard interface for csv file
*       operations, to be used in different Prolog environments. This module
*       implements this standard for the SWI-Prolog platform.
*
* PUBLIC PREDICATES :
*       csv_input_record(+Stream, +Record)
*       csv_input_record(+Stream, +Record, +CompiledOptions)
*       csv_input_records(+Stream, +Records)
*       csv_input_records(+Stream, +Records, + Options)
*       csv_is_header(+Record)
*       csv_output_record(+Stream, +Record)
*       csv_output_record(+Stream, +Record, Options)
*       csv_output_records(+Stream, +Records)
*       csv_output_records(+Stream, +Records, Options)
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
        csv_input_record/3,
        csv_input_records/2,
        csv_input_records/3,
        csv_is_header/1,
        csv_output_record/2,
        csv_output_record/3,
        csv_output_records/2,
        csv_output_records/3
    ]).

:- use_module(library(apply),
    [
        convlist/3,
        maplist/2,
        maplist/3
    ]).

:- use_module(library(csv),
    [
        csv_read_row/3,
        csv_read_stream/3,
        csv_write_stream/3
    ]).

%-------------------------------------------------------------------------------

% retrieve all csv records from Stream
% csv_input_records(+Stream, +Records, +Options)
% Stream    the input stream
% Records   list of lists of fields
% Options   input options

csv_input_records(Stream, Records) :-
	csv_input_records(Stream, Records, []).

csv_input_records(Stream, Records, Options) :-

    csv_read_stream(Stream, Rows, Options),
    convlist(unwrap_row, Rows, Records).

% retrieve the next csv record from Stream
% csv_input_records(+Stream, +Records, +CompiledOptions)
% Stream            the input stream
% Records           list of fields
% CompiledOptions   compiled input options

csv_input_record(Stream, Record) :-
    csv_input_record(Stream, Record, []).

csv_input_record(Stream, Record, CompiledOptions) :-

    csv_read_row(Stream, Row, CompiledOptions),
    unwrap_row(Row, Record).

% build a record from the items in a row
unwrap_row(Row, Record) :-
    Row =.. [_|Record].

%-------------------------------------------------------------------------------
% csv file output

% write the given csv records to Stream
% csv_output_records(+Stream, +Records, +Options)
% Stream    the output stream
% Records   list of lists of fields
% Options   the output options

csv_output_records(Stream, Records) :-
	csv_output_records(Stream, Records, []).

csv_output_records(Stream, Records, Options) :-
    maplist(csv_output_record_(Stream, Options), Records).

csv_output_record_(Stream, Options, Record) :-
    csv_output_record(Stream, Record, Options).

% write the given csv record to Stream
% csv_output_record(+Stream, +Records, +Options)
% Stream    the output stream
% Record    list of fields
% Options   the output options

csv_output_record(Stream, Record) :-
    csv_output_record(Stream, Record, []).

csv_output_record(Stream, Record, Options) :-

    (memberchk(functor(Functor), Options) ; Functor = row),
    Row =.. [Functor|Record],
    csv_write_stream(Stream, Row, Options).

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
