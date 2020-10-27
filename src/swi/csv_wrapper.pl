
:- module(csv_wrapper,
    [
        csv_input_record/2,     % csv_input_record(+Stream, +Record)
        csv_input_record/3,     % csv_input_record(+Stream, +Record, +CompiledOptions)
        csv_input_records/2,    % csv_input_records(+Stream, +Records)
        csv_input_records/3,    % csv_input_records(+Stream, +Records, +Options)
        csv_is_header/1,        % csv_is_header(+Record)
        csv_output_record/2,    % csv_output_record(+Stream, +Record)
        csv_output_record/3,    % csv_output_record(+Stream, +Record, Options)
        csv_output_records/2,   % csv_output_records(+Stream, +Records)
        csv_output_records/3    % csv_output_records(+Stream, +Records, Options)
    ]).

/** <module> An Interface for CSV file operations

This is an attempt to define a standard interface for CSV file operations,
to be used in different Prolog environments. This module implements this
standard for the SWI-Prolog platform.

@author GT Nunes
@version 1.1.1
@copyright (c) 2020 GT Nunes
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

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

%-------------------------------------------------------------------------------------

%! csv_input_record(+Stream:ref, +Records:list) is det.
%
%  Retrieve the next CSV record from Stream.
%
%  @param Stream The input stream
%  @param Record List of fields

csv_input_record(Stream, Record) :-
    csv_input_record(Stream, Record, []).

%! csv_input_record(+Stream:ref, +Records:list, +CompiledOptions:list) is det.
%
%  Retrieve the next CSV record from Stream.
%
%  @param Stream          The input stream
%  @param Record          List of fields
%  @param CompiledOptions Compiled input options

csv_input_record(Stream, Record, CompiledOptions) :-

    csv_read_row(Stream, Row, CompiledOptions),
    unwrap_row(Row, Record).

%-------------------------------------------------------------------------------------

%! csv_input_records(+Stream:ref, +Records:list) is det.
%
%  Retrieve all CSV records from Stream.
%
%  @param Stream  The input stream
%  @param Records List of lists of fields

csv_input_records(Stream, Records) :-
	csv_input_records(Stream, Records, []).

%! csv_input_records(+Stream:ref, +Records:list, +Options:list) is det.
%
%  Retrieve all CSV records from Stream.
%
%  @param Stream  The input stream
%  @param Records List of lists of fields
%  @param Options Input options

csv_input_records(Stream, Records, Options) :-

    csv_read_stream(Stream, Rows, Options),
    convlist(unwrap_row, Rows, Records).

% build a record from the items in a row
unwrap_row(Row, Record) :-
    Row =.. [_|Record].

%-------------------------------------------------------------------------------------

%! csv_output_record(+Stream:ref, +Record:list) is det.
%
%  Write the given CSV record to Stream.
%
%  @param Stream  The output stream
%  @param Record  List of fields

csv_output_record(Stream, Record) :-
    csv_output_record(Stream, Record, []).

%! csv_output_record(+Stream:ref, +Record:list, +Options:list) is det.
%
%  Write the given CSV record to Stream.
%
%  @param Stream  The output stream
%  @param Record  List of fields
%  @param Options The output options

csv_output_record(Stream, Record, Options) :-

    (memberchk(functor(Functor), Options) ; Functor = row),
    Row =.. [Functor|Record],
    csv_write_stream(Stream, Row, Options).

%-------------------------------------------------------------------------------------

%! csv_output_records(+Stream:ref, +Records:list) is det.
%
%  Write the given CSV records to Stream.
%
%  @param Stream  The output stream
%  @param Records List of lists of fields

csv_output_records(Stream, Records) :-
	csv_output_records(Stream, Records, []).
	
%! csv_output_records(+Stream:ref, +Records:list, +Options:list) is det.
%
%  Write the given CSV records to Stream.
%
%  @param Stream  The output stream
%  @param Records List of lists of fields
%  @param Options The output options

csv_output_records(Stream, Records, Options) :-
    maplist(csv_output_record_(Stream, Options), Records).

csv_output_record_(Stream, Options, Record) :-
    csv_output_record(Stream, Record, Options).

%-------------------------------------------------------------------------------------

%! csv_is_header(+Record:list) is semidet.
%
%  Assert whether all fields in Record may be column names.
%
%  @param Record List of fields

csv_is_header(Record) :-
    maplist(is_col_name, Record).

%! is_col_name(+Field:data) is semidet.
%
%  Assert whether Field may be a column name.
%
%  @param Field The column name candidate

is_col_name(Field) :-

    % fail points
    atom(Field),
    atom_length(Field, Len),
    Len =< 32.
