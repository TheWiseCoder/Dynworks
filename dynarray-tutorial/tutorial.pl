/*******************************************************************************
* FILENAME / MODULE : tutorial.pl / user
*
* DESCRIPTION :
*       This benchmark is also a beginner's tutorial on how to use dynvectors.
*       It will go through some significant features of the package, which
*       should be complemented by reading the documentation in the source code
*       files.
*
* PUBLIC PREDICATES :
*       tutorial_prepare
*       tutorial_display
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
* 2020-10-06  GT Nunes          Module creation
*
*******************************************************************************/

:- if(current_prolog_flag(dialect, sicstus)).   % SICStus ----------------------

:- use_module(library(between),
    [
        numlist/3
    ]).

:- use_module(library(csv),
    [
        read_record/2,
        read_records/2
    ]).

:- use_module(library(lists),
    [
        maplist/2,
        reverse/2
    ]).

:- elif(current_prolog_flag(dialect, swi)).     % SWI-Prolog -------------------

:- use_module(library(apply),
    [
        maplist/3
    ]).

:- use_module(library(csv),
    [
        csv_read_stream/3
    ]).

:- use_module(library(lists),
    [
        numlist/3
    ]).

:- endif.                                       % ------------------------------

:- use_module('../src/dynarray-core',
    [
        dynarray_create/2,
        dynarray_destroy/1,
        dynarray_label/3,
        dynarray_top/3,
        dynarray_value/3
    ]).

:- use_module('../src/dynarray-persistence',
    [
        dynarray_persist/2,
        dynarray_restore/2
    ]).

%-------------------------------------------------------------------------------

tutorial_prepare :-

    % identify the Prolog platform
    current_prolog_flag(dialect, Dialect),

    % create the dynarray
    dynarray_destroy(artists),
    dynarray_create(artists, [100,7]),

    % register the column names
    dynarray_label(artists, name, 0),
    dynarray_label(artists, years, 1),
    dynarray_label(artists, genre, 2),
    dynarray_label(artists, nationality, 3),
    dynarray_label(artists, bio, 4),
    dynarray_label(artists, wikipedia, 5),
    dynarray_label(artists, paintings, 6),

    % retrieve data from file
    open('artists.csv', 'read', File),
    input_data(Dialect, File, Records),
    close(File),

    % load data onto the dynarray, one row at a time
    maplist(load_row, Records),

    % persist the data to external storage (dataset is 'my_dataset')
    dynarray_persist(artists, my_dataset),

  % destroy the dynarray
    dynarray_destroy(artists).

% load row onto dynarray
load_row([Name,Years,Genre,Nationality,Bio,Wikipedia,Paintings]) :-
    
    % compute current row from top for dimension 1
    dynarray_top(artists, 1, Top),
    Row is Top + 1,

    % load the data

    dynarray_value(artists, [Row,name], Name),
    dynarray_value(artists, [Row,years], Years),
    dynarray_value(artists, [Row,genre], Genre),
    dynarray_value(artists, [Row,nationality], Nationality),
    dynarray_value(artists, [Row,bio], Bio),
    dynarray_value(artists, [Row,wikipedia], Wikipedia),
    dynarray_value(artists, [Row,paintings], Paintings).

%-------------------------------------------------------------------------------

input_data(Dialect, File, Lists) :-

    % read data
    read_stream(Dialect, File, Records),

    % skip first record (field names)
    [_|Tail] = Records,

    % process data
    input_data_(Dialect, Tail, [], Lists).

read_stream(sicstus, File, Records) :-
    read_records(File, Records).

read_stream(swi, File, Records) :-
    csv_read_stream(File, Records, []).

% (done)
input_data_(_Dialect, [], ListsProgress, ListsFinal) :-
    reverse(ListsProgress, ListsFinal).

% (iterate)
input_data_(sicstus, [Record|Records], ListsProgress, ListsFinal) :-

    [_,string(C1),string(C2),string(C3),
     string(C4),string(C5),string(C6),integer(_,C7)] = Record,
    atom_codes(A1, C1),
    atom_codes(A2, C2),
    atom_codes(A3, C3),
    atom_codes(A4, C4),
    atom_codes(A5, C5),
    atom_codes(A6, C6),
    atom_codes(A7, C7),
    input_data_(sicstus, Records,
                [[A1,A2,A3,A4,A5,A6,A7]|ListsProgress], ListsFinal).

% (iterate)
input_data_(swi, [Record|Records], ListsProgress, ListsFinal) :-

    row(_, A1, A2, A3, A4, A5, A6, A7) = Record,
    input_data_(swi, Records,
                [[A1,A2,A3,A4,A5,A6,A7]|ListsProgress], ListsFinal).

%-------------------------------------------------------------------------------

tutorial_display :-

    % restore the dynarray from external storage (dataset is 'my_dataset')
    dynarray_restore(artists, my_dataset),

    %retrieve the top row
    dynarray_top(artists, 1, Top),
    numlist(0, Top, Rows),

    % display the data, one row at a time
    maplist(display_row, Rows),

    % destroy the dynarray
    dynarray_destroy(artists).

% display row
display_row(Row) :-

    % retrieve the data
    dynarray_value(artists, [Row,name], Name),
    dynarray_value(artists, [Row,years], Years),
    dynarray_value(artists, [Row,genre], Genre),
    dynarray_value(artists, [Row,nationality], Nationality),
    dynarray_value(artists, [Row,bio], Bio),
    dynarray_value(artists, [Row,wikipedia], Wikipedia),
    dynarray_value(artists, [Row,paintings], Paintings),

    % display the data
    nl,
    format('       Name: ~a~n', Name),
    format('      Years: ~a~n', Years),
    format('      Genre: ~a~n', Genre),
    format('Nationality: ~a~n', Nationality),
    format('        Bio: ~a~n', Bio),
    format('  Wikipedia: ~a~n', Wikipedia),
    format('  Paintings: ~a~n', Paintings).
