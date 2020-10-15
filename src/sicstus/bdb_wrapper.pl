/*******************************************************************************
* FILENAME / MODULE : bdb_wrapper.pl / bdb_wrapper
*
* DESCRIPTION :
*       This module provides a simple, minimalistic approach to implementing
*       persistence for Prolog data on the SICStus platform, by means of the
*       Berkeley DB utility package. Berkeley DB is an open-source software
*       library intended to provide a high-performance embedded database for
*       key/value data. Please, refer to the SICStus 4.6 User Manual (from
*       page 401) for detailed instructions on how to use Berkeley DB.
*
*       Being an embedded database implies that the library provides access
*       to files containing one or more database tables. These tables are
*       always binary, mapping keys to values. The SICStus Prolog interface
*       to Berkeley DB allows for fast storage of arbitrary Prolog terms,
*       including cycles and constraints.
*
*       On Windows, the package Berkeley DB for Windows version 6.2.38 must
*       have been installed. The installers db-6.2.28_86.msi (32 bits) and
*       db-6.2.28_64.msi (64 bits) may be obtained directly from the
*       Oracle Berkeley DB site.
*
*       Most Linux distributions already carry the Berkeley DB library
*       by default. The Linux db-util package is fully compatible with
*       the database structure created by SICStus Prolog through Berkeley DB.
*       For the db-util manpages, please refer to 
*       https://manpages.debian.org/jessie/db-util/index.html .
*
* PUBLIC PREDICATES :
*       bdb_base(+DataSet)
*       bdb_erase(+DataSet)
*       bdb_erase(+TagSet, +DataSet)
*       bdb_retrieve(+TagSet, +DataSet, -Data)
*       bdb_store(+TagSet, +DataSet, +Data)
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

:- module(bdb_wrapper,
    [
        bdb_base/1,
        bdb_erase/1,
        bdb_erase/2,
        bdb_retrieve/3,
        bdb_store/3
    ]).

:- use_module(library(bdb),
    [
        db_close/1,
        db_fetch/3,
        db_open/4,
        db_store/3
    ]).

:- use_module(library(codesio),
    [
        format_to_codes/3
    ]).

:- use_module(library(file_systems),
    [
        current_directory/2,
        delete_directory/2,
        directory_exists/1,
        make_directory/1
    ]).

:- use_module(library(system),
    [
        environ/2
    ]).

%-------------------------------------------------------------------------------

% make sure the given dataset's base path for Berkeley DB exists
% bdb_base(+DataSet)
% DataSet   atom identifying Berkeley DB's base path fragment
bdb_base(DataSet) :-

    % obtain the storage location for Berkeley DB
    storage_dir(DataSet, BaseDir),

    % create directory, if necessary
    (directory_exists(BaseDir) ; make_directory(BaseDir)).
    

% persist the given data to external storage
% bdb_store(+TagSet, +DataSet, +Data)
% TagSet    atom identifying the dataset to store
% DataSet   atom identifying the dataset storage location fragment
% Data      the data to store
bdb_store(TagSet, DataSet, Data) :-

    % obtain the base storage location for this dataset 
    storage_dir(DataSet, BaseDir),

    % create base directory, if necessary
    (directory_exists(BaseDir) ; make_directory(BaseDir)),

    % establish the new working directory
    current_directory(SaveDir, BaseDir),

    !,
    % fail point (create the database)
    catch(db_open(TagSet, update, [bdb_data(-,-)], DbRef),
          _, bdb_fail(SaveDir)),

    !,
    % fail point (store the data)
    catch(db_store(DbRef, bdb_data(TagSet,Data), _TermRef),
          _, bdb_fail(SaveDir)),

    !,
    % fail point (close the database)
    catch(db_close(DbRef), _, bdb_fail(SaveDir)),

    % restore the saved working directory
    current_directory(_CurrDir, SaveDir).

% retrieve the specified data from external storage
% bdb_retrieve(+TagSet, +DataSet, -Data)
% TagSet    atom identifying the dataset to retrieve
% DataSet   atom identifying the dataset storage location fragment
% Data      the retrieve data, on success
bdb_retrieve(TagSet, DataSet, Data) :-

    % obtain the base and specific storage locations for this dataset 
    storage_dir(DataSet, BaseDir),
    atom_concat(BaseDir, TagSet, TagDir),

    !,
    % fail point
    directory_exists(TagDir),

    % establish the new working directory
    current_directory(SaveDir, BaseDir),

    !,
    % fail point (database might not exist)
    catch(db_open(TagSet, read, _DbSpecs, DbRef), _, bdb_fail(SaveDir)),

    !,
    % fail point (read the data)
    catch(db_fetch(DbRef, bdb_data(TagSet, Data), _TermRef),
          _, bdb_fail(SaveDir)),

    !,
    % fail point (close the database)
    catch(db_close(DbRef), _, bdb_fail(SaveDir)),

    % restore the saved working directory
    current_directory(_CurrDir, SaveDir).

% erase the specified external storage location
% bdb_erase(+DataSet)
% DataSet   atom identifying the dataset storage location fragment
bdb_erase(DataSet) :-

    % obtain the base storage location
    storage_dir(DataSet, BaseDir),

    % delete storage directory
    ( \+ directory_exists(BaseDir)
    ; (delete_directory(BaseDir, [if_nonempty(delete)])) ).

% erase the specified dataset from external storage
% bdb_erase(+TagSet, +DataSet)
% TagSet    atom identifying the dataset to erase
% DataSet   atom identifying the dataset storage location fragment
bdb_erase(TagSet, DataSet) :-

    % obtain the base storage location for this dataset 
    storage_dir(DataSet, BaseDir),

    % the storage directory for this dataset has TagSet added
    atom_concat(BaseDir, TagSet, BdbDir),

    % delete storage directory for data set
    ( \+ directory_exists(BdbDir)
    ; catch(delete_directory(BdbDir, [if_nonempty(delete)]), _, fail) ).

%-------------------------------------------------------------------------------

% bdb_fail(+SaveDir)
% SaveDir   the saved working directory
bdb_fail(SaveDir) :-

    % restore the saved working directory
    current_directory(_CurrDir, SaveDir),

    % fail the Berkeley DB action
    !, fail.

%-------------------------------------------------------------------------------

% Unify BaseDir with the base directory for Berkeley DB persistence.

% storage_dir(+DataSet, -BaseDir)
% DataSet   atom identifying the dataset storage location fragment
% BaseDir   the BDB base storage directory: <SICSTUS_BDB_DIR>/sicstus/<DataSet>/
storage_dir(DataSet, BaseDir) :-

    % obtain the BDB base path from the environment
    environ('SICSTUS_BDB_DIR', BdbDir),

    % SANITY POINT: SICSTUS_BDB_DIR may or may not be '/'-terminated
    (sub_atom(BdbDir, _, 1, 0, '/') ->
        format_to_codes('~a~a/', [BdbDir,DataSet], Codes)
    ;
        format_to_codes('~a/~a/', [BdbDir,DataSet], Codes)
    ),
    atom_codes(BaseDir, Codes).
