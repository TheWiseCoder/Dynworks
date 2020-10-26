:- module(bdb_wrapper,
    [
        bdb_base/1,         % bdb_base(+DataSet)
        bdb_erase/1,        % bdb_erase(+DataSet)
        bdb_erase/2,        % bdb_erase(+TagSet, +DataSet)
        bdb_retrieve/3,     % bdb_retrieve(+TagSet, +DataSet, -Data)
        bdb_store/3         % bdb_store(+TagSet, +DataSet, +Data)
    ]).

/** <module> Berkeley DB Wrapper

This module provides a simple, minimalistic approach to implementing
persistence for Prolog data on the SWI-Prolog platform, by means of the
Berkeley DB utility package. Berkeley DB is an open-source software
library intended to provide a high-performance embedded database for
key/value data. Please, refer to 
https://www.swi-prolog.org/pldoc/doc/_SWI_/library/bdb.pl
for additional instructions on how to use Berkeley DB.

Being an embedded database implies that the library provides access
to files containing one or more database tables. These tables are
always binary, mapping keys to values. The SWI-Prolog interface
to Berkeley DB allows for fast storage of arbitrary Prolog terms,
including cycles and constraints.

On Windows, the package _|Berkeley DB for Windows version 6.2.38|_ must
have been installed. The installers *|db-6.2.28_86.msi|* (32 bits) and
*|db-6.2.28_64.msi|* (64 bits) may be obtained directly at the
Oracle Berkeley DB site.

Most Linux distributions already carry the Berkeley DB library
by default. Additionally, on Linux environments SWI-Prolog requires
that the package *|swi-prolog-bdb|* be installed.

The Linux *|db-util|* package is fully compatible with the database structure
created by SWI-Prolog through Berkeley DB. For the *|db-util|* manpages, please
refer to https://manpages.debian.org/jessie/db-util/index.html .

@author GT Nunes
@version 1.1.1
@copyright (c) 2020 GT Nunes
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

:- use_module(library(codesio),
    [
        format_to_codes/3
    ]).

:- use_module(library(filesex),
    [
        delete_directory_and_contents/1
    ]).

%-------------------------------------------------------------------------------------

%! bdb_base(+DataSet:atom) is det.
%
%  Make sure that the given dataset's base path for Berkeley DB exists.
%
%  @param DataSet Atom identifying Berkeley DB's dataset

bdb_base(DataSet) :-

    % obtain the storage location for Berkeley DB
    storage_dir(DataSet, BaseDir),

    % create directory, if necessary
    (exists_directory(BaseDir) ; make_directory(BaseDir)).

%-------------------------------------------------------------------------------------

%! bdb_store(+TagSet:atom, +DataSet:atom, +Data:data) is det.
%
%  Persist Data to external storage.
%
%  @param TagSet  Atom identifying the tagset to store
%  @param DataSet Atom identifying the dataset storage location fragment
%  @param Data    The data to store

bdb_store(TagSet, DataSet, Data) :-

    % obtain the storage filepath for this dataset 
    storage_path(TagSet, DataSet, DsPath),

    % create base directory, if necessary
    file_directory_name(DsPath, BaseDir),
    (exists_directory(BaseDir) ; make_directory(BaseDir)),

    !,
    % fail point (create the database)
    catch(bdb_open(DsPath, update, DbRef, []), _, fail),

    !,
    % fail point (store the data)
    catch(bdb_put(DbRef, data, Data), _, fail),

    !,
    % fail point (close the database)
    catch(bdb_close(DbRef), _, fail).

%-------------------------------------------------------------------------------------

%! bdb_retrieve(+TagSet:atom, +DataSet:atom, -Data:data) is det.
%
%  Retrieve Data from external storage.
%
%  @param TagSet  Atom identifying the tagset to store
%  @param DataSet Atom identifying the dataset storage location fragment
%  @param Data    The data to retrieve

bdb_retrieve(TagSet, DataSet, Data) :-

    % obtain the storage filepath for this dataset 
    storage_path(TagSet, DataSet, DsPath),

    !,
    % fail point (open the database)
    catch(bdb_open(DsPath, read, DbRef, []), _, fail),

    !,
    % fail point (retrieve the data)
    catch(bdb_get(DbRef, data, Data), _, fail),

    !,
    % fail point (close the database)
    catch(bdb_close(DbRef), _, fail).

%-------------------------------------------------------------------------------------

%! bdb_erase(+DataSet:atom) is det.
%
%  Erase the specified external storage location.
%
%  @param DataSet Atom identifying the dataset storage path fragment

bdb_erase(DataSet) :-

    % obtain the base storage location
    storage_path('*', DataSet, DsPath),
    file_directory_name(DsPath, BaseDir),

    % delete storage directory, if necessary
    (\+ exists_directory(BaseDir) ; (delete_directory_and_contents(BaseDir))).

%! bdb_erase(+TagSet:atom, +DataSet:atom) is det.
%
%  Erase the specified dataset from external storage.
%
%  @param TagSet  Atom identifying the dataset to erase
%  @param DataSet Atom identifying the dataset storage location fragment

bdb_erase(TagSet, DataSet) :-

    % obtain the storage filepath for this dataset 
    storage_path(TagSet, DataSet, DsPath),

    % delete it, if necessary
    (\+ exists_file(DsPath) ; (delete_file(DsPath))).

%-------------------------------------------------------------------------------------

%! storage_dir(+DataSet:atom, -BaseDir:path) is det.
%
%   Unify BaseDir with the base directory for Berkeley DB persistence.
%
%  @param DataSet Atom identifying the dataset storage location fragment
%  @param BaseDir The BDB base storage directory: `<SWI_BDB_DIR>/<DataSet>/`

storage_dir(DataSet, BaseDir) :-

    % obtain the BDB base path from the environment
    getenv('SWI_BDB_DIR', BdbDir),

    % SANITY POINT: SWI_BDB_DIR may or may not be '/'-terminated
    (sub_atom(BdbDir, _, 1, 0, '/') ->
        format_to_codes('~a~a/', [BdbDir,DataSet], Codes)
    ;
        format_to_codes('~a/~a/', [BdbDir,DataSet], Codes)
    ),
    atom_codes(BaseDir, Codes).

%-------------------------------------------------------------------------------------

% storage_path(+TagSet:atom, +DataSet:atom, -DsPath:path) is det.
%
%  Build the BDB storage filepath.
%
%  @param TagSet  Atom identifying the dataset
%  @param DataSet Atom identifying the dataset storage location fragment
%  @param DsPath  The BDB storage path: <SWI_BDB_DIR>/<DataSet>/<TagSet>.bdb

storage_path(TagSet, DataSet, DsPath) :-

    storage_dir(DataSet, BaseDir),
    format_to_codes('~a~a.dbd', [BaseDir,TagSet], Codes),
    atom_codes(DsPath, Codes).
