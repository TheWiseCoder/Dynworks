:- module(bdb_wrapper,
    [
        bdb_base/1,
        bdb_erase/1,
        bdb_erase/2,
        bdb_retrieve/3,
        bdb_store/3
    ]).

/** <module> Persistence using Berkeley DB

This module provides a simple, minimalistic approach to implementing persistence
on the SWI-Prolog platform, by means of the Berkeley DB utility package. The
target data is organized in *|DataSets|*. A *|DataSet|* is comprised of one
or more *|TagSets|*, and a *|TagSet|* is a named collection of Prolog data.

Berkeley DB is an open-source software library intended to provide a
high-performance embedded database for key/value data. The database storage
is organized as a directory tree, starting with a root path specified with
*|bdb_path/1|*.

Being an embedded database implies that the library provides access to files
containing one or more database tables. These tables are always binary,
mapping keys to values. The SWI-Prolog interface to Berkeley DB allows
for fast storage of arbitrary Prolog terms, including cycles and constraints.

On Windows, the package *|Berkeley DB for Windows version 6.2.38|* must have been
installed. The installers *|db-6.2.28_86.msi|* (32 bits) and *|db-6.2.28_64.msi|*
(64 bits) may be obtained directly at the Oracle Berkeley DB site.

Most Linux distributions already carry the Berkeley DB library by default.
Additionally, on Linux environments SWI-Prolog requires that the package
*|swi-prolog-bdb|* be installed.

The Linux *|db-util|* package is fully compatible with the database structure
created by SWI-Prolog through Berkeley DB. For the db-util manpages, please
refer to https://manpages.debian.org/jessie/db-util/index.html .

Please, refer to https://www.swi-prolog.org/pldoc/doc/_SWI_/library/bdb.pl
for additional instructions on how to use Berkeley DB.

@author GT Nunes
@version 1.2
@copyright (c) TheWiseCoder 2020-2021
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

:- dynamic  swi_bdb_base/1.
:- volatile swi_bdb_base/1.

%-------------------------------------------------------------------------------------

%! bdb_base(+BasePath:atom) is det.
%! bdb_base(-BasePath:atom) is semidet.
%
%  Unify BasePath with the base path for Berkeley DB's persistence files.
%
% BasePath Atom identifying the base path for Berkeley DB's persistence files

bdb_base(BasePath) :-

    (var(BasePath) ->
        swi_bdb_base(BasePath)
    ;
        % register the base path for Berkeley DB (make sure it is '/'-terminated)
        (retract(swi_bdb_base(_)) ; true),
        (sub_atom(BasePath, _, 1, 0, '/') ->
            BdbPath = BasePath
        ;
            atom_concat(BasePath, '/', BdbPath)
        ),
        assertz(swi_bdb_base(BdbPath))
    ),

    % make sure path exists
    (exists_directory(BasePath) ; make_directory(BasePath)).

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

%! bdb_retrieve(+TagSet:atom, +DataSet:atom, -Data:data) is det.
%
% Retrieve Data from external storage.
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

%! bdb_erase(+DataSet:atom) is det.
%
%  Remove all the data associated with DataSet from external storage.
%
%  @param DataSet Atom identifying the dataset storage location fragment

bdb_erase(DataSet) :-

    % obtain the base storage location
    storage_path('*', DataSet, DsPath),
    file_directory_name(DsPath, BaseDir),

    % delete storage directory, if necessary
    (\+ exists_directory(BaseDir) ; (delete_directory_and_contents(BaseDir))).

%! bdb_erase(+TagSet:atom, +DataSet:atom) is det.
%
%  Erase the data associated with the TagSet within DataSet from external storage.
%
%  @param TagSet  Atom identifying the dataset to erase
%  @param DataSet Atom identifying the dataset storage location fragment
bdb_erase(TagSet, DataSet) :-

    % obtain the storage filepath for this dataset 
    storage_path(TagSet, DataSet, DsPath),

    % delete it, if necessary
    (\+ exists_file(DsPath) ; (delete_file(DsPath))).

%-------------------------------------------------------------------------------------

%! storage_path(+TagSet:atom, +DataSet:atom, -DsPath:atom) is det.
%
%  Unify DsPath with the directory pointing to the DataSet / TagSet repository.
%
%  @param TagSet  Atom identifying the dataset
%  @param DataSet Atom identifying the dataset storage location fragment
%  @param DsPath  The BDB storage path

storage_path(TagSet, DataSet, DsPath) :-

    % obtain the registered base path
    (swi_bdb_base(BasePath) ; BasePath = ''),

    % build the base directory
    atom_concat(BasePath, DataSet, BaseDir),

    % build the storage path
    format_to_codes('~a/~a.dbd', [BaseDir,TagSet], Codes),
    atom_codes(DsPath, Codes).
