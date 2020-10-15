The code in *bdb_wrapper.pl* provides a simple, minimalistic approach to implementing persistence for Prolog data in the SICStus platform, by means of the Berkeley DB utility package. The code has been fully tested with SICStus 4.6.0, under Windows 10 Enterpise and Ubuntu 20.04 operating systems. It might perform equally well in other releases, but this has not been tested.

On Microsoft Windows, Berkeley DB for Windows version 6.2.38 must be installed. The installers *db-6.2.28_86.msi* (32 bits) and *db-6.2.28_4.msi* (64 bits) may be obtained directly from the Oracle Berkeley DB site. Most Linux distributions already carry the Berkeley DB library by default. 

SICStus generates a distinct database structure, which can be fully manipulated with packages like Linux's *db-util* (see manpages at https://manpages.debian.org/jessie/db-util/index.html). There are many freely-available tutorials on Berkeley DB, and thus there is no need to digress on this subject here.

The environment variable *SICSTUS_BDB_DIR* must be set(regardless of the host OS, always use Linux path syntax). This indicates the starting path for the database files. These files are organized in *datasets* containing *tagsets*. Assuming that the environment variable has been set to *c:/Users/my_user/bdb/*, this is how the database files would be laid out:

`c:/Users/my_user/bdb/data_set_1/tag_set_a/admin.db`  
`c:/Users/my_user/bdb/data_set_1/tag_set_a/index.db`  
`c:/Users/my_user/bdb/data_set_1/tag_set_a/terms.db`  
`. . .`  
`c:/Users/my_user/bdb/data_set_N/tag_set_n/admin.db`  
`c:/Users/my_user/bdb/data_set_N/tag_set_n/index.db`  
`c:/Users/my_user/bdb/data_set_N/tag_set_n/terms.db`  

Using the available predicates is straigh forward:

- `bdb_base(+DataSet)` - make sure the given dataset's base path for Berkeley DB exists  
- `bdb_erase(+DataSet)` - erase the complete dataset from storage  
- `bdb_erase(+TagSet, +DataSet)` - erase the tagset within the given dataset from storage  
- `bdb_retrieve(+TagSet, +DataSet, -Data)` - retrieve the given tagset from storage  
- `bdb_store(+TagSet, +DataSet, +Data)` - persist the given tagset  
