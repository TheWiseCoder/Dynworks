### 1. Berkeley DB Wrapper

The code in *bdb_wrapper.pl* provides a simple, minimalistic approach to implementing persistence for Prolog data in the SWI-Prolog platform, by means of the Berkeley DB utility package. The code has been fully tested with SWI-Prolog 8.2.1, under Windows 10 Enterpise and Ubuntu 20.04 operating systems. It might perform equally well in other releases, but this has not been tested.

On Microsoft Windows, Berkeley DB for Windows version 6.2.38 must be installed. The installers *db-6.2.28_86.msi* (32 bits) and *db-6.2.28_4.msi* (64 bits) may be obtained directly from the Oracle Berkeley DB site. Most Linux distributions already carry the Berkeley DB library by default. Additionally, on Linux environments SWI-Prolog requires that the package *swi-prolog-bdb* be installed.

SWI-Prolog generates a distinct database structure, which can be fully manipulated with packages like Linux's *db-util* (see manpages at https://manpages.debian.org/jessie/db-util/index.html). There are many freely-available tutorials on Berkeley DB, and thus there is no need to digress on this subject here.

The environment variable *SWI_BDB_DIR* must be set(regardless of the host OS, always use Linux path syntax). This indicates the starting path for the database files. These files are organized in *datasets* containing *tagsets*. Assuming that the environment variable has been set to *c:/Users/my_user/bdb/*, this is how the database files would be laid out:

~~~
c:/Users/my_user/bdb/data_set_1/tag_set_a.bdb
. . .
c:/Users/my_user/bdb/data_set_1/tag_set_n.bdb
~~~

### 2. CSV Wrapper

These modules are an attempt to define a standard interface for '.csv' file operations, to be used in different Prolog environments. The code has been fully tested with SWI-Prolog 8.2.1, under Windows 10 Enterpise and Ubuntu 20.04 operating systems. It might perform equally well in other releases, but this has not been tested.

The code is a wrapper for the built-in library primitives in the SWI-Prolog platform. The input/output fields are plain scalar values, such as atoms standing for strings, integers, or floats. No attempt is made here to describe, document or explain the *CSV* standard, as appropriate documentation for that is widely available. Please, refer to the source code documentation, and to https://www.swi-prolog.org/pldoc/man?section=csv .

### 3. A global, stack-independent, simple counter

An implementation of a global, stack-independent, simple counter. It is intended for use with integer values. There are no limits on the number of counters that can be simultaneously active. Please, refer to the source code documentation.