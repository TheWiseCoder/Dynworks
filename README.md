**Dynworks** bring to the Prolog world high-performance implementations of *dynamic arrays* and *dynamic vectors*. The package has been fully tested under the SISCtus 4.6.0 and the SWI-Prolog 8.2.1, on both Windows 10 and Ubuntu 20.04 environments.

**1. DYNARRAYS**

* *Dynarrays* are powerful, flexible, high-performance, hash-based multi-dimensional arrays.
* *Dynarrays* have O(1) read/insert/update/delete times, and this holds true up to sizes in the order of millions of cells.
* *Dynarrays* are not immutable objects, or more specifically, they are not recreated upon modification.
* *Dynarrays* have no limitation on the number of dimensions, nor any restriction on dimension sizes, apart from the running platform's resource limitations.
* *Dynarrays* have a maximum number of cells, defined at creation time and kept constant thereafter.
* *Dynarrays* demand no storage space reservation previous to the actual cell-by-cell space allocation requests.
* *Dynarrays* are resource-minded; their cells are not required to have values assigned to, in any particular sequence or fashion.
* In order to avoid resource wastage, *dynarrays* should be explicitly destroyed, upon ceasing to be of any further use.

Please, find a comprehensive tutorial on using dynarrays in the folder *dynarray-tutorial*.

**2. DYNVECTORS**

* *Dynvectors* are powerful, flexible, extendable, high-performance, hash-based vectors.
* *Dynvectors* have O(1) read/insert/update/delete times, and this holds true up to sizes in the order of millions of cells.
* *Dynvectors* are not immutable objects, or more specifically, they are not recreated upon modification.
* *Dynvectors* have no limitation on the number of cells, apart from the running platform's resource limitations.
* *Dynvectors* do not have a maximum number of cells specified at creation time - a *dynvector* dynamically adjusts its upper bound as needed.
* *Dynvectors* demand no storage space reservation previous to the actual cell-by-cell space allocation requests.
* *Dynvectors* are resource-minded; their cells are not required to have values assigned to, in any particular sequence or fashion.
* In order to avoid resource wastage, *dynvectors* should be explicitly destroyed, upon ceasing to be of any further use.

In the folder *dynvector-benchmark* we present a beginner's tutorial on using *dynvectors*, and demonstrate the enormous performance gains they provide when used as replacements for standard Prolog lists.

**3. PERSISTENCE**

The implementation includes modules providing persistance for instances of both *dynarrays* and *dynvectors*, using the Berkeley DB library. The tutorial in the folder *dynarray-tutorial* illustrates persisting and restoring *dynarrays*.

**4. INCLUDED PROJECT**

This project includes as sub-module *Goldies*, a small collection of useful Prolog utilities. Please, visit https://github.com/TheWiseCoder/Goldies

**5. REQUIREMENTS**

The *Dynworks* library requires SICStus Prolog version 4.6.0 or newer, or SWI-Prolog version 8.2.1 or newer, running under Microsoft Windows 10 or Ubuntu 20.04. It might work with older versions of SICStus or SWI-Prolog, or on platforms other than the ones mentioned, but this has not been tested.

To use the persistence features on Microsoft Windows, Berkeley DB for Windows version 6.2.38 must be installed. The installers db-6.2.28-86.msi (32 bits) and db-6.2.28-64.msi (64 bits) may be obtained directly from the Oracle Berkeley DB site. Most Linux distributions already carry the Berkeley DB library by default. Additionally, on Linux environments SWI-Prolog requires that the package swi-prolog-bdb be installed.
