## Dynworks in Prolog

*Dynworks* bring to the Prolog world high-performance implementations of *dynamic multi-dimensional arrays* and *dynamic vectors*. The package has been fully tested under the SISCtus 4.6.0 and the SWI-Prolog 8.2.1 platforms, on both Windows 10 and Ubuntu 20.04 environments.

Please, find a comprehensive tutorial on using dynarrays in the folder *dynarray-tutorial*. Additionally, in the folder *dynvector-benchmark* we present a beginner's tutorial on using *dynvectors*, and demonstrate the enormous performance gains they provide when used as replacements for standard Prolog lists.

The implementation includes modules providing persistence for instances of both *dynarrays* and *dynvectors*, using the Berkeley DB library. The tutorial in the folder *dynarray_tutorial* illustrates persisting and restoring *dynarrays*.

To use the persistence features on Microsoft Windows, Berkeley DB for Windows version 6.2.38 must be installed. The installers *db-6.2.28_86.msi* (32 bits) and *db-6.2.28_64.msi* (64 bits) may be obtained directly from the Oracle Berkeley DB site. Most Linux distributions already carry the Berkeley DB library by default. Additionally, on Linux environments SWI-Prolog requires that the package *swi-prolog-bdb* be installed.

A pack is available for the SWI-Prolog platform, registered as an official contribution to SWI-Prolog at http://swi-prolog.osuosl.org/pack/list . As such, to install it simply execute

~~~
pack_install(dynworks).
~~~

The documentation for the pack is available online at https://www.swi-prolog.org/pack/list?p=dynworks .

Finally, an extensive documentation can be found in the *doc* folder, accessible by browser directly from *index.html*. It was produced with the  help of SWI-Prolog's *PlDoc Source Documentation System*, which extracts and organizes the documentation added to the source code.
