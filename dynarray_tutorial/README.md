### 1. Dynarray Tutorial

This tutorial presents a very simple, but yet illustrative, use of *dynarrays*, a high-performance implementation of *dynamic arrays*. We will be using a publicly available dataset from https://www.kaggle.com/supratimhaldar/deepartist-identify-artist-from-art . We will show how to use *dynarrays* to hold table data in a natural way, and how to easily persist and restore data using the Berkeley DB library. The file with the data we need is *artists.csv*, a very small part of a bigger *archive.zip* (if you are interested in the complete collection of high-quality images it contains, just register at the *kaggle.com* site and download everything free of charge).

The dataset in organized in *records* and *fields*, which will be mapped to *rows* and *columns* in a 2-dimension *dynarray*. Each row holds data about one artist, with the following columns: *name*, *years*, *genre*, *nationality*, *bio*, *wikipedia*, *paintings* (we dropped the column *id*, as we prefer to use *name* to uniquely identify an artist). Throughout this tutorial we will refer to the Prolog source code file *tutorial.pl*, located in this folder, a must-read for a full understanding of what is being presented.

To create a *dynarray*, it is a good idea to make sure it does not exist yet, as the operation will fail if it already exists. If necessary (not really the situation here), use

~~~
(\+ is_dynarray(artists) ; dynarray_destroy(artists)),
dynarray_create(artists, [100,7]),
~~~

or simply

~~~
dynarray_destroy(artists),
dynarray_create(artists, [100,7]),
~~~
since `dynarray_destroy/1` will neither fail nor complain when there is nothing to destroy.

The parameter [100,7] specifies a 2-dimension *dynarray* with a maximum of 100 rows and 7 columns. Although the *artists.csv* file contains only 50 rows, it is a good practice to specify a greater number of rows, if expansion is foreseen. This incurs no storage or performance costs, as *dynarrays* appropriate storage dynamically as needed, and sizes impact performance only when storage is actually allocated up to near or beyond the running platform's limits (for a discussion of performance *vis-a-vis* resources used, see the accompanying *dynvector-tutorial* in this package).

*Dynarrays* allows for labels to be used as indices for column identification, a much more intuitive and safer way than using integers. The code fragment (lines 96-102) is

~~~
dynarray_label(artists, name, 0),
dynarray_label(artists, years, 1),
dynarray_label(artists, genre, 2),
dynarray_label(artists, nationality, 3),
dynarray_label(artists, bio, 4),
dynarray_label(artists, wikipedia, 5),
dynarray_label(artists, paintings, 6),
~~~

The predicate `tutorial_prepare/0` (lines 86-116) creates the *dynarray*, invokes `input_data/2` to read the data from the *artists.csv* file, invokes `maplist/2` with `load_row/1` to load the rows onto the *dynarray*, and then it persists the data. The predicate `tutorial_display/0` restores the *dynarray* to memory and invokes `maplist/2` with `display_row/1` to display the rows. As simple as that!

### 2. Run the Tutorial

Before you start, follow the instructions in the documentation accompanying the Berkeley DB wrapper *bdb_wrapper.pl*, to prepare your installation to persist/restore data. Make sure the Prolog engine can find the file *artists.csv* in this folder (`open/3` at line 105), or move it to a more convenient location.

To run the tutorial, load and compile *tutorial.pl* in this folder, and then execute

~~~
tutorial_prepare.
~~~

followed by

~~~
tutorial_display.
~~~

Alternatively, from the command line on this folder run

~~~
sicstus < tutorial.cmd
~~~

or

~~~
swipl < tutorial.cmd
~~~

### 3. A Note About the CSV Implementation

You may appreciate the fact that the first part of the task performed in this tutorial, namely the loading of data from a *CSV* file onto a *dynarray*, could have been accomplished with this much simpler code:

~~~
open('artists.csv', read, Stream),
dynarray_csv(artists, Stream),
~~~

The complete `tutorial_prepare/0` predicate would be just:

~~~
tutorial_prepare :-
    % open csv file
    open('artists.sv', read, Stream),
    % create the dynarray from the csv file
    dynarray_destroy(artists),
    dynarray_csv(artists, Stream),
    % persist the dynarray
    dynarray_persist(artists, my_dataset),
    dynarray_destroy(artists).
~~~

This is due to the fact that *dynarrays* have a built-in implementation for persisting to, and restoring from, *CSV* files. Using this facility, though, would defeat the purpose of the tutorial.
