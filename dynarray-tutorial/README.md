**1) DYNARRAY TUTORIAL**

This tutorial presents a very simple, but yet illustrative, use of *dynarrays*, a high-performance implementation of *dynamic arrays*. To leverage this work, some utilities from another project, **Prolog Goldies**, are used, as specified in the appropriate `use_module` clauses.

We will be using a publicly available dataset from https://www.kaggle.com/supratimhaldar/deepartist-identify-artist-from-art . We will show how to use *dynarrays* to hold table data in a natural way, and how to easily persist and restore data using the Berkeley DB library. The file with the data we need is *artists.csv*, a very small part of a bigger *archive.zip* (if you are interested in the complete collection of high-quality images it contains, just register at the *kaggle.com* site and download everything free of charge).

The dataset in organized in *records* and *fields*, which will be mapped to *rows* and *columns* in a 2-dimension *dynarray*. Each rows holds data about one artist, with the following columns: *name*, *years*, *genre*, *nationality*, *bio*, *wikipedia*, *paintings* (we dropped the column *id*, as we prefer to use *name* to uniquely identify an artist). Throughout this tutorial we will refer to the Prolog source code file *tutorial.pl*, located in this folder, a must-read for a full understanding of what is being presented.

To create a *dynarray*, it is a good idea to make sure it does not exist yet, as the operation will fail if it already exists. If necessary (not really the situation here), use  
`(\+ is_dynarray(artists) ; dynarray_destroy(artists)),`  
`dynarray_create(artists, [100,7]),`  
or simply
`dynarray_destroy(artists),`  
`dynarray_create(artists, [100,7]),`  
since `dynarray_destroy/1` fails silently when there is nothing to destroy.

The parameter *[100,7]* specifies a 2-dimension *dynarray* with a maximum of 100 rows and 7 columns. Although the *artists.csv* file contains only 50 rows, it is a good practice to specify a greater number of rows, if expansion is foreseen. This incurs no storage or performance costs, as *dynarrays* take storage dynamically as needed, and sizes impact performance only when storage is actually allocated up to near or beyond the running platform's limits (for a discussion of performance *vis-à-vis* resources used, see the accompanying *dynvector tutorial* in this package).

*Dynarrays* allows for labels to be used as indices for column identification, a much more intuitive and safer way than using integers. The code fragment (lines 140-146) is  
`dynarray_label(artists, name, 0),`  
`dynarray_label(artists, years, 1),`  
`dynarray_label(artists, genre, 2),`  
`dynarray_label(artists, nationality, 3),`  
`dynarray_label(artists, bio, 4),`  
`dynarray_label(artists, wikipedia, 5),`  
`dynarray_label(artists, paintings, 6),`  

The predicate `tutorial_prepare/0` creates the *dynarray*, invokes `input_data/2` to read the data from the *artists.csv* file, invokes `maplist/2` with `load_row/1` to load the rows onto the *dynarray*, and then it persists the data. The predicate `tutorial_display/0` restores the *dynarray* to memory and invokes `maplist/2` with`display_row/1` to display the rows. As simple as that!


**2) RUN THE TUTORIAL**

Before you start, follow the instructions in Goldies/... to prepare your installation to persist/restore data using Prolog and Berkeley DB. Make sure the Prolog engine can find the file *artists.csv* in this folder (line 149), or move it to a more convenient location.

To run the tutorial, load and compile *tutorial.pl* in this folder, and then execute  
`tutorial_prepare.`  
followed by  
`tutorial_display.`  

Alternatively, from the command line on this folder run  
`sicstus < tutorial.cmd`  
or  
`swipl < tutorial.cmd`  
 

**3) SUMMARY OF PUBLIC PREDICATES**  


**3.1) dynarray-core.pl**  

- `dynarray_create(+Id, +DimRanges)` - Create dynarray *Id*, with dimensions/ranges *DimRanges*.  

- `dynarray_destroy(+Id)` - Destroy dynarray *Id*, releasing all the storage space taken.  

- `dynarray_value(+Id, +Indices, ?Value)` - Unify *Value* with the value stored at *Indices*.  

- `dynarray_delete(+Id, +Indices)` - Erase the cell at *Indices*, releasing the storage space taken.  

- `dynarray_find(+Id, ?Value, ?Indices)` - Unify *Value* or *Indices* with an occurrence of *Value* or *Indices*, respectively.  

- `dynarray_fill(+Id, +Value)` - Load all *Id*'s cells with *Value*.  

- `dynarray_list(+Id, ?List)` - Unify *List* with all the cells in the *dynarray*.  

- `dynarray_label(+Id, +Label, ?Value)` - Unify *Label* with *Value*.  

- `dynarray_dims(+Id, -DimCount)` - Retrieve the number of dimensions.  

- `dynarray_cells(+Id, -CellCount)` - Retrieve the total number of cells.  

- `dynarray_cells(+Id, +Dim, -CellCount)` - Retrieve the total number of cells in dimension *Dim*.  

- `dynarray_top(+Id, +Dim, -Top)` - Unify *Top* with the highest inserted index value on the given dimension, even if it has subsequently been deleted.  

- `dynarray_position_delete(+Id, +Position)` - Erase the cell at *Position*, releasing the storage space taken.  

- `dynarray_position_find(+Id, ?Value, ?Position)` - Unify *Value* or *Position* with an occurrence of *Value* or *Position*, respectively.  

- `dynarray_position_indices(+Id, ?Position, ?Indices)` - Unify *Position* with the corresponding *Indices* (*Position* is the cell's 0-based linear position).  

- `dynarray_position_value(+Id, +Position, ?Value)` - Unify *Value* with the value of cell at *Position*.  

- `is_dynarray(+Id)` - Succeed if *Id* identifies a *dynarray*.  


**3.2) dynarray-persist.pl**  

- `dynarray_clone(+IdSource, +IdTarget)` - Create *IdTarget* as a clone of *IdSource*.  

- `dynarray_erase(+DynId, +DataSet)` - Erase *Id*'s data, as part of *DataSet*, from external storage.  

- `dynarray_persist(+DynId, +DataSet)` - Persist *Id*'s data, as part of *DataSet*, in external storage.  

- `dynarray_restore(+DynId, +DataSet)` - Restore *Id*'s data, as part of *DataSet*, from external storage.  

- `dynarray_serialize(+Id, ?Serialized)` - Unify *Serialized* with *Id*'s serialized data, for backup/restore purposes.  
