These modules are an attempt to define a standard interface for '.csv' file operations, to be used in different Prolog environments. The code has been fully tested with SWI-Prolog 8.2.1, under Windows 10 Enterpise and Ubuntu 20.04 operating systems. It might perform equally well in other releases, but this has not been tested.

The code is a wrapper for the built-in library primitives in the SWI-Prolog platform. The input/output fields are plain scalar values, such as atoms standing for strings, integers, or floats. No attempt is made here to describe, document or explain the *CSV* standard, as appropriate documentation for that is widely available. Please, refer to the source code, and to https://www.swi-prolog.org/pldoc/man?section=csv .

Using the available predicates is straight forward:

- `csv_input_record(+Stream, +Record)` - retrieve the next *CSV* record from Stream  
- `csv_input_record(+Stream, +Record, +CompiledOptions)` - retrieve the next *CSV* record from Stream, according to *CompiledOptions*  
- `csv_input_records(+Stream, +Records)` - retrieve all *CSV* records from Stream  
- `csv_input_records(+Stream, +Records, _Options)` - retrieve all *CSV* records from Stream, according to *Options*  
- `csv_output_record(+Stream, +Record)` - write the given *CSV* record to Stream  
- `csv_output_record(+Stream, +Record, +Options)` - write the given *CSV* record to Stream, according to *Options*  
- `csv_output_records(+Stream, +Records)` - write the given *CSV* records to Stream  
- `csv_output_records(+Stream, +Records, +Options)` - write the given *CSV* records to Stream, according to *Options*  
- `csv_is_header(+Record)` - assert whether all fields in *Record* may be column names  
