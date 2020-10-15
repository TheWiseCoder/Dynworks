An implementation of a global, stack-independent, simple counter. It is intended for use with integer values. There are no limits on the number of counters that can be simultaneously active.

Counters are identified by simple atoms. These are the available predicates:

- `counter_create(+Key)` - create counter *Key*, initialized to 0 (zero)  
- `counter_create(+Key, +Value)` - create counter *Key*, initialized to *Value*  
- `counter_value(+Key, ?Value)` - retrieve/set value of counter *Key*, unifying with *Value*  
- `counter_destroy(+Key)` - destroy counter *Key*  
- `counter_add(+Key, +Add)` - add *Add* to counter *Key*  
- `counter_add(+Key, +Add, ?Value)` - add *Add* to counter *Key*, unifying the result with *Value*  
- `counter_dec(+Key)` - decrement counter *Key*  
- `counter_dec(+Key, ?Value)` - decrement counter *Key*, unifying the result with *Value*  
- `counter_inc(+Key)` - increment counter *Key*  
- `counter_inc(+Key, ?Value)` - increment counter *Key*, unifying the result with *Value*  

A straight-forward usage of counters is in `repeat/0` loops:  

`% create counter 'loop' with initial value 0`  
`counter_create(loop),`  
`% repeat loop Count times`  
`repeat,`  
        `% retrieve Value from counter`  
        `counter_value(loop, Value),`  
        `. . . (do something with Value)`  
        `% increment counter, fail until it reaches Count`  
        `counter_inc(loop, Count),`  
`!,`  
`% destroy the counter`  
`counter_destroy(loop),`  
`. . .`  

Additionally, you may peruse the documentation and code on the source code file.  
