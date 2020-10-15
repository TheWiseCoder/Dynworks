/*******************************************************************************
* FILENAME / MODULE : simple_counter.pl / simple_counter
*
* DESCRIPTION :
*       This is an implementation of a global, stack-independent, simple
*       counter for the SWI-Prolog platform. It is intended for use with
*       integer values, only. However, for performance reasons this is not
*       enforced anywhere in the code. Consequences of non-compliance with
*       this rule are unpredictable.
*
* PUBLIC PREDICATES :
*       counter_add(+Key, +Add)
*       counter_add(+Key, +Add, ?Value)
*       counter_create(+Key)
*       counter_create(+Key, +Value)
*       counter_destroy(+Key)
*       counter_dec(+Key)
*       counter_dec(+Key, ?Value)
*       counter_inc(+Key)
*       counter_inc(+Key, ?Value)
*       counter_value(+Key, ?Value)
*
* NOTES :
*       None yet.
*
*       Copyright TheWiseCoder 2020.  All rights reserved.
*
* REVISION HISTORY :
*
* DATE        AUTHOR            REVISION
* ----------  ----------------  ------------------------------------------------
* 2020-09-26  GT Nunes          Module creation
*
*******************************************************************************/

:- module(simple_counter,
    [
        counter_add/2,
        counter_add/3,
        counter_create/1,
        counter_create/2,
        counter_destroy/1,
        counter_dec/1,
        counter_dec/2,
        counter_inc/1,
        counter_inc/2,
        counter_value/2
    ]).

%-------------------------------------------------------------------------------
% An implementation of a global, stack-independent, simple counter.

% create counter Key, with 0 as its initial value
% counter_create(+Key)
% Key       atom identifying the counter
counter_create(Key) :-
    set_flag(Key, 0).

% create counter Key, with Value as its initial value
% counter_create(+Key, +Value)
% Key       atom identifying the counter
% Value     initial value (integer)
counter_create(Key, Value) :-
    set_flag(Key, Value).

% destroy counter Key
% (once set, flags cannot be removed, so we set it to the null char value)
% counter_destroy(+Key)
% Key       atom identifying the counter
counter_destroy(Key) :-
    set_flag(Key, '\000\').

% add Add to the counter
% counter_add(+Key, +Add)
% Key       atom identifying the counter
% Add       value to add to the counter (integer)
counter_add(Key, Add) :-
    flag(Key, Old, Old + Add).

% add Add to the counter, and unify Value with its final value
% counter_add(+Key, +Add, ?Value)
% Key       atom identifying the counter
% Add       value to add to the counter (integer)
% Value     counter's final value
counter_add(Key, Add, Value) :-

    % Add must be added to counter Key before Value unification
    flag(Key, Old, Old + Add),
    Value is Old + Add.

% decrement the counter
% counter_dec(+Key)
% Key       atom identifying the counter
counter_dec(Key) :-
    flag(Key, Old, Old - 1).

% decrement the counter, and unify Value with its final value
% counter_dec(+Key, ?Value)
% Key       atom identifying the counter
% Value     counter's final value
counter_dec(Key, Value) :-
    counter_add(Key, -1, Value).

% increment the counter
% counter_inc(+Key)
% Key       atom identifying the counter
counter_inc(Key) :-
    flag(Key, Old, Old + 1).

% increment the counter, and unify Value with its final value
% counter_inc(+Key, ?Value)
% Key       atom identifying the counter
% Value     counter's final value
counter_inc(Key, Value) :-
    counter_add(Key, 1, Value).

% unify Value with the counter's current value
% counter_value(+Key, ?Value)
% Key       atom identifying the counter
% Value     the counter's value
counter_value(Key, Value) :-

    (var(Value) ->
        get_flag(Key, Value)
    ;
        set_flag(Key, Value)
    ).
