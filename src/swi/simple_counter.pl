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

/** <module> A global, stack-independent, simple counter

This is an implementation of a global, stack-independent, simple counter for
the SWI-Prolog platform. It is intended for use with integer values, only.
However, for performance reasons this is not enforced anywhere in the code.
Consequences of non-compliance with this rule are unpredictable.

@author GT Nunes
@version 1.2
@copyright (c) TheWiseCoder 2020-2021
@license BSD-3-Clause License
*/

%-------------------------------------------------------------------------------------

%! counter_create(+Key:atom) is det.
%
%  Create counter Key, with 0 as its initial value.
%
%  @param Key Atom identifying the counter

counter_create(Key) :-
    set_flag(Key, 0).

%! counter_create(+Key:atom, +Value:int) is det.
%
%  Create counter Key, with Value as its initial value.
%
%  @param Key   Atom identifying the counter
%  @param Value Initial value (integer)

counter_create(Key, Value) :-
    set_flag(Key, Value).

%! counter_destroy(+Key:atom) is det.
%
%  Destroy counter Key.
%  (Once set, flags cannot be removed, so we set it to the null char value).
%
%  @param Key Atom identifying the counter

counter_destroy(Key) :-
    set_flag(Key, '\\000\\').

%-------------------------------------------------------------------------------------

%! counter_add(+Key:atom, +Add:int) is det.
%
%  Sum Add to counter Key.
%
%  @param Key Atom identifying the counter
%  @param Add Value to add to the counter (integer)

counter_add(Key, Add) :-
    flag(Key, Old, Old + Add).

%! counter_add(+Key:atom, +Add:int, -Value:int) is det.
%! counter_add(+Key:atom, +Add:int, +Value:int) is semidet.
%
%  Sum Add to counter Key, and unify Value with its final value.
%
%  @param Key   Atom identifying the counter
%  @param Add   Value to add to the counter (integer)
%  @param Value Counter's final value

counter_add(Key, Add, Value) :-

    % Add must be added to counter Key before Value unification
    flag(Key, Old, Old + Add),
    Value is Old + Add.

%-------------------------------------------------------------------------------------

%! counter_dec(+Key:atom) is det.
%
%  Decrement counter Key.
%
%  @param Key Atom identifying the counter
counter_dec(Key) :-
    flag(Key, Old, Old - 1).

%! counter_dec(+Key:atom, -Value:int) is det.
%! counter_dec(+Key:atom, +Value:int) is semidet.
%
%  Decrement counter Key, and unify Value with its final value.
%
%  @param Key   Atom identifying the counter
%  @param Value Counter's final value

counter_dec(Key, Value) :-
    counter_add(Key, -1, Value).

%-------------------------------------------------------------------------------------

%! counter_inc(+Key:atom) is det.
%
%  Increment counter Key.
%
%  @param Key Atom identifying the counter

counter_inc(Key) :-
    flag(Key, Old, Old + 1).

%! counter_inc(+Key:atom, -Value:int) is det.
%! counter_inc(+Key:atom, +Value:int) is semidet.
%
%  Increment counter Key, and unify Value with its final value.
%
%  @param Key   Atom identifying the counter
%  @param Value Counter's final value

counter_inc(Key, Value) :-
    counter_add(Key, 1, Value).

%-------------------------------------------------------------------------------------

%! counter_value(+Key:atom, ?Value:int) is det.
%
%  Unify Value with the current value of counter Key.
%
%  @param Key   Atom identifying the counter
%  @param Value The counter's value

counter_value(Key, Value) :-

    (var(Value) ->
        get_flag(Key, Value)
    ;
        set_flag(Key, Value)
    ).
