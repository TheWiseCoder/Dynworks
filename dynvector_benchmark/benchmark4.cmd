set_prolog_flag(stack_limit, 2_147_483_648).
compile('benchmark.pl').
benchmark([1,2,3,4], 10000000, 20000000),
halt.