-module(pwm).
-export([value/1]).
-on_load(init/0).

init() ->
	ok = erlang:load_nif("./pwm_nif", 0).

value(_X) ->
	exit(nif_library_not_loaded).

