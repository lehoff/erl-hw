-module(sensor).
-export([start/2, stop/0, loop/2]).

start(Pin, PidController) ->
	FdPin = gpio:init(Pin, in),
	Pid = spawn(?MODULE, loop, [FdPin, PidController]),
	Pid.

stop() ->
	%gpio:release(FdPin),
	exit(stopped).

loop(FdPin, PidController) ->
	PinState = gpio:read(FdPin),
	case PinState of
		"1" -> PidController ! {state, on_line};
		"0" -> PidController ! {state, off_line}
	end,
	timer:sleep(2000),
	loop(FdPin, PidController).

